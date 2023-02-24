library(dagitty)
library(tidyverse)
library(xgboost)

grafo <- dagitty::dagitty("dag {
    {A B} -> { X Y }; X -> {C Y}; C -> Y }")

####################
# Simular os dados #
####################

n <- 10^5
sd = 0.1
A <- rnorm(n, 0, sd)
B <- rnorm(n, 0, sd)
X <- as.numeric((A + B) > 0)
C <- rnorm(n, X, sd)
Y <- rnorm(n, A + B + C + X, sd)
data <- dplyr::tibble(A, B, C, X, Y)
  
##########################################
# Fórmula do ajuste por regressão linear #
##########################################

# Sejam Z variáveis que satisfazem o critério backdoor para
# estimar o efeito causal de causa em efeito em grafo.
# Retorna uma fórmula do tipo Y ~ X + Z_1 + ... + Z_d
fm_ajuste <- function(grafo, causa, efeito)
{
  var_backdoor <- dagitty::adjustmentSets(grafo, causa, efeito)[[1]]
  regressores = c(causa, var_backdoor)
  fm = paste(regressores, collapse = "+")
  fm = paste(c(efeito, fm), collapse = "~")
  as.formula(fm)
}

# Estima E[Efeito|do(causa = x)] pela 
# formula do ajuste usando mu_chapeu como regressao 
est_do_x_lm <- function(data, mu_chapeu, causa, x)
{
  data %>% 
    dplyr::mutate({{causa}} := x) %>%  
    predict(mu_chapeu, newdata = .) %>% 
    mean()
}

# Estimação do ACE com regressão linear simples
fm <- fm_ajuste(grafo, "X", "Y")
mu_chapeu <- lm(fm, data = data)
ace_est = est_do_x_lm(data, mu_chapeu, "X", 1) - 
  est_do_x_lm(data, mu_chapeu, "X", 0)
round(ace_est)

#################################
# Fórmula do ajuste por XGBoost #
#################################

mu_chapeu <- xgboost(
  data = data %>% dplyr::select(A, B, X) %>% as.matrix(),
  label = data %>% dplyr::select(Y) %>% as.matrix(),
  nrounds = 100,
  objective = "reg:squarederror",
  early_stopping_rounds = 3,
  max_depth = 2,
  eta = .25,
  verbose = FALSE
)

est_do_x_xgb <- function(data, mu_chapeu, causa, x)
{
  data %>% 
    dplyr::mutate({{causa}} := x) %>%  
    dplyr::select(A, B, X) %>% 
    as.matrix() %>% 
    predict(mu_chapeu, newdata = .) %>% 
    mean()
}

ace_est_xgb = est_do_x_xgb(data, mu_chapeu, "X", 1) - 
  est_do_x_xgb(data, mu_chapeu, "X", 0)
round(ace_est_xgb, 2)

###############################
# IPW por regressão logística #
###############################

# Sejam Z variáveis que satisfazem o critério backdoor para
# estimar o efeito causal de causa em efeito em grafo.
# Retorna uma fórmula do tipo X ~ Z_1 + ... + Z_d
fm_ipw <- function(grafo, causa, efeito)
{
  var_backdoor <- dagitty::adjustmentSets(grafo, causa, efeito)[[1]]
  fm = paste(var_backdoor, collapse = "+")
  fm = paste(c(causa, fm), collapse = "~")
  as.formula(fm)
}

# Estimação do ACE por IPW onde
# Supomos X binário e
# f_1 é o vetor P(X_i=1|Z_i)
ACE_ipw <- function(data, causa, efeito, f_1)
{
  data %>% 
    mutate(f_1 = f_1,
           est_1 = {{efeito}}*({{causa}}==1)/f_1,
           est_0 = {{efeito}}*({{causa}}==0)/(1-f_1)
    ) %>% 
    summarise(do_1 = mean(est_1),
              do_0 = mean(est_0)) %>% 
    mutate(ACE = do_1 - do_0) %>% 
    dplyr::select(ACE)
}

fm <- fm_ipw(grafo, "X", "Y")
f_chapeu <- glm(fm, family = "binomial", data = data)
f_1_lm <- predict(f_chapeu, type = "response")
ace_ipw_lm <- data %>% ACE_ipw(X, Y, f_1_lm) %>% as.numeric()
ace_ipw_lm %>% round(2)

###################
# IPW por XGBoost #
###################

var_backdoor <- dagitty::adjustmentSets(grafo, "X", "Y")[[1]]
f_chapeu <- xgboost(
  data = data %>% 
    dplyr::select(all_of(var_backdoor)) %>% 
    as.matrix(),
  label = data %>% 
    dplyr::select(X) %>% 
    as.matrix(),
  nrounds = 100,
  objective = "binary:logistic",
  early_stopping_rounds = 3,
  max_depth = 2,
  eta = .25,
  verbose = FALSE
)

covs <- data %>% dplyr::select(all_of(var_backdoor)) %>% as.matrix()
f_1 <- predict(f_chapeu, newdata = covs)
data %>% ACE_ipw(X, Y, f_1) %>% as.numeric() %>% round(2)

####################################
# Estimador duplamente robusto por #
# regressão linear e logística     #
####################################

mu_1_lm <- data %>% 
  dplyr::mutate(X = 1) %>%
  predict(mu_chapeu_lm, newdata = .)
mu_0_lm <- data %>% 
  dplyr::mutate(X = 0) %>%
  predict(mu_chapeu_lm, newdata = .)
corr <- data %>% 
  mutate(mu_1 = mu_1_lm, 
         mu_0 = mu_0_lm,
         f_1 = f_1_lm,
         corr_1 = (X == 1)*mu_1/f_1,
         corr_0 = (X == 0)*mu_0/(1-f_1)) %>% 
  summarise(corr_1 = mean(corr_1),
            corr_0 = mean(corr_0)) %>% 
  mutate(corr = corr_1 - corr_0) %>% 
  dplyr::select(corr) %>% 
  as.numeric()

ace_ajuste_lm + ace_ipw_lm - corr
