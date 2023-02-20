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
