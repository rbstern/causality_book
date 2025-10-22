library(tidyverse)
library(xgboost)

n <- 1000
Z <- rnorm(n)
X <- Z >= 0
#Y <- rnorm(n, 100*X + 20*Z)
Y <- rnorm(n, 50*(X+1)*(Z+1))
data <- tibble(X=X, Y=Y, Z=Z)

data %>% 
  ggplot(aes(x=Z, y=Y)) +
  geom_point()

regs = data %>%
  mutate(Z1 = (Z >= 0)) %>%
  group_by(Z1) %>%
  summarise(
    intercepto = lm(Y ~ Z)$coefficients[1],
    coef_angular = lm(Y ~ Z)$coefficients[2]
  )
regs

est_cace = 1*regs[2, 2] + 0*regs[2, 3] -
  1*regs[1, 2] + 0*regs[1, 3]
round(as.numeric(est_cace), 2)


n <- 1000
Z <- rnorm(n)
X <- Z >= 0
#Y <- rnorm(n, 100*X + 20*Z)
Y <- rnorm(n, X*exp(Z) + (1-X)*sin(Z), sd = 0.1) 
data <- tibble(X=X, Y=Y, Z=Z)


reg_1 = xgboost(
  data = data %>% filter(Z >= 0) %>% dplyr::select(Z) %>% as.matrix(),
  label = data %>% filter(Z >= 0) %>% dplyr::select(Y) %>% as.matrix(),
  nrounds = 100,
  objective = "reg:squarederror",
  early_stopping_rounds = 3,
  max_depth = 2,
  eta = .25,
  verbose = FALSE
)

reg_0 = xgboost(
  data = data %>% filter(Z < 0) %>% dplyr::select(Z) %>% as.matrix(),
  label = data %>% filter(Z < 0) %>% dplyr::select(Y) %>% as.matrix(),
  nrounds = 100,
  objective = "reg:squarederror",
  early_stopping_rounds = 3,
  max_depth = 2,
  eta = .25,
  verbose = FALSE
)

new_data = matrix(0)
predict(reg_1, new_data) - predict(reg_0, new_data)
