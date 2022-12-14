library(randomForest)
library(tidyverse)

# Dados 1
n = 10^4
U <- rnorm(n)
X <- U > 0
M <- X + rnorm(n)
Y <- U*M + rnorm(n)
data = tibble(U = U, X = X, M = M, Y = Y)

train = 1:(0.8*10^4)
teste = (0.8*10^4+1):n 

data_train = data[train,]
data_teste = data[teste,]

# E[Y|do(X=x)] = E[E[Y|X=x, U]]
# ATE = E[E[Y|X=1, U]] - E[E[Y|X=0, U]]

# Estimando E[Y|X=1, U]
reg_1 = data_train %>% 
  filter(X == 1) %>% 
  randomForest(Y~U, data=.)

# Estimando E[Y|X=0, U]
reg_0 = data_train %>% 
  filter(X == 0) %>% 
  randomForest(Y~U, data=.)

# Estimando o ATE do jeito correto
ATE_est = mean(predict(reg_1, newdata = data_train)) - 
  mean(predict(reg_0, newdata = data_train))
ATE_est


# Estimativa incorreta do efeito causal
data %>% 
  group_by(X) %>%  
  summarise(y.mean = mean(Y))



# Dados 2
n = 10^3
U <- rnorm(n)
X <- U > 0
M <- X + rnorm(n)
Y <- 2*U + 3*M + rnorm(n)
data = tibble(U = U, X = X, M = M, Y = Y)

train = 1:(0.8*10^4)
teste = (0.8*10^4+1):n 

data_train = data[train,]
data_teste = data[teste,]

# E[Y|do(X=x)] = E[E[Y|X=x, U]]
# ATE = E[E[Y|X=1, U]] - E[E[Y|X=0, U]]

# Estimando E[Y|X=1, U]
reg = data_train %>% 
  lm(Y~X+U, data=.)

# Estimando E[Y|X=0, U]
reg_0 = data_train %>% 
  filter(X == 0) %>% 
  lm(Y~U, data=.)

ATE = mean(predict(reg_1, newdata = data_teste)) - 
  mean(predict(reg_0, newdata = data_teste))
ATE
