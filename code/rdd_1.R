library(tidyverse)

n <- 1000
Z <- rnorm(n)
X <- Z >= 0
Y <- rnorm(n, 50*(X+1)*(Z+1))
data <- tibble(X=X, Y=Y, Z=Z)

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
