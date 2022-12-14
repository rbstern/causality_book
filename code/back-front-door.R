library(ggdag)
library(rstanarm)
library(tidybayes)
library(tidyverse)

# IPW

dag <- downloadGraph("dagitty.net/m331")
ggdag(dag) + labs(title = "")

n <- 500
b <- rnorm(n)
c <- rnorm(n)
z <- -2*b +2*c + rnorm(n)
a <- 3*b + rnorm(n)
d <- -3*c + rnorm(n)
x <- 4+a +2*z + rnorm(n)
w <- -2*x + rnorm(n)
y <- z + 2*w + d + rnorm(n)

data <- tibble(b, c, z, a, d, x, w, y)

model_backdoor <- stan_glm(y ~ x + a + z, data = data, refresh = 0)
model_backdoor %>% 
  spread_draws(x) %>% 
  ggplot(aes(x)) +
  stat_halfeye(alpha = 0.6) + 
  hrbrthemes::theme_ipsum_rc(grid = "y") +
  geom_vline(aes(xintercept = -4), linetype = 2, color = "red") +
  annotate("text", x = -4.08, y = 0.7, label = "True causal effect", color = "red", 
           family = theme_get()$text[["family"]]) +
  labs(title = "Causal inference from Model y ~ x + a + z",
       subtitle = "We deconfound our estimates by conditioning on a and z")

lm(y~w)$coefficients[2]*lm(w~x)$coefficients[2]

model_frontdoor <- ulam(
  alist(
    c(Y, W) ~ multi_normal(c(muY, muW), Rho, Sigma),
    muY <- alphaY + delta*W,
    muW <- alphaW + gamma*X, 
    gq> ate <- gamma * delta, # calculate ate directly in stan
    c(alphaY, alphaW) ~ normal(0, 0.2),
    c(gamma, delta) ~ normal(0, 0.5),
    Rho ~ lkj_corr(2),
    Sigma ~ exponential(1)
  ),
  data = list(Y = data$y, X = data$x, W = data$w), chains = 4, cores = 4, iter = 3000
)
precis(model_frontdoor)
