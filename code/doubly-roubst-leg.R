library(randomForest)
library(tidyverse)
br <- causaldata::black_politicians %>% 
  mutate(
    responded = as.factor(responded),
    leg_black = as.factor(leg_black)
  )

# Descrição das variáveis usadas:
# -------------------------------
# responded: resposta  - politico respondeu carta/e-mail?
# leg_black: tratament - político é preto?
# Backdoor
# medianhhincom: renda media
# blackpercent:  porcentagem de pretos
# leg_democrat:  legislador é democrata?

# Regressão: fórmula do ajuste
# E[E[Y|X=1,Z]] - E[E[Y|X=0,Z]]

# Via regressão logística
fm = responded ~ medianhhincom + blackpercent + leg_democrat
reg_1_log <- br %>% 
  filter(leg_black == 1) %>% 
  glm(fm, family = binomial, data = .)
reg_0_log <- br %>% 
  filter(leg_black == 0) %>% 
  glm(fm, family = binomial, data = .)

p_1 = mean(predict(reg_1_log, br, type = "response"))
p_0 = mean(predict(reg_0_log, br, type = "response"))
ATE = p_1 - p_0

# Via floresta aleatória
fm = responded ~ medianhhincom + blackpercent + leg_democrat
reg_1_forest <- br %>% 
  filter(leg_black == 1) %>% 
  randomForest(fm, data = .)
reg_0_forest <- br %>% 
  filter(leg_black == 0) %>% 
  randomForest(fm, data = .)

p_1 = mean(predict(reg_1_forest, br, type = "prob")[,2])
p_0 = mean(predict(reg_0_forest, br, type = "prob")[,2])
ATE = p_1 - p_0

# IPW (inverse probability weighting)
# g(z) = P(X=1|Z)
# p_1 = E[Y*I(X=1)/g(Z)]
# p_0 = E[Y*I(X=0)/(1-g(Z))]

# via Regressão Logística
fm = leg_black ~ medianhhincom + blackpercent + leg_democrat
reg_log = br %>% 
  glm(fm, family = binomial, data = .)
preds_log = predict(reg_log, br, type = "response")
br$pred_log = preds_log

br %>% 
  mutate(responded = responded == 1,
         leg_black = leg_black == 1,
         p1 = (responded*leg_black)/pred_log,
         p0 = (responded*(1-leg_black))/(1-pred_log)
  ) %>% 
  summarise(p1 = mean(p1),
            p0 = mean(p0)) %>% 
  mutate(ATE = p1 - p0)

# via Floresta Aleatória

fm = leg_black ~ medianhhincom + blackpercent + leg_democrat
reg_forest = br %>% 
  randomForest(fm, data = .)
preds_forest = predict(reg_forest, br, type = "prob")[,2]
preds_forest[preds_forest == 0] = 0.001
preds_forest[preds_forest == 1] = 0.999
br$pred_forest = preds_forest

br %>% 
  filter(leg_black == 1) %>% 
  mutate(dif = pred_forest-pred_log) %>% 
  ggplot(aes(x=dif)) +
  geom_histogram()

br %>% 
  mutate(responded = responded == 1,
         leg_black = leg_black == 1,
         p1 = (responded*leg_black)/pred,
         p0 = (responded*(1-leg_black))/(1-pred)
  ) %>% 
  summarise(p1 = mean(p1),
            p0 = mean(p0)) %>% 
  mutate(ATE = p1 - p0)

# Análise dos classificadores
br %>% 
  mutate(
    leg_black = leg_black == 1,
    acc_log = abs(leg_black-pred_log),
    acc_forest = abs(leg_black-pred_forest)
  ) %>% 
  filter(leg_black) %>% 
  summarise(acc_log = mean(acc_log),
            acc_forest = mean(acc_forest))0

## Doubly-robust

# Via regressão logística
br %>% 
  mutate(
    responded = responded == 1,
    leg_black = leg_black == 1,
    mu1 = predict(reg_1_log, br, type = "response"),
    mu0 = predict(reg_0_log, br, type = "response"),
    p1 = ((responded-mu1)*leg_black/pred_log) + mu1,
    p0 = ((responded-mu0)*(1-leg_black)/(1-pred_log)) + mu0
  ) %>% 
  summarise(p1 = mean(p1), 
            p0 = mean(p0)) %>% 
  mutate(ATE = p1 - p0)


# Via floresta aleatória
br %>% 
  mutate(
    responded = responded == 1,
    leg_black = leg_black == 1,
    mu1 = predict(reg_1_forest, br, type = "prob")[,2],
    mu0 = predict(reg_0_forest, br, type = "prob")[,2],
    p1 = ((responded-mu1)*leg_black/pred_forest) + mu1,
    p0 = ((responded-mu0)*(1-leg_black)/(1-pred_forest)) + mu0
  ) %>% 
  summarise(p1 = mean(p1), 
            p0 = mean(p0)) %>% 
  mutate(ATE = p1 - p0)
