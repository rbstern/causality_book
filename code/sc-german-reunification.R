library(janitor)
library(tidyverse)
library(magrittr)

dados <- "./code/dados/german_reunification.csv" %>% 
  read_csv() %>% 
  select(country, year, gdp)
dados <- dados %>% 
  spread(country, gdp) %>% 
  clean_names()

analise_sc <- function(ano_trat, ano_fim)
{
  dados_pre <- dados %>% 
    filter(year <= ano_trat)
  
  aux <- lm(west_germany~., data = dados_pre[,-1])
  
  dados %<>%
    mutate(contrafactual = predict(aux, newdata = dados))
  
  dados_fim <- dados %>% 
    gather(country, gdp, -year) %>% 
    filter(country %in% c("west_germany", "contrafactual")) %>% 
    filter(year <= ano_fim)
  
  dados_fim %>% 
    ggplot(aes(x = year, y = gdp, group = country, colour = country)) +
    geom_line()
}

# analise sc
analise_sc(1990, 2020)
# analise placebo
analise_sc(1982, 1990)
