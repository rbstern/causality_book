library(dplyr)
library(ggplot2)
library(SCtools)
library(Synth)

theme_set(theme_minimal())

alcohol %>% 
  filter(country_name == "Russian Federation") %>% 
  ggplot(aes(year, consumption))+
  geom_line()+
  geom_vline(xintercept = 2006, color  = "orange")+
  labs(
    title = "Per Capita Alcohol Consumption in the Russian Federation Since 1990",
    subtitle = "The Russian government began a series of policy changes in 2003\nto reduce alcohol consumption",
    y = "Per Capita Consumption (L/person)",
    x = NULL,
    caption = "Data: World Health Organization"
  )

alcohol %>% 
  filter(country_name == "Russian Federation") %>% 
  select(year, consumption,labor_force_participation_rate:manufacturing) %>% 
  tidyr::gather(predictor, value, -year) %>% 
  ggplot(aes(year, value))+
  geom_line()+
  facet_wrap(~predictor, scales = "free")

comparison_states <- c("USA", "UK", "UKR", "KAZ",
                       "GBR", "ESP", "DEU", "POL",
                       "FIN", "FRA", "GRC", "IRL",
                       "LTU", "ROU", "GEO", "MDA",
                       "SWE", "BEL", "BLR", "KGZ",
                       "CZE", "MEX", "SVN")

control_ids <- alcohol %>% 
  select(country_code,country_num) %>% 
  filter(country_code %in% comparison_states) %>% 
  distinct() %>% 
  pull(country_num)

dataprep.out <-dataprep(
  foo = as.data.frame(alcohol),
  predictors = c("labor_force_participation_rate",
                 "inflation",
                 "mobile_cellular_subscriptions",
                 "manufacturing"),
  predictors.op = "mean",
  dependent = "consumption",
  unit.variable = "country_num",
  time.variable = "year",
  treatment.identifier = 142,
  controls.identifier = control_ids,
  time.predictors.prior = c(1991:2005),
  time.optimize.ssr = c(2000:2005),
  special.predictors = list(
    list("consumption", 2000:2005 ,"mean")),
  unit.names.variable = "country_code",
  time.plot = 1992:2015
)

out <- synth(dataprep.out, Sigf.ipop = 3)

solution <- out$solution.w %>% 
  as.data.frame()

solution$country_num <- rownames(solution)

solution %>% 
  mutate(country_num = as.numeric(country_num)) %>% 
  left_join(alcohol %>% 
              select(country_code,country_num) %>% 
              filter(country_code %in% comparison_states) %>% 
              distinct(), by = "country_num") %>% 
  ggplot(aes(reorder(country_code, w.weight), w.weight))+
  geom_col()+
  coord_flip()+
  labs(
    title = "Donor Weights by Country",
    y = NULL,
    x = "Weight"
  )

path.plot(synth.res = out, dataprep.res = dataprep.out, 
          Xlab = "per Capita Alcohol Consumption", )
