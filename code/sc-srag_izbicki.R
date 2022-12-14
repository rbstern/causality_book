library(ggrepel)
library(ggthemes)
library(lubridate)
library(plotly)
library(scales)
library(tidyverse)

utils::globalVariables(unique(c(
  "CLASSI_FIN", "DT_INTERNA", "DT_NOTIFIC",
  "DT_SIN_PRI", "NU_IDADE_N", "SG_UF_NOT"
)))

pre_process <- function(dados, dia, semanas_trunca) {
  dados %>%
    dplyr::filter(!is.na(NU_IDADE_N), !is.na(DT_INTERNA),CLASSI_FIN==5) %>%
    dplyr::select(DT_INTERNA, DT_NOTIFIC, DT_SIN_PRI, SG_UF_NOT, NU_IDADE_N, CLASSI_FIN) %>%
    dplyr::mutate(
      DT_NOTIFIC = as.Date(DT_NOTIFIC, format = "%d/%m/%Y"),
      DT_INTERNA = as.Date(DT_INTERNA, format = "%d/%m/%Y"),
      DT_SIN_PRI = as.Date(DT_SIN_PRI, format = "%d/%m/%Y")
    ) %>%
    dplyr::filter(
      #DT_INTERNA > as.Date("2020-12-23"),
      DT_INTERNA > as.Date("2020-03-01"),
      DT_INTERNA <= dia - semanas_trunca * 7
    ) %>%
    dplyr::mutate(idade_cat = cut(NU_IDADE_N, c(0, 55, 63, 65, 120), right = FALSE)) %>%
    tibble::as_tibble()
}

#' Update SRAG data
#'
#' @export
update_srag_data <- function() {
  ckanr::ckanr_setup("https://opendatasus.saude.gov.br")
  
  arqs <- ckanr::package_search("srag 2020")$results %>%
    purrr::map("resources") %>%
    purrr::map(purrr::keep, ~.x$mimetype == "text/csv") %>%
    purrr::map_chr(purrr::pluck, 1, "url") %>% 
    stringr::str_subset("SRAG")
  
  arqs_new <- arqs %>% 
    stringr::str_subset("INFLUD20")
    
  dia <- arqs_new %>% 
    basename() %>% 
    str_replace("INFLUD20-", "") %>% 
    lubridate::dmy()
  
  semanas_trunca <- 1
  
  # demora
  dados <- purrr::map(arqs, data.table::fread)
  gc()
  
  # rapido
  dados_todos <- purrr::map_dfr(
    dados_tot, 
    pre_process,
    dia = dia, 
    semanas_trunca = semanas_trunca
  )
  gc()
  
  dados_todos
}

#dados <- update_srag_data()
#dados <- dados_todos %>% 
#  filter(SG_UF_NOT %in% "SP")

#write_rds(dados, "./code/dados/srag_2020.rds")
dados <- read_rds("./code/dados/srag_2020.rds") %>% 
  as_tibble()

dados_resumo_interna <- dados  %>% 
  filter(!is.na(idade_cat)) %>%
  filter(idade_cat %in% c("[55,63)","[65,120)")) %>% 
  group_by(idade_cat, DT_INTERNA) %>% 
  summarise(numero_casos = n()) %>% 
  ungroup() %>% 
  complete(idade_cat, DT_INTERNA, fill = list(numero_casos=0)) %>% 
  group_by(idade_cat) %>% 
  arrange(DT_INTERNA) %>% 
  mutate(numero_casos_media_movel=
           zoo::rollapply(numero_casos, 14, mean, align='right', fill = NA)) %>% 
  filter(!is.na(numero_casos_media_movel))%>% 
  filter(DT_INTERNA <= (as.Date("2021-05-28"))) %>% 
  select(!numero_casos)

data_spread <- dados_resumo_interna %>% 
  pivot_wider(names_from = idade_cat, values_from = numero_casos_media_movel)

fit <- lm(`[65,120)`~`[55,63)`,
          data = data_spread %>% 
            filter(DT_INTERNA <= as.Date("2021-02-08")))

#fit <- lm(`[55,63)`~`[65,120)`,
#          data = data_spread %>% 
#            filter(DT_INTERNA <= as.Date("2021-02-08")))

data_spread$contrafactual <-  predict(fit, newdata = data_spread)

evitadas <- data_spread %>% 
  filter(DT_INTERNA > as.Date("2021-02-08")+7*2) %>% 
  summarise(internacoes_evitadas=sum(contrafactual-`[65,120)`))

round(evitadas$internacoes_evitadas*0.45)
round(evitadas$internacoes_evitadas*12178.28/1000000)

pred_intervals <-  predict(fit,newdata = data_spread, interval="predict")

data_spread <- cbind(data_spread, pred_intervals)

data_long <- data_spread %>% 
  pivot_longer(cols= c("[55,63)","[65,120)", "contrafactual"),
               names_to="idade_cat",values_to="numero_casos_media_movel")

data_long_full <- data_long

trim <- 1
data_long$numero_casos_media_movel[
  data_long$idade_cat=="contrafactual"&data_long$DT_INTERNA<as.Date("2021-02-08")+trim] <- 
  NA
data_long$lwr[
  data_long$idade_cat=="contrafactual"&data_long$DT_INTERNA<as.Date("2021-02-08")+trim] <- 
  NA
data_long$upr[
  data_long$idade_cat=="contrafactual"&data_long$DT_INTERNA<as.Date("2021-02-08")+trim] <- 
  NA

reduction <- data_long %>% 
  filter(DT_INTERNA==max(DT_INTERNA))
1-reduction$numero_casos_media_movel[reduction$idade_cat=="[65,120)"]/reduction$numero_casos_media_movel[reduction$idade_cat=="contrafactual"]
1-reduction$numero_casos_media_movel[reduction$idade_cat=="[65,120)"]/reduction$upr[reduction$idade_cat=="contrafactual"]
1-reduction$numero_casos_media_movel[reduction$idade_cat=="[65,120)"]/reduction$lwr[reduction$idade_cat=="contrafactual"]


Sys.setenv("LANGUAGE"="En")
Sys.setlocale("LC_ALL", "English")

ggplot(data_long %>% 
         filter(idade_cat%in%c("[65,120)","contrafactual")))+
#  filter(idade_cat%in%c("[55,63)","contrafactual")))+
  geom_line(aes(x=DT_INTERNA,y=numero_casos_media_movel,color=idade_cat),size=1) +
  theme_bw()+
  theme(axis.text.x = element_text(size=18), legend.text = element_text(size = 14),legend.title = element_text( size = 16,face="bold"),axis.title=element_text(size=18),plot.title = element_text(size=12))+
  ylab("New hospitalizations due to SARI-COVID \n  in elderly patients (>65)")+
  xlab("")+
  geom_vline(xintercept=as.Date("2021-02-08"), colour="#e6ab02",size=1,alpha=0.5, lty = 2) +
  geom_text(aes(x=as.Date("2021-02-08"), 
                label="\n  1st dose 90+", y=90), 
            colour="#e6ab02", angle=90, text=element_text(size=10))+
  geom_vline(xintercept=as.Date("2021-04-21"), colour="#e6ab02",size=1,alpha=0.5, lty = 2) +
  geom_text(aes(x=as.Date("2021-04-21"), 
                label="\n  1st dose 65+", y=90), 
            colour="#e6ab02", angle=90, text=element_text(size=10))+
  geom_vline(xintercept=as.Date("2021-02-27"), colour="#e6ab02",size=1,alpha=0.5, lty = 2) +
  geom_text(aes(x=as.Date("2021-02-27"), 
                label="\n  1st dose 80+", y=90), 
            colour="#e6ab02", angle=90, text=element_text(size=10))+
  geom_vline(xintercept=as.Date("2021-03-26"), colour="#e6ab02",size=1,alpha=0.5, lty = 2) +
  geom_text(aes(x=as.Date("2021-03-26"), 
                label="\n  1st dose 70+", y=90), 
            colour="#e6ab02", angle=90, text=element_text(size=10))+
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b/%y")+
  #scale_color_brewer(name = "Faixa Etária",palette="Dark2")+
  scale_color_manual(name = "Hospitalizations in \nelderly patients (>65)",labels=c("\n Real \n","\n Without vaccines \n (projection)"),
                     values=c("#e6ab02","#1b9e77"),guide = guide_legend(reverse = TRUE))+
  geom_ribbon(data = data_long %>% filter(idade_cat=="contrafactual",
                                          DT_INTERNA>as.Date("2021-02-08")),
              aes(x=DT_INTERNA,ymin=lwr, ymax=upr),
              fill = "#1b9e77", linetype=2, alpha=0.2)+
  scale_y_continuous(breaks = seq(0, 1200, 200), 
                     limits = c(0, 1200), 
                     labels = seq(0, 1200, 200)) + 
  coord_cartesian(expand = FALSE) + 
  theme(axis.text.x  = element_text(size = 12), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), 
        panel.border = element_blank(), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(color = "gray79",
                                          size = 0.25, 
                                          linetype = 4))



#ggsave("contrafactual_paper_atualizado.png",width = 12,height = 7)


ggplot(data_long_full %>% 
         filter(idade_cat%in%c("[65,120)","contrafactual"),DT_INTERNA<as.Date("2021-02-08")))+
  geom_line(aes(x=DT_INTERNA,y=numero_casos_media_movel,color=idade_cat),size=1) +
  theme_bw()+
  theme(axis.text.x = element_text(size=18), legend.text = element_text(size = 14),legend.title = element_text( size = 16,face="bold"),axis.title=element_text(size=18),plot.title = element_text(size=12))+
  ylab("New hospitalizations due to SARI-COVID \n  in elderly patients (>65)")+
  xlab("")+
  geom_ribbon(data = data_long_full %>% filter(idade_cat=="contrafactual",
                                               DT_INTERNA<as.Date("2021-02-08")),
              aes(x=DT_INTERNA,ymin=lwr, ymax=upr),
              fill = "#1b9e77", linetype=2, alpha=0.2)+
  geom_vline(xintercept=as.Date("2021-02-08"), colour="#e6ab02",size=1,alpha=0.5, lty = 2) +
  geom_text(aes(x=as.Date("2021-02-08"), 
                label="\n  1st dose 90+", y=90), 
            colour="#e6ab02", angle=90, text=element_text(size=10))+
  geom_vline(xintercept=as.Date("2021-04-21"), colour="#e6ab02",size=1,alpha=0.5, lty = 2) +
  geom_text(aes(x=as.Date("2021-04-21"), 
                label="\n  1st dose 65+", y=90), 
            colour="#e6ab02", angle=90, text=element_text(size=10))+
  geom_vline(xintercept=as.Date("2021-02-27"), colour="#e6ab02",size=1,alpha=0.5, lty = 2) +
  geom_text(aes(x=as.Date("2021-02-27"), 
                label="\n  1st dose 80+", y=90), 
            colour="#e6ab02", angle=90, text=element_text(size=10))+
  geom_vline(xintercept=as.Date("2021-03-26"), colour="#e6ab02",size=1,alpha=0.5, lty = 2) +
  geom_text(aes(x=as.Date("2021-03-26"), 
                label="\n  1st dose 70+", y=90), 
            colour="#e6ab02", angle=90, text=element_text(size=10))+
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b/%y")+
  #scale_color_brewer(name = "Faixa Etária",palette="Dark2")+
  scale_color_manual(name = "Hospitalizations in \nelderly patients (>65)",labels=c("\n Real \n","\n Fitted Model"),
                     values=c("#e6ab02","#1b9e77"),guide = guide_legend(reverse = TRUE))+
  scale_y_continuous(breaks = seq(0, 1200, 200), 
                     limits = c(0, 1200), 
                     labels = seq(0, 1200, 200)) + 
  coord_cartesian(expand = FALSE) + 
  theme(axis.text.x  = element_text(size = 12), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), 
        panel.border = element_blank(), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(color = "gray79",
                                          size = 0.25, 
                                          linetype = 4))


#ggsave("contrafactual_paper_fitted_atualizado.png",width = 11,height = 7)




# alternativo -------------------------------------------------------------


Sys.setenv("LANGUAGE"="En")
#Sys.setlocale("LC_ALL", "English")
Sys.setlocale("LC_TIME", "C")
(g1 <- ggplot()+
    geom_vline(xintercept=as.Date("2021-02-08"), colour="black",size=1,alpha=0.2, lty = 2) +
    geom_text(aes(x=as.Date("2021-02-08"), 
                  label="\n  1st dose 90+", y=95), 
              colour="black", angle=90, text=element_text(size=11))+
    geom_vline(xintercept=as.Date("2021-04-21"), colour="black",size=1,alpha=0.2, lty = 2) +
    geom_text(aes(x=as.Date("2021-04-21"), 
                  label="\n  1st dose 65+", y=95), 
              colour="black", angle=90, text=element_text(size=11))+
    geom_vline(xintercept=as.Date("2021-02-27"), colour="black",size=1,alpha=0.2, lty = 2) +
    geom_text(aes(x=as.Date("2021-02-27"), 
                  label="\n  1st dose 80+", y=95), 
              colour="black", angle=90, text=element_text(size=11))+
    geom_vline(xintercept=as.Date("2021-03-26"), colour="black",size=1,alpha=0.2, lty = 2) +
    geom_text(aes(x=as.Date("2021-03-26"), 
                  label="\n  1st dose 70+", y=95), 
              colour="black", angle=90, text=element_text(size=11))+
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%b/%y")+
    geom_ribbon(data = data_long_full %>% filter(idade_cat=="contrafactual",
                                                 DT_INTERNA<as.Date("2021-02-08")),
                aes(x=DT_INTERNA,ymin=lwr, ymax=upr,color = "fitted", fill = "fitted"),
                linetype=3, alpha=0.1, size = 0.5, show.legend = FALSE)+
    geom_line(data = data_long_full %>% filter(idade_cat=="contrafactual",
                                               DT_INTERNA<as.Date("2021-02-08")),
              aes(x=DT_INTERNA,y=fit, color = "fitted"),
              linetype=1, size = .5)+
    geom_ribbon(data = data_long %>% filter(idade_cat=="contrafactual",
                                            DT_INTERNA>as.Date("2021-02-08")),
                aes(x=DT_INTERNA,ymin=lwr, ymax=upr),
                fill = "#D81B60", linetype=2, alpha=0.3)+
    geom_line(data = data_long %>% 
                filter(idade_cat%in%c("[65,120)","contrafactual")), 
              aes(x=DT_INTERNA,y=numero_casos_media_movel,color=idade_cat),size=1) +
    labs(y = "New hospitalizations due to \n SARI-COVID in elderly patients (>65)", 
         x = NULL) + 
    coord_cartesian(expand = FALSE) +
    scale_fill_manual(values = c("fitted" = "#004D40")) + 
    scale_color_manual(name = NULL,
                       labels=c("real",
                                "without vaccines (projection)", 
                                "fitted model"),
                       values=c("#004D40", 
                                "#D81B60",
                                "#1E88E5"),
                       guide = guide_legend(reverse = FALSE))+
    theme_bw() + 
    theme(legend.position = c(.2, .9),
          legend.key.size = unit(1.2,"line"),
          legend.key.width= unit(2, 'line'),
          legend.text=element_text(size=13),
          axis.text.x  = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"), 
          panel.border = element_blank(), 
          panel.grid.major.x = element_blank(), 
          panel.grid.major.y = element_line(color = "gray79",
                                            size = 0.25, 
                                            linetype = 4)))
ggsave("fig1_final.png", g1, width = 11, height = 7)


ggsave("fig1_final.png", g1, width = 11, height = 7)
ggsave("fig1_final.pdf", g1, width = 11, height = 7)
ggsave("fig1_final.tiff", g1, width = 11, height = 7,dpi=300)

(g1 <- ggplot()+
    geom_vline(xintercept=as.Date("2021-02-08"), colour="black",size=1,alpha=0.2, lty = 2) +
    geom_text(aes(x=as.Date("2021-02-08"), 
                  label="\n  1st dose 90+", y=95), 
              colour="black", angle=90, text=element_text(size=11))+
    geom_vline(xintercept=as.Date("2021-04-21"), colour="black",size=1,alpha=0.2, lty = 2) +
    geom_text(aes(x=as.Date("2021-04-21"), 
                  label="\n  1st dose 65+", y=95), 
              colour="black", angle=90, text=element_text(size=11))+
    geom_vline(xintercept=as.Date("2021-02-27"), colour="black",size=1,alpha=0.2, lty = 2) +
    geom_text(aes(x=as.Date("2021-02-27"), 
                  label="\n  1st dose 80+", y=95), 
              colour="black", angle=90, text=element_text(size=11))+
    geom_vline(xintercept=as.Date("2021-03-26"), colour="black",size=1,alpha=0.2, lty = 2) +
    geom_text(aes(x=as.Date("2021-03-26"), 
                  label="\n  1st dose 70+", y=95), 
              colour="black", angle=90, text=element_text(size=11))+
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%b/%y")+
    geom_ribbon(data = data_long_full %>% filter(idade_cat=="contrafactual",
                                                 DT_INTERNA<as.Date("2021-02-08")),
                aes(x=DT_INTERNA,ymin=lwr, ymax=upr,color = "fitted", fill = "fitted"),
                linetype=3, alpha=0.1, size = 0.5, show.legend = FALSE)+
    geom_line(data = data_long_full %>% filter(idade_cat=="contrafactual",
                                               DT_INTERNA<as.Date("2021-02-08")),
              aes(x=DT_INTERNA,y=fit, color = "fitted"),
              linetype=1, size = .5)+
    geom_ribbon(data = data_long %>% filter(idade_cat=="contrafactual",
                                            DT_INTERNA>as.Date("2021-02-08")),
                aes(x=DT_INTERNA,ymin=lwr, ymax=upr),
                fill = "#D81B60", linetype=2, alpha=0.3)+
    geom_line(data = data_long %>% 
                filter(idade_cat%in%c("[65,120)","contrafactual")), 
              aes(x=DT_INTERNA,y=numero_casos_media_movel,color=idade_cat),size=1) +
    labs(y = "New hospitalizations due to \n SARI-COVID in elderly patients (>65)", 
         x = NULL) + 
    coord_cartesian(expand = FALSE) +
    scale_fill_manual(values = c("fitted" = "#004D40")) + 
    scale_color_manual(name = NULL,
                       labels=c("observed",
                                "without vaccines (projection)", 
                                "fitted model"),
                       values=c("#004D40", 
                                "#D81B60",
                                "#1E88E5"),
                       guide = guide_legend(reverse = FALSE))+
    theme_bw() + 
    theme(legend.position = c(.2, .9),
          legend.key.size = unit(1.2,"line"),
          legend.key.width= unit(2, 'line'),
          legend.text=element_text(size=13),
          axis.text.x  = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"), 
          panel.border = element_blank(), 
          panel.grid.major.x = element_blank(), 
          panel.grid.major.y = element_line(color = "gray79",
                                            size = 0.25, 
                                            linetype = 4)))
ggsave("fig1_final_t.png", g1, width = 11, height = 7)