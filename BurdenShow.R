rm(list = ls())

library(readr)
library(dplyr)
library(tidyverse)
library(purrr)
#install.packages("errors")
library(errors)
library(Hmisc)
#install.packages("ggrepel")
require("ggrepel")

# install.packages("remotes")
# remotes::install_github("coolbutuseless/ggpattern")
# install.packages("ggpattern")
# library(ggpattern)

popHessen <-
  6116203#Quelle: Summe der Exponierten nach Kartierungsdokumentation

doseRes_list <- read_delim(
  "rawdata/ERF_Study_Data.csv",
  "\t",
  escape_double = FALSE,
  locale = locale(date_names = "de", decimal_mark = ","),
  trim_ws = TRUE
)
doseRes_list$rrPer10dB <-
  set_errors(doseRes_list$rrPer10dB,
             doseRes_list$rrPer10dB - doseRes_list$rrLoCI)
doseRes_list$linearTerm <- doseRes_list$linearTerm %>% set_errors()
doseRes_list$constantTerm <-
  doseRes_list$constantTerm %>% set_errors()
doseRes_list$quadraticTerm <-
  doseRes_list$quadraticTerm %>% set_errors()

set <-
  .Primitive("[[<-") #eine Hilfsfunktion, die definiert wird, um in Pipes Werte manipulieren zu können.

plus_Lochmann <- read_delim(
  "rawdata/StraßePlusHessen2.txt",
  " ",
  escape_double = FALSE,
  locale = locale(decimal_mark = "."),
  trim_ws = TRUE#,  col_types = "cnnccn"
) %>%
  filter(FP_HP == "HP") %>% #nutze hier nur die Pegel der Hauspunkte (also lauteste Fassadenpegel)
  transmute(
    Lo = start,
    LoIncluded = TRUE,
    Hi = end,
    HiIncluded = FALSE,
    Exposed = Belastete,
    noiseMetric = Metrik,
    source = "PLUS"
  )
plus_Lochmann$noiseMetric <- plus_Lochmann$noiseMetric %>%
  ifelse(. == "LN", "Lnight", .)

# persSum<-plus_Lochmann %>% group_by(noiseMetric) %>% summarise_at(c("Exposed"),sum)
# persSum
# persSum[2]/popHessen
## rausfinden, dass in diesem Datensatz etwa 9 % weniger Belastete zu finden sind als hessische Einwohner.

plus_Lochmann <- plus_Lochmann %>% filter(noiseMetric == "LDEN") %>%
  mutate(noiseMetric = "L24h",
         Lo = Lo - 3.3,
         Hi = Hi - 3.3) %>%
  rbind(plus_Lochmann)
plus_Lochmann <- plus_Lochmann %>%
  mutate(source = "PLUS - 3 dB",
         Lo = Lo - 3,
         Hi = Hi - 3) %>%
  rbind(plus_Lochmann)


END_LDEN <- read_delim(
  "rawdata/Exponierte_ULR_LDEN.csv",
  "\t",
  escape_double = FALSE,
  locale = locale(date_names = "de",
                  decimal_mark = ","),
  trim_ws = TRUE
) %>% mutate(noiseMetric = "LDEN", source = "END")

END_L24h <-
  END_LDEN %>% mutate(
    Lo = Lo - 3.3,
    Hi = Hi - 3.3,
    source = "END",
    noiseMetric = "L24h"
  )

END_Lnight <- read_delim(
  "rawdata/Exponierte_ULR_Lnight.csv",
  "\t",
  escape_double = FALSE,
  locale = locale(date_names = "de",
                  decimal_mark = ","),
  trim_ws = TRUE
) %>% mutate(noiseMetric = "Lnight", source = "END")

exposure_list <-
  rbind(plus_Lochmann,
        # plus_LDEN_Hege,
        # plus_L24h,
        # plus_LDEN_red,
        # plus_L24h_red,
        END_LDEN,
        END_L24h,
        END_Lnight) %>% mutate(
          Lmid = (Lo + Hi) / 2,
          ExposedPerdB = Exposed /
            (Hi - Lo),
          .before = 1
        ) %>% .[-which(.$Lmid == Inf), ]

burden_list <- read_delim(
  "rawdata/AllRiskCauseSpecificBurden.csv",
  "\t",
  escape_double = FALSE,
  locale = locale(date_names = "de", decimal_mark = ","),
  trim_ws = TRUE
)

matTheme<-
  theme(text = element_text(size=rel(4)),
        strip.text = element_text(size=rel(4)),
        axis.text = element_text(size=rel(1.05)),
        axis.title = element_text(size=rel(1.1)),
        legend.text = element_text(size=rel(3.2)))

pl1 <-
  ggplot(exposure_list, aes(x = Lo, y = ExposedPerdB/100000, color = source))
pl1 + geom_step(direction = "hv") + #waagrechter Strich bis zum nächsten Pegelwert, dann gerade runter
  facet_grid(cols = vars(noiseMetric)) +
  geom_segment(aes(xend = Hi, yend = ExposedPerdB/100000)) + #geom_segment, um noch das letzte waagrechte hinzuzufügen. Alleine wären keine senkrechten Striche
  ylim(0, 3.2) + xlim(40, 85) +
  #labs(x = "Sound pressure level on (loudest/every) facade [dB(A)]",y="Exposed [100,000/dB]")+
  labs(x = "Pegel am (lautesten/jedem) Fassadenpunkt [dB(A)]",y="Exponierte [100,000/dB]")+
  scale_color_discrete("Datenquelle")+
  #scale_color_discrete("data source")+
  matTheme#+theme(legend.position="none")
ggsave("graphs/Exposure.png",width=10,height = 5)
#exposure_list %>% describe()

short_l <- doseRes_list %>% filter(included == TRUE) %>%
  select(
    shortName,
    outcome,
    outcomeGroup,
    noiseMetric,
    counterfactualValue,
    rrPer10dB,
    rrLoCI,
    rrHiCI,
    quadraticTerm,
    linearTerm,
    constantTerm,
    burdenCalculation
  )



# paraTransform = function(x_tbl) {
#   x_tbl$quadraticTerm <- 0
#   x_tbl$linearTerm <- (x_tbl$rrPer10dB - 1) * 10
#   x_tbl$constantTerm <-
#     -(x_tbl$linearTerm*x_tbl$counterfactualValue )
#   return(x_tbl)
# }
#
# short_l[is.na(short_l$quadraticTerm), ] <-
#   paraTransform(short_l[is.na(short_l$quadraticTerm), ])


quadr = function(x_tbl) {
  return(
    x_tbl$quadraticTerm * x_tbl$Lmid ^ 2 +
      x_tbl$linearTerm * x_tbl$Lmid +
      x_tbl$constantTerm
  )
}
linF = function(x_tbl) {
  dummy <- case_when(#dieser Befehl ist hier wichtig, damit alle Werte im Vektor einzeln prozessiert werden
    x_tbl$Lmid > x_tbl$counterfactualValue ~ (x_tbl$rrPer10dB - 1) * 10 * (x_tbl$Lmid - x_tbl$counterfactualValue),
    x_tbl$Lmid <= x_tbl$counterfactualValue ~ set_errors(0, 0)
  )
  return(dummy)
}

#hier generiere eine Liste mit Auswertestellen (Pegeln), an denen Wirkungen berechnet werden.
Lstep <- 0.1 #Schrittgröße
auswertestellen <- seq(40, 80, Lstep)
doseRespF_data <-
  short_l %>% crossing(Lmid = auswertestellen)%>%   mutate(effectSize = set_errors(NA, 0)) %>%
  mutate(effectSize = case_when(# je nachdem, ob Wert für lineare Steigung verfügbar oder nicht.
    is.na(rrPer10dB) ~ quadr(.),
    !is.na(rrPer10dB) ~ linF(.)
  ))

drfLabel<-doseRespF_data %>% group_by(shortName) %>% summarise_at(vars(effectSize),list(Ende=max))
pl2 <-
  ggplot(
    doseRespF_data,
    aes(
      x = Lmid,
      y = effectSize,
      color = shortName,
      ymin = errors_min(effectSize),
      ymax = errors_max(effectSize)
    )
  )
pl2 + geom_line(size = 1.2)+
  # geom_text_repel(data=drfLabel,inherit.aes = F,#to be to facet selection!
  #   mapping=aes(x = 70, y = Ende,
  #   label = shortName,color=shortName),
  #   size = 3.5,
  #   angle = 55,show.legend = F
  # ) +
  facet_grid(cols = vars(noiseMetric)) +
  geom_ribbon(aes(fill = shortName, linetype = NA), alpha = 0.2) +
  scale_color_discrete("Endpunkt") +
  scale_fill_discrete("Endpunkt") +
  #scale_color_discrete("endpoint") +
  #scale_fill_discrete("endpoint") +
  #labs(x = "Sound pressure level on loudest facade [dB(A)]", y = "effect size [%HA, %HSD or %ExcessRisk]") +
  labs(x = "Pegel am lautesten Fassadenpunkt [dB(A)]", y = "Effektgröße [%HA, %HSD oder %ExzRisiko]") +
  matTheme#+theme(legend.position="none")
ggsave("graphs/DRF.png",width=12,height = 5)
#ggsave("graphs/DRFEngl.png",width=12,height = 5)
#ggsave("graphs/DRFOLeg.png",width=10,height = 5)

er_list <-
  left_join(exposure_list, short_l, by = "noiseMetric") %>%mutate(response =  -1000000)

#Spalte response soll numerisch sein
#Enthält dann entweder %HA, %HSD oder %Risikoerhöhung
er_list$response <-
  case_when(
    er_list$Lmid < er_list$counterfactualValue ~ set_errors(0, 0),# unter der Schwelle wird keine Wirkung angenommen
    is.na(er_list$rrPer10dB) ~ quadr(er_list),    # je nachdem, ob Wert für lineare Steigung verfügbar oder nicht
    !is.na(er_list$rrPer10dB) ~ linF(er_list)
  )
er_list <- er_list %>% mutate(affected_i = Exposed * response / 100)

dw_list <-
  data.frame(outcome = c("Annoyance", "Sleep disorders"),
             dw = c(0.02, 0.07))

# Problem des Paketes Error: wenn aus Werten in einem Dataframe Summen gebildet werden,
# wird automatisch für die Autokorrelation der Default-Wert angewendet.
# Abweichende Setzungen sind dann nicht möglich. Hier soll aber maximale Korrelation verwendet werden! Deshalb definiere eigene Funktio

sumGrossFehler = function(x){
  set_errors(sum(x), value=(sum(errors_max(x))-sum(errors_min(x)))/2)
  }

streetNoiseBurden1 <-
  er_list %>% filter(burdenCalculation == "paf")  %>%
  mutate(PAF_i = affected_i / popHessen / (1 + affected_i / popHessen) *
           100,
         .keep = "unused")%>%
  group_by(source, noiseMetric, shortName,outcome,outcomeGroup) %>%
  summarise(PAF = sumGrossFehler(PAF_i))%>%
  left_join(., data.frame(filter(
    burden_list, population ==
      "Hessians aged over 40 years"
  )),
  by = "outcome")%>%
  left_join(., dw_list, by = "outcome") %>%
  mutate(attrBurden = PAF * burden / 100) %>%
  select(source, noiseMetric, shortName, outcome,outcomeGroup, PAF, attrBurden) %>%
  mutate(dw = NA , HAorHSD = NA)

streetNoiseBurden2 <- er_list %>%
  filter(burdenCalculation == "total") %>%
  group_by(source, noiseMetric, shortName,outcome,outcomeGroup) %>%
  summarise(HAorHSD = sumGrossFehler(affected_i))%>%
  left_join(., dw_list, by = "outcome") %>%
  mutate(attrBurden = HAorHSD * dw, PAF = NA)
# names(streetNoiseBurden1)
# names(streetNoiseBurden2)
streetNoiseBurden <- rbind(streetNoiseBurden1, streetNoiseBurden2)
SA0<-data.frame()

pl3 <-
  ggplot(
    streetNoiseBurden,
    aes(
      x = outcomeGroup,
      xlab = NULL,
      y = 100000 * attrBurden / popHessen,
    #  group=shortName,
      fill = shortName
    )
  ) +
 # theme(axis.text.x = element_blank()) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(
    ymin = errors_min(100000 * attrBurden / popHessen),
    ymax = errors_max(100000 * attrBurden / popHessen)),
    width=.6,
    size=1,
    position=position_dodge(.9)
  ) +
  facet_grid(cols = vars(source)) +
  # geom_text(label = streetNoiseBurden$shortName,
  #           size = 3.5,
  #           angle = 35,
  #           position=position_dodge(.9)) +
  xlab(NULL) + ylab("attr. burden [DALY/year/100,000 pers]")+
  scale_fill_discrete("endpoint")
pl3 + matTheme + theme(axis.text.x = element_text(angle = 25)) #+theme(legend.position = "none")

ggsave("graphs/StreetNoiseBurdenEngl.png",width=12,height = 5)
ggsave("graphs/StreetNoiseBurdenOLeg.png",width=10,height = 5)



streetNoiseBurden %>% group_by(shortName) %>%
  summarise_at(c("PAF","attrBurden"),sum)
