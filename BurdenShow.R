rm(list = ls())

library(readr)
library(dplyr)
library(tidyverse)
library(purrr)
#install.packages("errors")
library(errors)


popHessen <-
  6116203#Quelle: Summe der Exponierten nach Kartierungsdokumentation

doseRes_list <- read_delim(
  "rawdata/Dosis_Wirkungs_Studien.csv",
  ";",
  escape_double = FALSE,
  locale = locale(date_names = "de", decimal_mark = ","),
  trim_ws = TRUE
)
doseRes_list$rrPer10dB <-
  set_errors(doseRes_list$rrPer10dB,
             doseRes_list$rrPer10dB - doseRes_list$rrLoCI)
doseRes_list$linearTerm<-doseRes_list$linearTerm %>% set_errors()
doseRes_list$constantTerm<-doseRes_list$constantTerm %>% set_errors()
doseRes_list$quadraticTerm<-doseRes_list$quadraticTerm %>% set_errors()

set <-
  .Primitive("[[<-") #eine Hilfsfunktion, die definiert wird, um in Pipes Werte manipulieren zu können.

plus_LDEN <- read_delim(
  "rawdata/PLUS_Lden_Hegewald.csv",
  ";",
  escape_double = FALSE,
  locale = locale(decimal_mark = "."),
  trim_ws = TRUE
) %>% select(road_lden, GKZ, ew_lden) %>%
  aggregate(ew_lden ~ road_lden,
            data = .,
            sum) %>%
  set(1, "road_lden", 39.9) %>%
  mutate(
    Lo = road_lden - 0.05,
    LoIncluded = TRUE,
    Hi = road_lden + 0.05,
    HiIncluded = FALSE,
    Exposed = ew_lden,
    noiseMetric = "LDEN",
    source = "plus",
    .keep = "unused"
  )

plus_L24h <- plus_LDEN %>% mutate(
  Lo = Lo - 3.3,
  Hi = Hi - 3.3,
  source = "plus",
  noiseMetric = "L24h"
)

plus_LDEN_red <-
  plus_LDEN %>% mutate(Lo = Lo - 3,
                       Hi = Hi - 3,
                       source = "plus-3dB")

plus_L24h_red <-
  plus_L24h %>% mutate(Lo = Lo - 3,
                       Hi = Hi - 3,
                       source = "plus-3dB")

END_LDEN <- read_delim(
  "rawdata/Exponierte_ULR_LDEN.csv",
  ";",
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
  ";",
  escape_double = FALSE,
  locale = locale(date_names = "de",
                  decimal_mark = ","),
  trim_ws = TRUE
) %>% mutate(noiseMetric = "Lnight", source = "END")

exposure_list <-
  rbind(plus_LDEN,
        plus_L24h,
        plus_LDEN_red,
        plus_L24h_red,
        END_LDEN,
        END_L24h,
        END_Lnight) %>% mutate(
          Lmid = (Lo + Hi) / 2,
          ExposedPerdB = Exposed /
            (Hi - Lo),
          .before = 1
        )

burden_list <- read_delim(
  "rawdata/AllRiskCauseSpecificBurden.csv",
  ";",
  escape_double = FALSE,
  locale = locale(date_names = "de", decimal_mark = ","),
  trim_ws = TRUE
)


pl1 <-
  ggplot(exposure_list, aes(x = Lo, y = ExposedPerdB, color = source))
pl1 + geom_step(direction = "hv") + #waagrechter Strich bis zum nächsten Pegelwert, dann gerade runter
  facet_grid(cols = vars(noiseMetric)) +
  geom_segment(aes(xend = Hi, yend = ExposedPerdB)) + #geom_segment, um noch das letzte waagrechte hinzuzufügen. Alleine wären keine senkrechten Striche
  ylim(0, 300000) + xlim(40, 85)
#ggsave("graphs/Exposure.pdf")

short_l <- doseRes_list %>% filter(included == TRUE) %>%
  select(
    shortName,
    outcome,
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
pl2 + geom_line() + facet_grid(cols = vars(noiseMetric)) + 
  geom_ribbon(aes(fill =shortName, linetype = NA), alpha = 0.15)
#ggsave("graphs/DRF.pdf")

er_list <-
  left_join(exposure_list, short_l, by = "noiseMetric") %>%mutate(response =  -1000000)

#Spalte response soll numerisch sein
#Enthält dann entweder %HA, %HSD oder %Risikoerhöhung
er_list$response<-case_when(# je nachdem, ob Wert für lineare Steigung verfügbar oder nicht.
             is.na(er_list$rrPer10dB) ~ quadr(er_list),
             !is.na(er_list$rrPer10dB) ~ linF(er_list))

er_list <- er_list %>% mutate(affected_i = Exposed * response / 100)

dw_list <-
  data.frame(outcome = c("Annoyance", "Sleep disorders"),
             dw = c(0.02, 0.07))


streetNoiseBurden1 <-
  er_list %>% filter(burdenCalculation == "paf")  %>% 
  mutate(PAF_i = affected_i / popHessen / (1 + affected_i / popHessen) *
           100,
         .keep = "unused") %>%
  group_by(source, noiseMetric, shortName,outcome) %>%
  summarise(PAF = sum(PAF_i))%>%
  left_join(., data.frame(filter(
    burden_list, population ==
      "Hessians aged over 40 years"
  )),
  by = "outcome")%>%
  left_join(., dw_list, by = "outcome") %>%
  mutate(attrBurden = PAF * burden / 100) %>%
  select(source, noiseMetric, shortName, outcome, PAF, attrBurden) %>%
  mutate(dw = NA , HAorHSD = NA) 

#Problem: die Autokorrelation ist nicht eingetragen#

bla<-set_errors(seq(2,8,1),value=1)
blubb<-set_errors(seq(5,11,1),value=1)
correl(bla[1],bla[2])<-1
correl(bla[1],bla[2])
print(bla[1]+bla[2],digits=4)


#-------------------------------------------------------#
streetNoiseBurden2 <- er_list %>%
  filter(burdenCalculation == "total") %>%
  group_by(source, noiseMetric, shortName,outcome) %>%
  summarise(HAorHSD = sum(affected_i))%>%
  left_join(., dw_list, by = "outcome") %>%
  mutate(attrBurden = HAorHSD * dw, PAF = NA)
names(streetNoiseBurden1)
names(streetNoiseBurden2)
streetNoiseBurden <- rbind(streetNoiseBurden1, streetNoiseBurden2)
pl3 <-
  ggplot(
    streetNoiseBurden,
    aes(
      x = shortName,
      xlab = NULL,
      y = 100000 * attrBurden / popHessen,
      fill = shortName
    )
  ) +
  theme(axis.text.x = element_blank()) +
  geom_col() +geom_errorbar(aes(ymin=errors_min(100000 * attrBurden / popHessen),ymax=errors_max(100000 * attrBurden / popHessen)))+
  facet_grid(cols = vars(source)) + geom_text(label = streetNoiseBurden$shortName,
                                              size = 3.5,
                                              angle = 55) +
  xlab(NULL) + ylab("attributable Burden /100000")
pl3
ggsave("graphs/StreetNoiseBurden.pdf")
