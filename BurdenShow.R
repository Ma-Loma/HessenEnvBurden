rm(list = ls())

library(readr)
library(dplyr)
library(tidyverse)
library(purrr)

popHessen <-
  6116203#Quelle: Summe der Exponierten nach Kartierungsdokumentation

doseRes_list <- read_delim(
  "rawdata/Dosis_Wirkungs_Studien.csv",
  ";",
  escape_double = FALSE,
  locale = locale(date_names = "de", decimal_mark = ","),
  trim_ws = TRUE
)

set <- .Primitive("[[<-")

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

# test_LDEN <- read_delim(
#   "rawdata/Exponierte_Test_LDEN.csv",
#   ";",
#   escape_double = FALSE,
#   locale = locale(date_names = "de",
#                   decimal_mark = ","),
#   trim_ws = TRUE
# ) %>% mutate(noiseMetric = "LDEN", source = "test")
#
#
# test_LDEN_red <-
#   test_LDEN %>% mutate(Lo = Lo - 3,
#                        Hi = Hi - 3,
#                        source = "test-3dB")
#
# test_L24h <-
#   test_LDEN %>% mutate(
#     Lo = Lo - 3.3,
#     Hi = Hi - 3.3,
#     source = "test",
#     noiseMetric = "L24h"
#   )

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
ggsave("graphs/Exposure.pdf")

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

linFkt = function(pegel, rr, cf, q, l, c) {
  # stelle als prozentuale Risikoerhöhung dar
  (rr - 1) * 10 * pegel - cf *
    (rr - 1) * 10
}
quadr = function(pegel, rr, cf, q, l, c) {
  q * pegel ^ 2 + l *
    pegel + c
}
poly = function(pegel, rr, cf, q, l, c) {
  if (is.na(q))
    data.frame(L = pegel, effectSize = pmax(linFkt(pegel, rr, cf, q, l, c), 0))
  else
    data.frame(L = pegel, effectSize = quadr(pegel, rr, cf, q, l, c))
}

Lstep <- 0.1
auswertestellen <- seq(40, 80, Lstep)
doseRespF_data <- c()
for (j in 1:nrow(short_l)) {
  doseRespF_data <-
    poly(
      auswertestellen,
      as.numeric(short_l[j, "rrPer10dB"]),
      as.numeric(short_l[j, "counterfactualValue"]),
      as.numeric(short_l[j, "quadraticTerm"]),
      as.numeric(short_l[j, "linearTerm"]),
      as.numeric(short_l[j, "constantTerm"])
    )  %>% mutate(short_l[j, c("shortName", "noiseMetric")]) %>% bind_rows(doseRespF_data)
}

pl2 <-
  ggplot(doseRespF_data, aes(x = L, y = effectSize, color = shortName))
pl2 + geom_line() + facet_grid(cols = vars(noiseMetric))
#ggsave("graphs/DRF.pdf")


er_list <-
  left_join(exposure_list, short_l, by = "noiseMetric") %>% mutate(response =
                                                                     -1000000)
#Spalte response soll numerisch sein, und noch leer sein, wird gleich gefüllt.
#Enthält dann entweder %HA, %HSD oder %Risikoerhöhung
for (j in 1:nrow(er_list)) {
  er_list[j, "response"] <- poly(
    as.numeric(er_list[j, "Lmid"]),
    as.numeric(er_list[j, "rrPer10dB"]),
    as.numeric(er_list[j, "counterfactualValue"]),
    as.numeric(er_list[j, "quadraticTerm"]),
    as.numeric(er_list[j, "linearTerm"]),
    as.numeric(er_list[j, "constantTerm"])
  )["effectSize"]
}
er_list <- er_list %>% mutate(affected_i = Exposed * response / 100)

dw_list <-
  data.frame(outcome = c("Annoyance", "Sleep disorders"),
             dw = c(0.02, 0.07))

streetNoiseBurden1 <- er_list %>%
  filter(burdenCalculation == "paf") %>% aggregate(affected_i ~ source + noiseMetric + shortName +
                                                     outcome,
                                                   data = .,
                                                   sum) %>%
  mutate(PAF = affected_i / popHessen / (1 + affected_i / popHessen) *
           100,
         .keep = "unused") %>%
  left_join(., data.frame(filter(
    burden_list, population ==
      "Hessians aged over 40 years"
  )),
  by = "outcome") %>%
  left_join(., dw_list, by = "outcome") %>%
  mutate(attrBurden = PAF * burden / 100) %>% 
  select(source, noiseMetric, shortName,outcome, PAF, attrBurden) %>% 
  mutate(dw = NA ,HAorHSD=NA)

streetNoiseBurden2<-er_list %>%
  filter(burdenCalculation == "total") %>%
  aggregate(affected_i ~ source + noiseMetric + shortName + outcome,
            data = .,
            sum) %>%
  left_join(., dw_list, by = "outcome") %>%
  mutate(HAorHSD = affected_i, .keep = "unused") %>%
  mutate(attrBurden = HAorHSD * dw,PAF=NA)
names(streetNoiseBurden1)
names(streetNoiseBurden2)
streetNoiseBurden <- rbind(streetNoiseBurden1,streetNoiseBurden2)
pl3<-ggplot(streetNoiseBurden,aes(x=shortName,xlab=NULL,y=100000*attrBurden/popHessen,fill=shortName))+
  theme(axis.text.x = element_blank())+
  geom_col()+
  facet_grid(cols = vars(source))+geom_text(label=streetNoiseBurden$shortName,size=3.5,angle=55)+
  xlab(NULL) + ylab("attributable Burden /100000")
pl3
ggsave("graphs/StreetNoiseBurden.pdf")
