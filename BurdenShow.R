library(readr)
library(dplyr)
library(tidyverse)
library(purrr)
dw_list <- read_delim(
  "rawdata/Dosis_Wirkungs_Studien_ver2.csv",
  ";",
  escape_double = FALSE,
  locale = locale(date_names = "de", decimal_mark = ","),
  trim_ws = TRUE
)
test_LDEN <- read_delim(
  "rawdata/Exponierte_Test_LDEN.csv",
  ";",
  escape_double = FALSE,
  locale = locale(date_names = "de",
                  decimal_mark = ","),
  trim_ws = TRUE
) %>% mutate(noiseMetric = "LDEN", source = "test")

END_LDEN <- read_delim(
  "rawdata/Exponierte_ULR_LDEN.csv",
  ";",
  escape_double = FALSE,
  locale = locale(date_names = "de",
                  decimal_mark = ","),
  trim_ws = TRUE
) %>% mutate(noiseMetric = "LDEN", source = "END_LDEN")

END_Lnight <- read_delim(
  "rawdata/Exponierte_ULR_Lnight.csv",
  ";",
  escape_double = FALSE,
  locale = locale(date_names = "de",
                  decimal_mark = ","),
  trim_ws = TRUE
) %>% mutate(noiseMetric = "Lnight", source = "END_night")

exposure_list <-
  rbind(test_LDEN, END_LDEN, END_Lnight) %>% mutate(ExposedPerdB = Exposed /
                                                      (Hi - Lo))

auswertestellen <- seq(40, 80, 5)
fEND <-
  approxfun(END_Lnight$Lo,
            END_Lnight$Exposed,
            method = "constant",
            0,
            0,
            f = 0)
ENDdata <-
  data.frame(L = auswertestellen, exposed = fEND(auswertestellen))

pl1 <-
  ggplot(exposure_list, aes(x = Lo, y = ExposedPerdB, color = source))
pl1 + geom_step(direction = "hv") + #waagrechter Strich bis zum nächsten Pegelwert, dann gerade runter
  facet_grid(cols = vars(noiseMetric)) +
  geom_segment(aes(xend = Hi, yend = ExposedPerdB)) #geom_segment, um noch das letzte waagrechte hinzuzufügen. Alleine wären keine senkrechten Striche


short_l <- dw_list %>% filter(included == TRUE) %>%
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
    comment,
    "ICD-10"
  )

j = 1
v <- short_l[j,]
pegel = 10
v$quadraticTerm

rm(v, pegel)
quadr(auswertestellen, v)
poly(auswertestellen, v)


linFkt = function(pegel, v) {
  as.numeric(v["rrPer10dB"]) * pegel - as.numeric(v["counterfactualValue"]) /
    as.numeric(v["rrPer10dB"])
}
quadr = function(pegel, v) {
  as.numeric(v["quadraticTerm"]) * pegel ^ 2 + as.numeric(v["linearTerm"]) *
    pegel + as.numeric(v["constantTerm"])
}

poly = function(pegel, v) {
  data.frame(L = pegel,
             effectSize = ifelse(is.na(v["quadraticTerm"]),
                                 quadr(pegel, v),
                                 quadr(pegel, v)))
}
dwf_data <- c()
for (j in 1:nrow(short_l)) {
  dwf_data <-
    poly(auswertestellen, short_l[j,]) %>% mutate(short_l[j, c("shortName", "noiseMetric")]) %>% bind_rows(dwf_data)
}

pl2 <-
  ggplot(dwf_data, aes(x = L, y = effectSize, color = shortName))
pl2 + geom_line() + facet_grid(cols = vars(noiseMetric))
