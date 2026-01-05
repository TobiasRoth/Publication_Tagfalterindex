rm(list = ls(all = TRUE))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Einstellungen ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Libraries
library(tidyverse)
library(readxl)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Resultate auslesen ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# File mit Resultaten einlesen
# resultate <- tibble()
resultate <- read_csv("Stanresults/resultate.csv", col_types = "ciddddddddic") 

# Ordner einlesen
ordner <- list.files("Stanresults/", full.names = FALSE) 
ordner <- ordner[ordner != "resultate.csv"]

for(o in ordner) {
  resultate <- resultate %>%  filter(runID != o)
  resfiles <- list.files(paste0("Stanresults/", o), full.names = TRUE, pattern = "Berech_")
  for (r in 1:length(resfiles)) {
    load(resfiles[r])
    resultate <- resultate %>% 
      rbind(
        tibble(
          runID = o,
          NUESP = res$auswspec,
          year = 1990:2023,
          mittel = apply(rstan::extract(res$fit)$a, 2, mean) %>% round(3),
          sd  = apply(rstan::extract(res$fit)$a, 2, sd) %>% round(3),
          lo  = apply(rstan::extract(res$fit)$a, 2, quantile, probs = 0.025) %>% round(3),
          up  = apply(rstan::extract(res$fit)$a, 2, quantile, probs = 0.975) %>% round(3),
          STI = round(100 * mittel %>% plogis / mean(mittel[14:18] %>% plogis), 2),
          STI_lo = round(100 * lo %>% plogis / mean(mittel[14:18] %>% plogis), 2),
          STI_up = round(100 * up %>% plogis / mean(mittel[14:18] %>% plogis), 2)
        )
      )
  }
  
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Resultat-File speichern ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# File speichern
write_csv(resultate, file = "Stanresults/resultate.csv")

# Übersicht
resultate %>% 
  group_by(runID) %>% 
  dplyr::summarise(
    Anz_Arten_alle = n_distinct(NUESP)
  )




