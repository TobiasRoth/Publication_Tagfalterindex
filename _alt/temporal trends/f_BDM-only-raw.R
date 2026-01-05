rm(list = ls(all = TRUE))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Settings ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Libraries
library(tidyverse)
library(readxl)

# Load data
load("Data/bdm.RData")
load("Data/survBDM.RData")

# Load species list
species <- read_excel("Tables/Appendix_Species-List_v3.xlsx") %>% 
  filter(bdm_n_stao >= 20)

# Name of the model type
runName <- "2025_BDM_v4"

# Load table with results from all models
resultate <- read_csv("Tables/results.csv", col_types = "ciddddddddic") 

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Run calculation for all species ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

for(s in 1:nrow(species)) {
  
  auswspec <- species$NUESP[s] 
  
  # Prepare species data
  dat <- survBDM %>% 
    left_join(bdm %>% filter(NUESP == auswspec) %>% dplyr::select(aID_KD, n)) %>% 
    replace_na(list(n = 0)) %>% 
    mutate(
      pre = as.integer(n > 0),
      yearID = as.integer(year - 2002),
      siteID = as.factor(aID_STAO) %>% as.integer()
    ) 
  meanocc <- tapply(dat$pre, dat$year, mean)
  
  # Save results
  read_csv("Tables/results.csv", col_types = "ciddddddddic") %>% 
    filter(!(NUESP == auswspec & runID == runName)) %>% 
    rbind(
      tibble(
        runID = runName,
        NUESP = auswspec,
        year = 2003:2023,
        mittel = meanocc %>% round(3),
        sd  = as.numeric(NA),
        lo  = as.numeric(NA),
        up  = as.numeric(NA),
        STI = round(100 * mittel / mean(mittel[1:5]), 2),
        STI_lo = as.numeric(NA),
        STI_up = as.numeric(NA)
      )
    ) %>% 
    write_csv(file = "Tables/results.csv")
  print(paste0("Art ", s, " (NUESP = ", auswspec, ") wurde berechnet"))
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Overview of different runs ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

read_csv("Tables/results.csv", col_types = "ciddddddddic") %>% 
  group_by(runID) %>% 
  dplyr::summarise(
    Anz_Arten_alle = n_distinct(NUESP)
  )


