rm(list = ls(all = TRUE))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Settings ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Libraries
library(tidyverse)
library(readxl)
library(arm)

# Load data
load("Data/bdm.RData")
load("Data/survBDM.RData")

# Load species list
species <- read_excel("Tables/Appendix_Species-List.xlsx") %>% 
  filter(bdm_n_squares >= 20)

# Name of the model type
runName <- "2025_BDM_v5"

# Load table with results from all models
resultate <- read_csv("Tables/results.csv", col_types = "ciddddddddic")  %>% 
  filter(runID != runName)

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
      occ = as.integer(n > 0),
      yearID = as.integer(year - 2002),
      siteID = as.factor(aID_STAO) %>% as.integer()
    ) 
  
  # Calculate yearly values
  mod <- glmer(occ ~ 1 + (1|year), data = dat, family = binomial)
  res <- fixef(mod) + ranef(mod)$year[,1]
  resultate <- resultate %>%
    rbind(
      tibble(
        runID = runName,
        NUESP = auswspec,
        year = 2003:2023,
        mittel = res,
        sd  = NA,
        lo  = NA,
        up  = NA,
        STI = 100 * plogis(res) / plogis(mean(res[1:5])),
        STI_lo = NA,
        STI_up = NA,
      )
    )
  print(s)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Save results ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
write_csv(resultate, file = "Tables/results.csv")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Overview of different runs ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

read_csv("Tables/results.csv", col_types = "ciddddddddic") %>% 
  group_by(runID) %>% 
  dplyr::summarise(
    Anz_Arten_alle = n_distinct(NUESP)
  )


