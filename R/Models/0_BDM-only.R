rm(list = ls(all = TRUE))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Settings ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Libraries
library(tidyverse)
library(readxl)
library(rstan)

# Load data
load("Data/bdm.RData")
load("Data/survBDM.RData")

# Load species list
species <- read_excel("Tables/Appendix_Species-List_v3.xlsx") %>% 
  filter(bdm_n_stao >= 20)

# Name of the model type
runName <- "2025_BDM_v3"

# Load table with results from all models
resultate <- read_csv("Tables/results.csv", col_types = "ciddddddddic") 
  # filter(runID != runName)

# Stan settings
options(mc.cores = 2)
rstan_options(auto_write = TRUE)

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
  standat <- list(
    nyear = max(dat$yearID),
    yearID = dat$yearID,
    nobs = nrow(dat),
    y = as.integer(dat$pre)
  )
  stanmod <- read_file("Models/bdm.stan")
  # fit <- stan(model_code = stanmod, data = standat, chains = 2, warmup = 10, iter = 20)   # Used for Setup
  fit <- stan(model_code = stanmod, data = standat, chains = 2)
  
  # Save results
  read_csv("Tables/results.csv", col_types = "ciddddddddic") %>% 
    filter(!(NUESP == auswspec & runID == runName)) %>% 
    rbind(
      tibble(
        runID = runName,
        NUESP = auswspec,
        year = 2003:2023,
        mittel = apply(rstan::extract(fit)$a, 2, mean) %>% round(3),
        sd  = apply(rstan::extract(fit)$a, 2, sd) %>% round(3),
        lo  = apply(rstan::extract(fit)$a, 2, quantile, probs = 0.025) %>% round(3),
        up  = apply(rstan::extract(fit)$a, 2, quantile, probs = 0.975) %>% round(3),
        STI = round(100 * mittel %>% plogis / mean(mittel[1:5] %>% plogis), 2),
        STI_lo = round(100 * lo %>% plogis / mean(mittel[1:5] %>% plogis), 2),
        STI_up = round(100 * up %>% plogis / mean(mittel[1:5] %>% plogis), 2)
      )
    ) %>% 
    write_csv(file = "Tables/results.csv")
  print(paste0("Art ", s, " (NUESP = ", auswspec, ") wurde berechnet"))
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Overview of different runs ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
resultate %>% 
  group_by(runID) %>% 
  dplyr::summarise(
    Anz_Arten_alle = n_distinct(NUESP)
  )


