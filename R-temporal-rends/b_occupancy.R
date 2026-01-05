rm(list = ls(all = TRUE))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Settings ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Libraries
library(tidyverse)
library(Rmisc)
library(ggthemes)
library(readxl)
library(arm)

# Load data
load("Data/cscf.RData")

# Load species list
species <- read_excel("Tables/Appendix_Species-List_v3.xlsx") %>% 
  filter(bdm_n_stao >= 20)

# Name of the model type
runName <- "infospecies-occupancy"

# Load table with results from all models
resultate <- read_csv("Tables/results.csv", col_types = "ciddddddddic") %>% 
  filter(runID != runName)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Run calculation for all species ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

for(s in species$NUESP) {
  
  # Select species
  dat <- cscf %>% 
    group_by(year) %>%
    dplyr::summarise(
      nsites = n_distinct(aID_STAO),
      N = n_distinct(aID_STAO[NUESP == s]),
      occup = N / nsites
    )  

  # Calculate yearly values
  tmp <- tibble(
    runID = runName,
    NUESP = s,
    year = 1990:2023,
    mittel = NA,
    sd  = NA,
    lo  = NA,
    up  = NA,
    STI = NA,
    STI_lo = NA,
    STI_up = NA
  )
  ref <- mean(dat$occup[14:18]) 
  dat$Nindex <- 100 * dat$occup / ref
  tmp$mittel[match(dat$year, tmp$year)] <- dat$occup
  tmp$STI[match(dat$year, tmp$year)] <- dat$Nindex
  resultate <- resultate %>% rbind(tmp)
  print(s)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Save results ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
write_csv(resultate, file = "Tables/results.csv")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Overview of different runs ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
resultate %>% 
  group_by(runID) %>% 
  dplyr::summarise(
    Anz_Arten_alle = n_distinct(NUESP)
  )


