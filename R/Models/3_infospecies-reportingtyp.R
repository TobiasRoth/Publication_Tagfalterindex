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
# spec <- read_excel("Tables/Appendix_Species-List_v1.xlsx") 
# spec <- read_excel("Tables/2000 bearb Pl#Appendix_Species-List_v1.xlsx") 
# spec$Meldetyp[!is.na(spec$Ausbreiter_e1)] <- "Ausbreiter"
spec <- read_excel("Tables/Appendix_Species-List_v2.xlsx") 
spec$Meldetyp <- spec$Meldetyp_v2
species <- spec %>% filter(bdm_n_stao >= 20)

# Name of the model type
# runName <- "infospecies-raw-occupancy-reportingtyp"
# runName <- "infospecies-raw-occupancy-increasingspecies"
runName <- "infospecies-raw-occupancy-reportingtyp_v2"

# Load table with results from all models
resultate <- read_csv("Stanresults/resultate.csv", col_types = "ciddddddddic") %>% 
  filter(runID != runName)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Run calculation for all species ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

for(s in species$NUESP) {
  
  # Select data from same "reporting typ"
  meldetyp <- species$Meldetyp[species$NUESP == s]
  ausw <- spec$Meldetyp[match(cscf$NUESP, spec$NUESP)] == meldetyp
  ausw[is.na(ausw)] <- FALSE
  cscf_ausw <- cscf[ausw, ]
  
  # Calculate yearly values
  dat <- cscf_ausw %>% 
    group_by(year) %>%
    dplyr::summarise(
      nsites = n_distinct(aID_STAO),
      N = n_distinct(aID_STAO[NUESP == s]),
      occup = N / nsites
    )  

  # Save results in `resultate`
  ref <- dat %>% pull(occup) %>% mean() 
  dat$Nindex <- 100 * dat$occup / ref
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
  tmp$mittel[match(dat$year, tmp$year)] <- dat$occup
  tmp$STI[match(dat$year, tmp$year)] <- dat$Nindex
  resultate <- resultate %>% rbind(tmp)
  print(s)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Save results ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
write_csv(resultate, file = "Stanresults/resultate.csv")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Overview of different runs ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
resultate %>% 
  group_by(runID) %>% 
  dplyr::summarise(
    Anz_Arten_alle = n_distinct(NUESP)
  )


