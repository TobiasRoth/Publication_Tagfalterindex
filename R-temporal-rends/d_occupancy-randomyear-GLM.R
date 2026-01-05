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
spec <- read_excel("Tables/Appendix_Species-List_v3.xlsx") 
species <- spec %>% filter(bdm_n_stao >= 20)

# Name of the model type
runName <- "infospecies-randomyear-GLM-occ"

# Load table with results from all models
resultate <- read_csv("Tables/results.csv", col_types = "ciddddddddic") %>% 
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
  
  # Prepare data for model
  dat <- cscf_ausw %>% 
    group_by(year = factor(year, levels = 1990:20023), aID_STAO) %>%
    dplyr::summarise(
      occ = as.integer(sum(NUESP == s) > 0)
    ) 
  
  # Calculate yearly values
  mod <- glmer(occ ~ -1 + (1|year), data = dat, family = binomial)
  res <- ranef(mod)$year[,1]
  resultate <- resultate %>%
    rbind(
      tibble(
        runID = runName,
        NUESP = s,
        year = 1990:2023,
        mittel = res,
        sd  = NA,
        lo  = NA,
        up  = NA,
        STI = 100 * plogis(res) / plogis(mean(res[14:18])),
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
resultate %>% 
  group_by(runID) %>% 
  dplyr::summarise(
    Anz_Arten_alle = n_distinct(NUESP)
  )


