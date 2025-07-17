rm(list=ls(all=TRUE))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Settings ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Libraries
library(tidyverse)
library(readxl)
library(rstan)

# Load data
load("Data/cscf.RData")
load("Data/survBDM.RData")

# Remove BDM squares
staobdm <- survBDM$aID_STAO %>% unique()
cscf <- cscf %>% 
  filter(!aID_STAO %in% staobdm) %>%
  mutate(aID_KD = as.character(aID_KD))

# Load species list 
species <- read_csv("Data/species.csv") %>% 
  filter(Berechnung_TFI == "ja")

# Load model
stanmod <- read_file("Models//standard.stan")

# Study period
start_yr <- as.integer(1990)
end_yr <- as.integer(2023)

# Settings
options(mc.cores = 2)
rstan_options(auto_write = TRUE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Functions ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Function to prepare data for one species
getspecdat <- function(auswspec, cscf) {
  cscf %>% group_by(aID_KD, aID_STAO, year, program) %>%
    dplyr::summarise(nrep = sum(n)) %>%
    left_join(
      cscf %>%
        ungroup() %>%
        filter(NUESP == auswspec) %>%
        dplyr::select(aID_KD, program, n)) %>%
    filter(year >= start_yr & year <= end_yr) %>%
    replace_na(list(n = 0)) %>% 
    ungroup()
}

# Function to prepare data for Rstan
getstandat <- function(specdat) {
  specdat <- specdat %>% 
    mutate(yearID = as.integer(year - (start_yr - 1)),
           siteID = as.factor(aID_STAO) %>% as.integer()) 
  list(
    nyear = max(specdat$yearID),
    yearID = specdat$yearID,
    nsite = max(specdat$siteID),
    siteID = specdat$siteID,
    nobs = nrow(specdat),
    y = as.integer(specdat$n),
    nrep = specdat$nrep,
    nsources = n_distinct(specdat$program),
    program = factor(specdat$program) %>% as.integer,
    x = as.integer(specdat$n > 0)
  )
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Run stan ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# for(s in 1:nrow(species)) {
for(s in 1:10)  {
# for(s in 1:3) {
  
  auswspec <- species$NUESP[s]

  # Select data from same "Meldetyp"
  meldetyp <- species$Meldetyp[s]
  ausw <- species$Meldetyp[match(cscf$NUESP, species$NUESP)] == meldetyp
  ausw[is.na(ausw)] <- FALSE
  cscf_ausw <- cscf[ausw, ]

  # Run stan
  dat <- getspecdat(
    auswspec = auswspec, 
    cscf = cscf_ausw
  )
  
  standat <- getstandat(dat)
  # fit <- stan(model_code = stanmod, data = standat, chains = 2, warmup = 10, iter = 20)   # Used for Setup
  fit <- stan(model_code = stanmod, data = standat, chains = 2)
  
  # Save results
  res <- list(s= s, auswspec = auswspec, fit = fit)
  filename <- paste0("Stanresults/withoutBDM/Berech_", auswspec, ".RData")
  save(res, file = filename)
  print(paste("Art", s, "wurde berechnet"))
}

