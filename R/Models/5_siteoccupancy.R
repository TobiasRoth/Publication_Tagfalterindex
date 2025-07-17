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
load("Data/bdm.RData")
load("Data/survBDM.RData")

# Load species list 
# species <- read_csv("Data/species.csv") %>% 
#   filter(Berechnung_TFI == "ja")
species <- read_excel("Tables/Appendix_Species-List_v1.xlsx")
  # filter(bdm_n_stao >= 20)

# Name of the model type
# runName <- "siteoccupancy-sources"
runName <- "siteoccupancy-2sources"

# Load table with results from all models
resultate <- read_csv("Stanresults/resultate.csv", col_types = "ciddddddddic") 

# Load model
stanmod <- read_file("Models//sources.stan")

# Study period
start_yr <- as.integer(1990)
end_yr <- as.integer(2023)

# Stan settings
options(mc.cores = 2)
rstan_options(auto_write = TRUE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Functions ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Function to prepare data for one species
getspecdat <- function(auswspec, bdm, cscf) {
  
  # BDM data
  survBDM %>%
    transmute(aID_KD, aID_STAO, year, program = "BDM", nrep) %>%
    left_join(
      bdm %>%
        filter(NUESP == auswspec) %>%
        dplyr::select(aID_KD, n)) %>%
    filter(year >= start_yr & year <= end_yr) %>%
    replace_na(list(n = 0)) %>%
    ungroup() %>%

    # CSCF data
    rbind(
      cscf %>% group_by(aID_KD, aID_STAO, year, program) %>%
        dplyr::summarise(nrep = sum(n)) %>%
        left_join(
          cscf %>%
            ungroup() %>%
            filter(NUESP == auswspec) %>%
            dplyr::select(aID_KD, program, n)) %>%
        filter(year >= start_yr & year <= end_yr) %>%
        replace_na(list(n = 0)) %>% 
        ungroup()) 
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

for(s in 1:nrow(species)) {
    # for(s in 1:3) {
  
  auswspec <- species$NUESP[s]

  # Select data from same "Meldetyp"
  meldetyp <- species$Meldetyp[s]
  bdm_ausw <- bdm
  ausw <- species$Meldetyp[match(cscf$NUESP, species$NUESP)] == meldetyp
  ausw[is.na(ausw)] <- FALSE
  cscf_ausw <- cscf[ausw, ]

  # Run stan
  dat <- getspecdat(
    auswspec = auswspec, 
    bdm = bdm_ausw, 
    cscf = cscf_ausw
  )
  
  dat$program[dat$program != "BDM"] <- "CSCF" # DIESE ZEILE LÖSCHEN FÜR 6 SOURCES!!
  
  standat <- getstandat(dat)
  # fit <- stan(model_code = stanmod, data = standat, chains = 2, warmup = 10, iter = 20)   # Used for Setup
  fit <- stan(model_code = stanmod, data = standat, chains = 2)
  
  # Save results in Results
  resultate <- resultate %>% 
    filter(!(NUESP == auswspec & runID == runName)) %>% 
    rbind(
      tibble(
        runID = runName,
        NUESP = auswspec,
        year = 1990:2023,
        mittel = apply(rstan::extract(fit)$a, 2, mean) %>% round(3),
        sd  = apply(rstan::extract(fit)$a, 2, sd) %>% round(3),
        lo  = apply(rstan::extract(fit)$a, 2, quantile, probs = 0.025) %>% round(3),
        up  = apply(rstan::extract(fit)$a, 2, quantile, probs = 0.975) %>% round(3),
        STI = round(100 * mittel %>% plogis / mean(mittel[14:18] %>% plogis), 2),
        STI_lo = round(100 * lo %>% plogis / mean(mittel[14:18] %>% plogis), 2),
        STI_up = round(100 * up %>% plogis / mean(mittel[14:18] %>% plogis), 2)
      )
    )
  write_csv(resultate, file = "Stanresults/resultate.csv")
  print(paste("Art", s, "wurde berechnet"))
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Overview of different runs ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
resultate %>% 
  group_by(runID) %>% 
  dplyr::summarise(
    Anz_Arten_alle = n_distinct(NUESP)
  )

