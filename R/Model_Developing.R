rm(list=ls(all=TRUE))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Settings ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Libraries
library(tidyverse)
library(readxl)
library(rstan)
library(ggthemes)
library(arm)
library(brms)

# Load data
load("Data/cscf.RData")
load("Data/bdm.RData")
load("Data/survBDM.RData")

# Load species list 
species <- read_excel("Tables/Appendix_Species-List_v1.xlsx") 

# Load table with results from all models
resultate <- read_csv("Stanresults/resultate.csv", col_types = "ciddddddddic")

# Study period
start_yr <- as.integer(1990)
end_yr <- as.integer(2023)

# Stan settings
options(mc.cores = 2)
rstan_options(auto_write = TRUE)

# Plot Einstellungen
theme_set(
  theme_clean() +
    theme(
      legend.title = element_blank(), 
      legend.position = "right", 
      legend.background = element_rect(colour = "white"),
      plot.background = element_blank())
)
options(ggplot2.discrete.colour= c("#1F78B4", "#FF7F00", "#33A02C", "#E31A1C", "#6A3D9A"))

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
        # dplyr::summarise(nrep = sum(n)) %>%
        dplyr::summarise(nrep = max(n)) %>%
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
# Model entwickeln und testen ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Art auswählen und Daten bereitstellen
# view(species)
# s <- 185 # Maniola jurtina
# s <- 61  # Cupido argiades
# s <- 186  # Melanargia galathea
# s <- 60  # Cupido alcetas
s <- 49 # Pieris napi aggr
auswspec <- species$NUESP[s] 
meldetyp <- species$Meldetyp[s]
bdm_ausw <- bdm
ausw <- species$Meldetyp[match(cscf$NUESP, species$NUESP)] == meldetyp
ausw[is.na(ausw)] <- FALSE
cscf_ausw <- cscf[ausw, ]
dat <- getspecdat(
  auswspec = auswspec, 
  bdm = bdm_ausw, 
  cscf = cscf_ausw
)

dat <- dat %>% filter(program != "Ornitho")
table(dat$program)
dat$program[dat$program != "BDM" & dat$program != "Ornitho"] <- "CSCF" 
standat <- getstandat(dat)

# Modell laufen lassen
stanmod <- read_file("Models/sources.stan")
# stanmod <- read_file("Models/sourcesyearlyP.stan")
# fit <- stan(model_code = stanmod, data = standat, chains = 2, warmup = 10, iter = 20)   # Used for Setup
fit <- stan(model_code = stanmod, data = standat, chains = 2)

# BDM-only Modell neu laufen lassen
bdmdat <- survBDM %>% 
  left_join(bdm %>% filter(NUESP == auswspec) %>% dplyr::select(aID_KD, n)) %>% 
  replace_na(list(n = 0)) %>% 
  mutate(pre = as.integer(n > 0), yr = as.factor(year))
# bdmmod <- glmer(
#   pre ~ (1|year), 
#   family = binomial,
#   data = bdmdat
# )
# bdmres <- fixef(bdmmod) + ranef(bdmmod)$year[,1]
# bdmres <- tapply(bdmdat$pre, bdmdat$year, mean)

# Mit brms
# bdmmod2<- brm(
#   pre ~ (1|year), 
#   family = bernoulli,
#   data = bdmdat
# )
# bdmres <- fixef(bdmmod2)["Intercept", "Estimate"] + ranef(bdmmod2)$year[,,][,"Estimate"]

# Mit STAN
specdat <- bdmdat %>% 
  mutate(yearID = as.integer(year - 2002),
         siteID = as.factor(aID_STAO) %>% as.integer()) 
standat <- list(
  nyear = max(specdat$yearID),
  yearID = specdat$yearID,
  nobs = nrow(specdat),
  y = as.integer(specdat$pre)
)
stanmodbdm <- read_file("Models/bdm.stan")
# bdmfit <- stan(model_code = stanmodbdm, data = standat, chains = 2, warmup = 10, iter = 20)   # Used for Setup
bdmfit <- stan(model_code = stanmodbdm, data = standat, chains = 2, iter = 5000)

# Grafik mit Resultaten machen
resultate %>% 
  filter(NUESP == auswspec) %>% 
  filter(runID %in% c("siteoccupancy-2sources")) %>% 
  rbind(
    tibble(
      runID = "BDM-only",
      NUESP = auswspec,
      year = 2003:2023,
      mittel = apply(rstan::extract(bdmfit)$a, 2, mean) %>% round(3),
      sd  = apply(rstan::extract(bdmfit)$a, 2, sd) %>% round(3),
      lo  = apply(rstan::extract(bdmfit)$a, 2, quantile, probs = 0.025) %>% round(3),
      up  = apply(rstan::extract(bdmfit)$a, 2, quantile, probs = 0.975) %>% round(3),
      STI = round(100 * mittel %>% plogis / mean(mittel[1:5] %>% plogis), 2),
      STI_lo = round(100 * lo %>% plogis / mean(mittel[1:5] %>% plogis), 2),
      STI_up = round(100 * up %>% plogis / mean(mittel[1:5] %>% plogis), 2),
    ),
    # tibble(
    #   runID = "BDM-only",
    #   NUESP = auswspec,
    #   year = 2003:2023,
    #   mittel = bdmres,
    #   sd  = NA,
    #   lo  = NA,
    #   up  = NA,
    #   STI = 100 * plogis(bdmres) / plogis(mean(bdmres[1:5])),
    #   # STI = 100 * bdmres / mean(bdmres[1:5]),
    #   STI_lo = NA,
    #   STI_up = NA
    # ),
    tibble(
      runID = "Neues Modell",
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
  ) %>% 
  mutate(
    runID = factor(runID,
      levels = c("BDM-only", "siteoccupancy-2sources", "Neues Modell"),
      labels = c("BDM only", "Site occupancy", "Site occupancy new"))
    ) %>% 
  ggplot(aes(x = year, y = STI, col = runID)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 100, linetype = "dashed") +
  labs(
    title = paste(species$ESP[s]),
    subtitle = "Neues Model: 3 Sources, BDM mit STAN berechnet."
  ) +
  ylim(0, NA)


