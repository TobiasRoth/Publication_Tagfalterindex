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

# Connection to data base
db <- DBI::dbConnect(RSQLite::SQLite(), "~/OneDrive - Hintermann + Weber AG/BDM_DB/DB_BDM_2025-02-04.db")

# Load species list 
species <- read_excel("Tables/Appendix_Species-List_v1.xlsx") 

# Load table with results from all models
resultate <- read_csv("Tables/results.csv", col_types = "ciddddddddic")

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
# s <- 49 # Pieris napi aggr
s <- 147 # Brintesia circe
# s <- 33 # Papilio machaon


# Flächen der Kantonalen Prgramme entfernen
# ausw <- tbl(db, "Raumdaten_Z7") %>% pull(aID_STAO)
# cscf <- cscf %>% filter(!(aID_STAO %in% ausw))

cscf$program[cscf$program != "BDM" & cscf$program != "Ornitho"] <- "CSCF"
# cscf$program[cscf$program != "BDM"] <- "CSCF"
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

# Höhe anhängen
dat <- dat %>% 
  mutate(
    KX = paste0(str_sub(aID_STAO, 1, 3), "500") %>% as.integer(),
    KY = paste0(str_sub(aID_STAO, 4, 6), "500") %>% as.integer()
  ) %>% 
  left_join(
    read_csv2("~/OneDrive - Hintermann + Weber AG/Datengrundlagen/Gelaendedaten/Gelaendedaten.CSV.zip") %>% 
      transmute(
        KX = as.integer(KX),
        KY = as.integer(KY),
        HOEHE
      )
  )
dat <- dat %>% 
  filter(HOEHE > quantile(dat$HOEHE[dat$n > 0], probs = 0.05)) %>% 
  filter(HOEHE < quantile(dat$HOEHE[dat$n > 0], probs = 0.95))

# dat <- dat %>% filter(program != "Ornitho")
table(dat$program)
# dat$program[dat$program != "BDM" & dat$program != "Ornitho"] <- "CSCF" 
# dat$program[dat$program != "BDM"] <- "CSCF"
standat <- getstandat(dat)

# Modell laufen lassen
# stanmod <- read_file("Models/siteoccupancy_LM_new.stan")
stanmod <- read_file("Models/siteoccupancy.stan")
# fit <- stan(model_code = stanmod, data = standat, chains = 2, warmup = 10, iter = 20)   # Used for Setup
# fit <- stan(model_code = stanmod, data = standat, chains = 2)
fit <- stan(model_code = stanmod, data = standat, chains = 2, warmup = 500, iter = 1000)   # Used for Setup


# Grafik mit Resultaten machen
resultate %>% 
  filter(NUESP == auswspec) %>% 
  filter(runID %in% c("2025_siteoccupancy_v1", "2025_BDM_v3", "2025_BDM_v4")) %>% 
  rbind(
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
      levels = c("2025_BDM_v3", "2025_BDM_v4", "2025_siteoccupancy_v1", "Neues Modell"),
      labels = c("BDM (glm)", "BDM (roh)", "Site occupancy v1", "Site occupancy new"))
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


dat %>% 
  group_by(year, program) %>% 
  dplyr::summarise(occ = mean(n > 0)) %>% 
  ggplot(aes(x = year, y = occ, col = program)) +
  geom_point() +
  geom_line()



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Drei Sources plotten ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

for(s in 1:nrow(species)) {
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
  dat$program[dat$program != "BDM" & dat$program != "Ornitho"] <- "CSCF" 

  
  dat %>% 
    group_by(year, program) %>% 
    # dplyr::summarise(occ = mean(n / nrep)) %>%
    dplyr::summarise(occ = mean(n > 0)) %>%
    ggplot(aes(x = year, y = occ, col = program)) +
    geom_point() +
    geom_line() +
    geom_smooth(method = lm) +
    labs(title = paste(species$ESP[s])) 
  ggsave(paste("Figures_rawtrends/", species$ESP[s], ".pdf"), width = 6, height = 3)
  
}


Sys.time() 




