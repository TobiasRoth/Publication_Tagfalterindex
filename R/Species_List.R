rm(list=ls(all=TRUE))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Settings ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Libraries
library(tidyverse)
library(readxl)

# Load data
load("Data/cscf.RData")
load("Data/bdm.RData")
load("Data/survBDM.RData")

# Connection to data base
db <- DBI::dbConnect(RSQLite::SQLite(), "~/OneDrive - Hintermann + Weber AG/BDM_DB/DB_BDM_2025-06-02.db")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Compile species list ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# BDM taxa list -> 252 taxa
species <- tbl(db, "Arten") %>%
  filter(TF == 1) %>% 
  as_tibble() %>% 
  transmute(
    NUESP = InfoSpeciesNr,
    aID_SP,
    aID_SP1,
    aID_SP_Komplex,
    valid = Z7Z9,
    Species_Name = paste(Gattung, Art)
  )

# Remove species never recorded in Switzerland -> 243 taxa
# Papilio	alexanor
# Zerynthia	polyxena
# Anthocharis	euphenoides
# Gonepteryx	cleopatra
# Polygonia	egea
# Coenonympha	dorus
# Pyronia	cecilia
# Zygaena	erythrus
# Zygaena	hilaris
species <- species %>% 
  filter(!(aID_SP %in% c(8174, 4977, 4753, 4843, 4930, 4793, 4960, 4980, 4984)))

# Add own NUESP
species$NUESP[species$aID_SP == 4749] <- 1
species$NUESP[species$aID_SP == 4861] <- 2

# Add further species information
species <- species %>% 
  left_join(
    rbind(
      read_excel("Data/Arten_Tagfalterindex.xlsx", sheet = "Taxa_CSCF"),
      read_excel("Data/Arten_Tagfalterindex.xlsx", sheet = "Zusätzliche_Taxa") %>% 
        transmute(
          NUESP,
          ESP = as.character(ESP),
          Export_CSCF,
          Berechnung_TFI,
          Meldetyp,
          `1990_Peak`,
          Trend_verwenden,
          Begründung
        )
    ) %>% 
      transmute(
        NUESP = as.integer(NUESP),
        Berechnung_TFI,
        Meldetyp,
        Peak_1990 = `1990_Peak`,
        Trend_verwenden
      )
  )

# Summary    
nrow(species)
n_distinct(species$NUESP)
n_distinct(species$aID_SP_Komplex) # -> 204 Species
sum(species$valid == 1)
sum(species$Berechnung_TFI == "ja") # -> 194 Species
sum(species$Trend_verwenden %in% c("ja", "BDM")) # -> 170 Species

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Anzahl Nachweise anhängen ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

species %>% 
  filter(valid == 1) %>%
  transmute(
    NUESP,
    aID_SP,
    Species_Name,
    Meldetyp,
    Berechnung_TFI,
    Trend_verwenden,
    Peak_1990
  ) %>% 
  left_join(
    cscf %>% 
      group_by(NUESP) %>% 
      dplyr::summarise(
        if_n_obs = sum(n),
        if_n_stao = n_distinct(aID_STAO),
        if_n_years = n_distinct(year)
      ) 
  ) %>% 
  left_join(
    bdm %>% 
      left_join(survBDM) %>%
      group_by(NUESP) %>% 
      dplyr::summarise(
        bdm_n_obs = sum(n),
        bdm_n_stao = n_distinct(aID_STAO),
        bdm_n_years = n_distinct(year)
      ) 
  ) %>% 
  replace_na(list(
    if_n_obs = 0,
    if_n_stao = 0,
    if_n_years = 0,
    bdm_n_obs = 0,
    bdm_n_stao = 0,
    bdm_n_years = 0
  )) %>% 
  openxlsx::write.xlsx("Tables/Appendix_Species-List_v3.xlsx")

