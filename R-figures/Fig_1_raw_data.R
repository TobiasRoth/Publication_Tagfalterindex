rm(list = ls(all = TRUE))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Settings ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Libraries
library(tidyverse)
library(Rmisc)
library(ggthemes)
library(readxl)
library(patchwork)
library(sf)
library(raster)

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
# Read data and sources ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Read info fauna and bdm data
load("Data/cscf.RData")
load("Data/bdm.RData")
load("Data/survBDM.RData")
bdm <- bdm %>% left_join(survBDM) 

# Sources for distribution maps (not on github repository)
load("~/Dropbox/3_Ressourcen/Datengrundlagen/Kartengrundlagen/ch.RData")
load("~/Dropbox/3_Ressourcen/Datengrundlagen/Kartengrundlagen/kantone.RData")
load("~/Dropbox/3_Ressourcen/Datengrundlagen/Kartengrundlagen/gewaesser.RData")

# Connection to data base (not on github repository)
db <- DBI::dbConnect(RSQLite::SQLite(), "~/OneDrive - Hintermann + Weber AG/BDM_DB/DB_BDM_2025-06-02.db")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Some stats of the studey site and the two datasets ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Regions of Switzerland
tt <- st_read("~/OneDrive - Hintermann + Weber AG/Datengrundlagen/BGR/BGR.gpkg")
round(100 * sum(tt %>% filter(DERegionNa == "Mittelland") %>% st_area()) / (41285 * 1000^2) ,1)
round(100 * sum(tt %>% filter(DERegionNa == "Jura") %>% st_area()) / (41285 * 1000^2) ,1)
100 - 27.3 - 10.1

# Info fauna
nrow(cscf)
n_distinct(cscf$aID_STAO)
n_distinct(cscf$aID_STAO) / 41285
n_distinct(cscf$NUESP) 

# Number of species per square and year
tmp <- cscf %>% 
  group_by(aID_KD) %>% 
  dplyr::summarise(SR = n_distinct(NUESP))
mean(tmp$SR)
sd(tmp$SR)

# BDM 
nrow(bdm)
n_distinct(survBDM$aID_STAO)
n_distinct(survBDM$aID_STAO) / 41285
n_distinct(bdm$NUESP)
nrow(survBDM)

# Number of species per square 
tmp <- survBDM %>% 
  left_join(
    bdm %>% 
      group_by(aID_KD) %>% 
      dplyr::summarise(SR = n_distinct(NUESP))
  ) 
mean(tmp$SR)
sd(tmp$SR)

# Elevational range 
tmp <- tibble(aID_STAO = as.integer(unique(survBDM$aID_STAO))) %>% 
  left_join(
    tbl(db, "Raumdaten_Z7") %>% 
      transmute(aID_STAO, Hoehe) %>% 
      as_tibble()
  ) 
range(tmp$Hoehe)
mean(tmp$Hoehe)
sd(tmp$Hoehe)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Temporal trends in the number of observations ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# info fauna
nachwif <- cscf %>% 
  group_by(year) %>% 
  dplyr::summarise(n = sum(n, na.rm = TRUE)) %>% 
  replace_na(list(n = 0)) %>%
  ggplot(aes(x = year, y = n)) +
  geom_point() +
  geom_line() +
  labs(
    title = "Info fauna",
    x = "",
    y = "Number of observations",
  ) +
  xlim(1990, 2023) +
  ylim(0, 200000) +
  theme(plot.title = element_text(hjust = 0.5))
nachwif
ggsave("Figures/Fig1a-Trend-observations-infofauna.pdf", width = 4, height = 3)

# BDM
nachwbdm <- bdm %>% 
  group_by(year) %>% 
  dplyr::summarise(n = sum(n, na.rm = TRUE)) %>% 
  replace_na(list(n = 0)) %>%
  ggplot(aes(x = year, y = n)) +
  geom_point() +
  geom_line() +
  labs(
    title = "BDM",
    x = "",
    y = "Number of observations",
  ) +
  xlim(1990, 2023) +
  ylim(0, 200000) +
  theme(plot.title = element_text(hjust = 0.5))
nachwbdm
ggsave("Figures/Fig1b-Trend-observations-bdm.pdf", width = 4, height = 3)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Map with number of observation per 1km-grid   ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# info fauna
r <- cscf %>% 
  group_by(aID_STAO) %>% 
  dplyr::summarise(
    SR = n_distinct(NUESP),
    nobs = sum(n, na.rm = TRUE)
  ) %>% 
  mutate(
    x = str_sub(aID_STAO, 1, 3) %>% as.numeric() * 1000 + 500,
    y = str_sub(aID_STAO, 4, 6) %>% as.numeric() * 1000 + 500,
  ) %>% 
  dplyr::select(x, y, nobs, SR) 

mapif <- ggplot() +
  geom_raster(data = r, aes(x = x, y = y, fill = nobs)) + 
  geom_sf(data = ch, fill = "transparent", color = "black", size = 0.2) +
  scale_fill_viridis_c(
    trans = "log10",
    option = "viridis", 
    name = "Number of observations", 
    limits = c(1, 10000),
    breaks = c(1, 10, 100, 1000, 10000),    
    labels = c("1", "10", "100", "1'000", "10'000"),
    direction = -1,
    guide = guide_colorbar(
      title.position = "top",      
      title.hjust = 1,            
      barwidth = unit(3, "cm"),     
      barheight = unit(0.2, "cm") 
    )
  ) + 
  geom_sf(data = gewaesser, fill = "#D6F1FF", color = "darkblue") +
  labs(x = "", y = "") +
  theme_map() +
  theme(
    # legend.position = "bottom",        # unten rechts
    legend.position = c(0, 0.85),        # unten rechts
    legend.direction = "horizontal",        # waagrecht
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7)
  )
mapif
ggsave("Figures/Fig1c-Map-observations-infofauna.pdf", width = 5, height = 4)

# BDM
r <- bdm %>% 
  group_by(aID_STAO) %>% 
  dplyr::summarise(
    SR = n_distinct(NUESP),
    nobs = sum(n, na.rm = TRUE)
  ) %>% 
  mutate(
    x = str_sub(aID_STAO, 1, 3) %>% as.numeric() * 1000 + 500,
    y = str_sub(aID_STAO, 4, 6) %>% as.numeric() * 1000 + 500,
  ) %>% 
  dplyr::select(x, y, nobs, SR) 

mapbdm <- ggplot() +
  geom_point(data = r, aes(x = x, y = y, fill = nobs), col = "transparent", pch = 22, size = 0.9) + 
  geom_sf(data = ch, fill = "transparent", color = "black", size = 0.2) +
  scale_fill_viridis_c(
    trans = "log10",
    option = "viridis", 
    name = "Number of observations", 
    limits = c(1, 10000),
    breaks = c(1, 10, 100, 1000, 10000),    
    labels = c("1", "10", "100", "1'000", "10'000"),
    direction = -1,
    guide = guide_colorbar(
      title.position = "top",      
      title.hjust = 1,            
      barwidth = unit(3, "cm"),     
      barheight = unit(0.2, "cm") 
    )
  ) + 
  geom_sf(data = gewaesser, fill = "#D6F1FF", color = "darkblue") +
  labs(x = "", y = "") +
  theme_map() +
  theme(
    # legend.position = "bottom",        # unten rechts
    legend.position = c(0, 0.85),        # unten rechts
    legend.direction = "horizontal",        # waagrecht
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7)
  )
mapbdm
ggsave("Figures/Fig1d-Map-observations-bdm.pdf", width = 5, height = 4)
