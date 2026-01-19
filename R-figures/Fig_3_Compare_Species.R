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

# Plot style
theme_set(
  theme_clean() +
    theme(
      plot.title.position = "plot", 
      plot.title = element_text(face = "plain", size = 14),
      plot.subtitle = element_text(face = "plain", size = 12),
      legend.title = element_blank(), 
      legend.direction = "vertical",
      legend.position = "right", 
      legend.background = element_rect(colour = "white"),
      legend.key.height = unit(2, "lines"),
      plot.background = element_blank())
)
options(ggplot2.discrete.colour= c("#1F78B4", "#FF7F00", "#33A02C", "#E31A1C", "#6A3D9A"))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Link zur BDM Datenbank
db <- DBI::dbConnect(RSQLite::SQLite(), "~/OneDrive - Hintermann + Weber AG/BDM_DB/DB_BDM_2025-06-02.db") 

# Load species list
species <- read_excel("Tables/Appendix_Species-List.xlsx") %>% 
  filter(bdm_n_squares >= 20) %>% 
  left_join(tbl(db, "Traits_TF") %>% transmute(aID_SP, Waermetyp, vagabundierend, Nutrient, EU_Grassland_Indicator), copy = TRUE) 

# Load model results
dat <- read_csv("Tables/results.csv", col_types = "ciddddddddic") %>% 
  filter(year >= 2003) %>% 
  filter(runID %in% c("2025_BDM_v5", "infospecies-raw", "infospecies-occupancy", 
                      "infospecies-reportingtyp", 
                      "infospecies-randomyear-GLM-occ", "2025_siteoccupancy_v2"))

# Rename runID for better readability
dat$runID <- factor(
  dat$runID, 
  levels = c("2025_BDM_v5", "infospecies-raw", "infospecies-occupancy", 
             "infospecies-reportingtyp", 
             "infospecies-randomyear-GLM-occ", "2025_siteoccupancy_v2"),
  labels = c("Structured BDM data", "Raw number of occupied squares", "Proportion of occupied squares", 
             "Proportion of occupied squares\nbased on reporting types", 
             "Random year GLMM of\noccupied squares", "Site-occupancy model"))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Make plot for selected species ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Helpfunction to make a plot for a species
getplot <- function(s, figcap) {
  print(species[species$NUESP == s,] %>% as.data.frame())
  dat %>% 
    filter(NUESP == s) %>% 
    ggplot(aes(x = year, y = STI, color = runID)) +
    geom_point() +
    geom_line() +
    # geom_ribbon(aes(ymin = STI_lo, ymax = STI_up), alpha = 0.2) +
    labs(
      title = paste(figcap, species$Species_Name[species$NUESP == s]),
      x = "",
      y = "Species Index\n[2003-2007 = 100]"
    ) +
    scale_color_brewer(palette = "Set2") +
    theme(legend.position = "bottom") +
    ylim(0, NA) +
    geom_hline(
      yintercept = 100, 
      color = "black", 
      linewidth = 0.5
    ) 
}

# Make plots for four selected species
p1 <- getplot(31092, "(a)") # Cupido argiades, thermophiler und mobiler Ausbreiter
p2 <- getplot(31191, "(b)") # Nymphalis antiopa, mobiler Ausbreiter, starke Schwankungen, überdurchschnittlich oft in Ornitho-Daten
p3 <- getplot(31044, "(c)") # Parnassius phoebus, Abnahme eines Kältezeigers
# p3 <- getplot(31080, "(c)") # Agriades glandon, Entwicklung eines Kältezeigers
p4 <- getplot(31119, "(d)") # Polyommatus dorylas, nährstoffarme Lebensräume
p5 <- getplot(31198, "(e)") # Euphydryas aurinia, Art des Grasland Indikators, nährstoffarme Standorte, abnehmend
p6 <- getplot(31058, "(f)") # Gonepteryx rhamni, häufige Art, die noch häufiger geworden ist, Art die am meisten zur Zunahme des Artenreichtums begetragen hat

# Combine plots in a grid layout with one legend
p1 + p2 + p3 + p4 + p5 + p6 +
  plot_layout(ncol = 3, guides = "collect") 
ggsave("Figures/Fig3-camparision-6-species.pdf", width = 12, height = 6)


