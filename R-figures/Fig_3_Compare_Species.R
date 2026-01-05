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
      plot.background = element_blank())
)
options(ggplot2.discrete.colour= c("#1F78B4", "#FF7F00", "#33A02C", "#E31A1C", "#6A3D9A"))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Load species list
species <- read_excel("Tables/Appendix_Species-List_v3.xlsx") %>% 
  filter(bdm_n_stao >= 20)

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
  labels = c("structured BDM data", "Raw number of observations", "Proportion of occupied squares", 
             "Proportion of occupied squares based on reporting types", 
             "Random year GLMm of occupied squares", "Site-occupancy model"))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Make plot for selected species ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Helpfunction to make a plot for a species
getplot <- function(s, figcap) {
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
      size = 0.5
    ) 
}

# Make plots for four selected species
p1 <- getplot(31121, "(a)")
p2 <- getplot(31191, "(b)")
p3 <- getplot(31091, "(c) ")
p4 <- getplot(31092, "(d)")

# Combine plots in a grid layout with one legend
p1 + p2 + p3 + p4 +
  plot_layout(ncol = 2, guides = "collect") 
ggsave("Figures/Fig3-camparision-4-species.pdf", width = 12, height = 6)


