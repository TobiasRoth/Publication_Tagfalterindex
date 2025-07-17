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

# Load model results and select relvant models
dat <- read_csv("Stanresults/resultate.csv", col_types = "ciddddddddic") %>% 
  filter(runID %in% c("BDM-only", "infospecies-raw", "infospecies-raw-occupancy", 
                        "infospecies-raw-occupancy-reportingtyp", 
                        "infospecies-raw-randomyear-GLM", "siteoccupancy-2sources")) %>% 
  filter(year >= 2003)

# Rename runID for better readability
dat$runID <- factor(
  dat$runID, 
  levels = c("BDM-only", "infospecies-raw", "infospecies-raw-occupancy", 
             "infospecies-raw-occupancy-reportingtyp", 
             "infospecies-raw-randomyear-GLM", "siteoccupancy-2sources"),
  labels = c("BDM only", "Raw data", "Occupancy", 
             "Occupancy + reporting type", 
             "Random year GLM", "Site occupancy model"))


# Load species list
species <- read_excel("Tables/Appendix_Species-List_v1.xlsx") %>% 
  filter(bdm_n_stao >= 20)

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
      title = paste(figcap, species$ESP[species$NUESP == s]),
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
p1 <- getplot(31125, "(a)")
p2 <- getplot(31121, "(b)")
p3 <- getplot(31191, "(c)")
p4 <- getplot(31092, "(d)")

# Combine plots in a grid layout with one legend
p1 + p2 + p3 + p4 +
  plot_layout(ncol = 2, guides = "collect") 
ggsave("Figures/Fig3-camparision-4-species.pdf", width = 12, height = 6)


