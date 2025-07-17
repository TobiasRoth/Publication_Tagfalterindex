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
      legend.position = "right", 
      legend.background = element_rect(colour = "white"),
      plot.background = element_blank())
)
options(ggplot2.discrete.colour= c("#1F78B4", "#FF7F00", "#33A02C", "#E31A1C", "#6A3D9A"))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Load model results
dat <- read_csv("Stanresults/resultate.csv", col_types = "ciddddddddic") %>% 
  filter(year >= 2003)

# Load species list
species <- read_excel("Tables/Appendix_Species-List_v1.xlsx") %>% 
  filter(bdm_n_stao >= 20)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Compile result per model and species----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Overview of the available model runs
dat %>%
  group_by(runID) %>% 
  dplyr::summarise(
    n = n(),
    nJahre = n_distinct(year),
    nArten = n_distinct(NUESP)
  )

# Tmporal trends
for(i in 1:nrow(species)) {
  s <- species$NUESP[i]
  if(sum(dat$NUESP == s) > 0) {
    tmp <- dat %>% 
      filter(NUESP == s) %>% 
      group_by(runID) %>%
      dplyr::summarise(
        nyear = length(year),
        nSTI = length(STI),
        Trend = coef(lm(STI ~ year))[2]
      ) 
    species[i, "BDM-only"] <- ifelse(sum(tmp$runID == "BDM-only") > 0, tmp$Trend[tmp$runID == "BDM-only"], NA)
    species[i, "infospecies-raw"] <- ifelse(sum(tmp$runID == "infospecies-raw") > 0, tmp$Trend[tmp$runID == "infospecies-raw"], NA)
    species[i, "infospecies-raw-occupancy"] <- ifelse(sum(tmp$runID == "infospecies-raw-occupancy") > 0, tmp$Trend[tmp$runID == "infospecies-raw-occupancy"], NA)
    species[i, "Standard"] <- ifelse(sum(tmp$runID == "Standard") > 0, tmp$Trend[tmp$runID == "Standard"], NA)
    species[i, "infospecies-raw-occupancy-reportingtyp"] <- ifelse(sum(tmp$runID == "infospecies-raw-occupancy-reportingtyp") > 0, tmp$Trend[tmp$runID == "infospecies-raw-occupancy-reportingtyp"], NA)
    species[i, "infospecies-raw-randomyear-GLM"] <- ifelse(sum(tmp$runID == "infospecies-raw-randomyear-GLM") > 0, tmp$Trend[tmp$runID == "infospecies-raw-randomyear-GLM"], NA)
    species[i, "siteoccupancy-sources"] <- ifelse(sum(tmp$runID == "siteoccupancy-sources") > 0, tmp$Trend[tmp$runID == "siteoccupancy-sources"], NA)
    species[i, "ohne_sources"] <- ifelse(sum(tmp$runID == "siteoccupancy-2sources") > 0, tmp$Trend[tmp$runID == "siteoccupancy-2sources"], NA)
    species[i, "infospecies-raw-occupancy-increasingspecies"] <- ifelse(sum(tmp$runID == "infospecies-raw-occupancy-increasingspecies") > 0, tmp$Trend[tmp$runID == "infospecies-raw-occupancy-increasingspecies"], NA)
    species[i, "infospecies-raw-occupancy-reportingtyp_v2"] <- ifelse(sum(tmp$runID == "infospecies-raw-occupancy-reportingtyp_v2") > 0, tmp$Trend[tmp$runID == "infospecies-raw-occupancy-reportingtyp_v2"], NA)
  }
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Calculate correlation and bias ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# BDM vs. infospecies-raw
t1 <- cor.test(species$`BDM-only`, species$`infospecies-raw`, paired = TRUE)
t2 <- t.test(species$`BDM-only`, species$`infospecies-raw`, paired = TRUE)
p1 <- species %>% 
  ggplot(aes(x = `BDM-only`, y = `infospecies-raw`)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  # geom_smooth(method = "lm", color = "orange") +
  labs(
    title = "(a) Raw number of\nobservations", 
    subtitle = paste0("Correlation: ", round(t1$estimate, 2), "; bias = ", round(t2$estimate, 2)),
    x = "Species trend based on\nBDM data",
    y = "Species trends based on\nraw number of observations"
    ) +
  ylim(-10, 35) +
  xlim(-10, 35)

# BDM vs. infospecies-raw-occupancy
t1 <- cor.test(species$`BDM-only`, species$`infospecies-raw-occupancy`, paired = TRUE)
t2 <- t.test(species$`BDM-only`, species$`infospecies-raw-occupancy`, paired = TRUE)
p2 <- species %>% 
  ggplot(aes(x = `BDM-only`, y = `infospecies-raw-occupancy`)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  # geom_smooth(method = "lm", color = "orange") +
  labs(
    title = "(b) Proportion of occupied\nsites", 
    subtitle = paste0("Correlation: ", round(t1$estimate, 2), "; bias = ", round(t2$estimate, 2)),
    x = "Species trend based on\nBDM data",
    y = "Species trends based on\nproportion of occupied sites"
  ) +
  ylim(-10, 35) +
  xlim(-10, 35)

# BDM vs. occupancy with reporting type
t1 <- cor.test(species$`BDM-only`, species$`infospecies-raw-occupancy-reportingtyp`, paired = TRUE)
t2 <- t.test(species$`BDM-only`, species$`infospecies-raw-occupancy-reportingtyp`, paired = TRUE)
p3 <- species %>% 
  ggplot(aes(x = `BDM-only`, y = `infospecies-raw-occupancy-reportingtyp`)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  # geom_smooth(method = "lm", color = "orange") +
  labs(
    title = "(c) Proportion of occupied\nsites based on reporting type", 
    subtitle = paste0("Correlation: ", round(t1$estimate, 2), "; bias = ", round(t2$estimate, 2)),
    x = "Species trend based on\nBDM data",
    y = "Species trends based on\nproportion of occupied sites"
  ) +
  ylim(-10, 35) +
  xlim(-10, 35)

# BDM vs. infospecies-raw-randomyear-GLM
t1 <- cor.test(species$`BDM-only`, species$`infospecies-raw-randomyear-GLM`, paired = TRUE)
t2 <- t.test(species$`BDM-only`, species$`infospecies-raw-randomyear-GLM`, paired = TRUE)
p4 <- species %>% 
  ggplot(aes(x = `BDM-only`, y = `infospecies-raw-randomyear-GLM`)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  # geom_smooth(method = "lm", color = "orange") +
  labs(
    title = "(d) Random year GLM of\noccupied sites", 
    subtitle = paste0("Correlation: ", round(t1$estimate, 2), "; bias = ", round(t2$estimate, 2)),
    x = "Species trend based on\nBDM data",
    y = "Species trends based on\nrandom year GLM"
  ) +
  ylim(-10, 35) +
  xlim(-10, 35)

# BDM vs. siteoccupancy model
t1 <- cor.test(species$`BDM-only`, species$`siteoccupancy-sources`, paired = TRUE)
t2 <- t.test(species$`BDM-only`, species$`siteoccupancy-sources`, paired = TRUE)
p6 <- species %>%
  ggplot(aes(x = `BDM-only`, y = `siteoccupancy-sources`)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  # geom_smooth(method = "lm", color = "#33A02C") +
  labs(
    title = "(e) Site-occupancy model\n",
    subtitle = paste0("Correlation: ", round(t1$estimate, 2), "; bias = ", round(t2$estimate, 2)),
    x = "Species trend based on BDM data",
    y = "Species trends based on site-occupancy model"
  ) +
  ylim(-10, 35) +
  xlim(-10, 35)
s = 44
p6 <- p6 +
  annotate(geom = "curve",
    xend = 0.98 * species$`BDM-only`[s],
    yend = 0.99 * species$`siteoccupancy-sources`[s],
    x = 30, y = 14,
    col = "black", curvature = -.3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 30, y = 13, label = species$ESP[s], hjust = "center")
s = 95
p6 <- p6 +
  annotate(geom = "curve",
           xend = 1.01 * species$`BDM-only`[s],
           yend = 1.06 * species$`siteoccupancy-sources`[s],
           x = 6, y = 14,
           col = "black", curvature = -.3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 6, y = 15, label = species$ESP[s], hjust = "center")
s = 58
p6 <- p6 +
  annotate(geom = "curve",
           xend = species$`BDM-only`[s],
           yend = -0.8,
           x = -2, y = 8,
           col = "black", curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = -2, y = 9, label = species$ESP[s], hjust = "center")
s = 61
p6 <- p6 +
  annotate(geom = "curve",
           xend = species$`BDM-only`[s],
           yend = -1.7,
           x = -5, y = -8,
           col = "black", curvature = -.3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = -5, y = -9, label = species$ESP[s], hjust = "left")

# BDM vs. siteoccupancy model without sources
t1 <- cor.test(species$`BDM-only`, species$ohne_sources, paired = TRUE)
t2 <- t.test(species$`BDM-only`, species$ohne_sources, paired = TRUE)
p5 <- species %>% 
  ggplot(aes(x = `BDM-only`, y = ohne_sources)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  # geom_smooth(method = "lm", color = "#33A02C") +
  labs(
    title = "(e) Site-occupancy model\n", 
    subtitle = paste0("Correlation: ", round(t1$estimate, 2), "; bias = ", round(t2$estimate, 2)),
    x = "Species trend based on BDM data",
    y = "Species trends based on site-occupancy model"
  ) +
  ylim(-10, 35) +
  xlim(-10, 35)
s = 44
p5 <- p5 + 
  annotate(geom = "curve", 
           xend = 0.98 * species$`BDM-only`[s], 
           yend = 0.99 * species$ohne_sources[s], 
           x = 30, y = 14, 
           col = "black", curvature = -.3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 30, y = 13, label = species$ESP[s], hjust = "center")
s = 95
p5 <- p5 + 
  annotate(geom = "curve", 
           xend = 1.0 * species$`BDM-only`[s], 
           yend = 1.06 * species$ohne_sources[s], 
           x = 3, y = 21, 
           col = "black", curvature = -.3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 0, y = 22, label = species$ESP[s], hjust = "center")
s = 58
p5 <- p5 + 
  annotate(geom = "curve", 
           xend = species$`BDM-only`[s], 
           yend = -0.8, 
           x = -2, y = 8, 
           col = "black", curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = -2, y = 9, label = species$ESP[s], hjust = "center")
s = 61
p5 <- p5 + 
  annotate(geom = "curve", 
           xend = species$`BDM-only`[s], 
           yend = -1.7, 
           x = -5, y = -8, 
           col = "black", curvature = -.3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = -5, y = -9, label = species$ESP[s], hjust = "left")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Make plot ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

layout <- "
ABEEFF
CDEEFF
"
p1 + p2 +p3 +p4 + p5 + p6 +
  plot_layout(design = layout)
ggsave("Figures/Fig2-Trend-compare-models.pdf", width = 15, height = 6)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Weitere Vergleiche ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Eigener Meldetyp für zunehmende Arten
t1 <- cor.test(species$`BDM-only`, species$`infospecies-raw-occupancy-increasingspecies`)
t2 <- t.test(species$`BDM-only`, species$`infospecies-raw-occupancy-increasingspecies`, paired = TRUE)
species %>% 
  ggplot(aes(x = `BDM-only`, y = `infospecies-raw-occupancy-increasingspecies`)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  # geom_smooth(method = "lm", color = "orange") +
  labs(
    title = "(a) Raw number of\nobservations", 
    subtitle = paste0("Correlation: ", round(t1$estimate, 2), "; bias = ", round(t2$estimate, 2)),
    x = "Species trend based on\nBDM data",
    y = "Species trends based on\nraw number of observations"
    ) +
  ylim(-10, 35) +
  xlim(-10, 35)

# Meldetyp_v2
t1 <- cor.test(species$`BDM-only`, species$`infospecies-raw-occupancy-reportingtyp_v2`, paired = TRUE)
t2 <- t.test(species$`BDM-only`, species$`infospecies-raw-occupancy-reportingtyp_v2`, paired = TRUE)
species %>% 
  ggplot(aes(x = `BDM-only`, y = `infospecies-raw-occupancy-reportingtyp`)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  # geom_smooth(method = "lm", color = "orange") +
  labs(
    title = "(c) Proportion of occupied\nsites based on reporting type", 
    subtitle = paste0("Correlation: ", round(t1$estimate, 2), "; bias = ", round(t2$estimate, 2)),
    x = "Species trend based on\nBDM data",
    y = "Species trends based on\nproportion of occupied sites"
  ) +
  ylim(-10, 35) +
  xlim(-10, 35)







