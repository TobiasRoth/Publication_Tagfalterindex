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
dat <- read_csv("Tables/results.csv", col_types = "ciddddddddic") %>% 
  filter(year >= 2003)

# Load species list
species <- read_excel("Tables/Appendix_Species-List.xlsx") %>% 
  filter(bdm_n_squares >= 20)

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
    species[i, "BDM-only"] <- ifelse(sum(tmp$runID == "2025_BDM_v5") > 0, tmp$Trend[tmp$runID == "2025_BDM_v5"], NA)
    species[i, "infospecies-raw"] <- ifelse(sum(tmp$runID == "infospecies-raw") > 0, tmp$Trend[tmp$runID == "infospecies-raw"], NA)
    species[i, "infospecies-occupancy"] <- ifelse(sum(tmp$runID == "infospecies-occupancy") > 0, tmp$Trend[tmp$runID == "infospecies-occupancy"], NA)
    species[i, "infospecies-reportingtyp"] <- ifelse(sum(tmp$runID == "infospecies-reportingtyp") > 0, tmp$Trend[tmp$runID == "infospecies-reportingtyp"], NA)
    species[i, "infospecies-randomyear-GLM"] <- ifelse(sum(tmp$runID == "infospecies-randomyear-GLM-occ") > 0, tmp$Trend[tmp$runID == "infospecies-randomyear-GLM-occ"], NA)
    species[i, "siteoccupancy"] <- ifelse(sum(tmp$runID == "2025_siteoccupancy_v2") > 0, tmp$Trend[tmp$runID == "2025_siteoccupancy_v1"], NA)
  }
}

# Save file for review
species %>% 
  transmute(
    NUESP, 
    Species_Name, 
    `BDM-Reference` = `BDM-only` %>% round(3), 
    `a-number-squares` = `infospecies-raw` %>% round(3),
    `b-occupied` = `infospecies-occupancy` %>% round(3),
    `c-reportingtyp` = `infospecies-reportingtyp` %>% round(3),
    `d-randomyear-GLM` = `infospecies-randomyear-GLM` %>% round(3),
    `e-siteoccupancy` = siteoccupancy %>% round(3)) %>% 
  openxlsx::write.xlsx("Review/Campare_estimated_trends.xlsx")
  
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Calculate correlation and bias ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# BDM vs. infospecies-raw
t1 <- cor.test(species$`BDM-only`, species$`infospecies-raw`, paired = TRUE)
t2 <- t.test(species$`infospecies-raw`, species$`BDM-only`, paired = TRUE)
p1 <- species %>% 
  ggplot(aes(x = `BDM-only`, y = `infospecies-raw`)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  # geom_smooth(method = "lm", color = "orange") +
  labs(
    title = "(a) Raw number of occupied\nsquares", 
    subtitle = paste0("Correlation: ", round(t1$estimate, 2), "; bias = ", round(t2$estimate, 2)),
    x = "Species trend based on\nstructured data",
    y = "Species trends based on\nunstructured data"
    ) +
  ylim(-10, 35) +
  xlim(-10, 35)

# BDM vs. infospecies-raw-occupancy
t1 <- cor.test(species$`BDM-only`, species$`infospecies-occupancy`, paired = TRUE)
t2 <- t.test(species$`infospecies-occupancy`, species$`BDM-only`, paired = TRUE)
p2 <- species %>% 
  ggplot(aes(x = `BDM-only`, y = `infospecies-occupancy`)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  # geom_smooth(method = "lm", color = "orange") +
  labs(
    title = "(b) Proportion of occupied\nsquares", 
    subtitle = paste0("Correlation: ", round(t1$estimate, 2), "; bias = ", round(t2$estimate, 2)),
    x = "Species trend based on\nstructured data",
    y = "Species trends based on\nunstructured data"
  ) +
  ylim(-10, 35) +
  xlim(-10, 35)

# BDM vs. occupancy with reporting type
t1 <- cor.test(species$`BDM-only`, species$`infospecies-reportingtyp`, paired = TRUE)
t2 <- t.test(species$`infospecies-reportingtyp`, species$`BDM-only`, paired = TRUE)
p3 <- species %>% 
  ggplot(aes(x = `BDM-only`, y = `infospecies-reportingtyp`)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  # geom_smooth(method = "lm", color = "orange") +
  labs(
    title = "(c) Proportion of occupied\nsquares based on reporting type", 
    subtitle = paste0("Correlation: ", round(t1$estimate, 2), "; bias = ", round(t2$estimate, 2)),
    x = "Species trend based on\nstructured data",
    y = "Species trends based on\nunstructured data"
  ) +
  ylim(-10, 35) +
  xlim(-10, 35)

# BDM vs. infospecies-randomyear-GLM
t1 <- cor.test(species$`BDM-only`, species$`infospecies-randomyear-GLM`, paired = TRUE)
t2 <- t.test(species$`infospecies-randomyear-GLM`, species$`BDM-only`, paired = TRUE)
p4 <- species %>% 
  ggplot(aes(x = `BDM-only`, y = `infospecies-randomyear-GLM`)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  # geom_smooth(method = "lm", color = "orange") +
  labs(
    title = "(d) Random year GLMM of\noccupied squares", 
    subtitle = paste0("Correlation: ", round(t1$estimate, 2), "; bias = ", round(t2$estimate, 2)),
    x = "Species trend based on\nstructured data",
    y = "Species trends based on\nunstructured data"
  ) +
  ylim(-10, 35) +
  xlim(-10, 35)

# BDM vs. siteoccupancy model without sources
t1 <- cor.test(species$`BDM-only`, species$siteoccupancy, paired = TRUE)
t2 <- t.test(species$siteoccupancy, species$`BDM-only`, paired = TRUE)
p5 <- species %>% 
  ggplot(aes(x = `BDM-only`, y = siteoccupancy)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  # geom_smooth(method = "lm", color = "#33A02C") +
  labs(
    title = "(e) Site-occupancy model\n", 
    subtitle = paste0("Correlation: ", round(t1$estimate, 2), "; bias = ", round(t2$estimate, 2)),
    x = "Species trend based on structured data",
    y = "Species trends based on unstructured data"
  ) +
  ylim(-10, 35) +
  xlim(-10, 35)
s = 36
p5 <- p5 + 
  annotate(geom = "curve", 
           xend = 1 * species$`BDM-only`[s], 
           yend = 0.98 * species$siteoccupancy[s], 
           x = 30, y = 14, 
           col = "black", curvature = -.3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 30, y = 13, label = species$Species_Name[s], hjust = "center")
s = 91
p5 <- p5 + 
  annotate(geom = "curve", 
           xend = 1.0 * species$`BDM-only`[s], 
           yend = 1.06 * species$siteoccupancy[s], 
           x = 3, y = 21, 
           col = "black", curvature = -.3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 0, y = 22, label = species$Species_Name[s], hjust = "center")
s = 116
p5 <- p5 + 
  annotate(geom = "curve", 
           xend = species$`BDM-only`[s], 
           yend = 0.8 * species$siteoccupancy[s], 
           x = -4, y = 6, 
           col = "black", curvature = -.3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = -4, y = 7, label = species$Species_Name[s], hjust = "center")
s = 35
p5 <- p5 + 
  annotate(geom = "curve", 
           xend = species$`BDM-only`[s], 
           yend = 0.95 * species$siteoccupancy[s], 
           x = 15, y = -1, 
           col = "black", curvature = -.3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 15, y = -2, label = species$Species_Name[s], hjust = "left")
p5
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Make plot ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

layout <- "
ABEE
CDEE
"
p1 + p2 +p3 +p4 + p5 +
  plot_layout(design = layout)
ggsave("Figures/Fig2-Trend-compare-models.pdf", width = 15, height = 6)

