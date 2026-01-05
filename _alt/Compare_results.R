rm(list = ls(all = TRUE))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Einstellungen ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Libraries
library(tidyverse)
library(Rmisc)
library(ggthemes)
library(readxl)

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

# Connection to BDM data base
db <- DBI::dbConnect(RSQLite::SQLite(), "~/OneDrive - Hintermann + Weber AG/BDM_DB/DB_BDM_2025-03-28.db")

# Resultate einlesen
resultate <- read_csv("Stanresults/resultate.csv")

# Arten einlesen
species <- read_csv("Data/species.csv")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Vergleich mit und ohne BDM ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Auswahl der Resultate
res <- resultate %>% 
  filter(!is.na(match(runID, c("Standard", "withoutBDM"))))

# Arten mit mehreren Nachweisen verwenden
species <- res %>% 
  group_by(NUESP) %>%
  dplyr::summarise(
    AnzModels = n_distinct(runID),
  ) %>%
  left_join(
    species %>% 
      dplyr::select(NUESP, Art = ESP)
  ) %>% 
  filter(AnzModels == 2)

# Resultate aufbereiten
res$quelle <- "Standard"
res$quelle[res$runID == "withoutBDM"] <- "without BDM"
res$quelle <- factor(res$quelle, levels = c("Standard", "without BDM"))

# Grafiken erstellen und Resultate zusammenstellen
for (r in species$NUESP) {
  # Breite der Vertrauensbereiche berechnen
  tt <- res %>% 
    filter(NUESP == r & year >= 2003) %>% 
    group_by(NUESP, runID) %>%
    dplyr::summarise(
      er = mean(sd)
    )
  species[match(r, species$NUESP), "Improve"] <- round(100 * (tt$er[2] - tt$er[1]) / tt$er[2], 1)
  
  # Grafik erstellen
  ttitle <- paste(species$Art[match(r, species$NUESP)])
  pos <- position_dodge(width = 0.4)
  res %>% 
    filter(NUESP == r) %>% 
    ggplot(aes(x = year, y = STI, col = quelle)) +
    geom_abline(slope = 0, intercept = 100, col = "grey", lty = 2) +
    geom_point(cex = 2, position = pos) +
    geom_line(position = pos) +
    ylim(0, NA) +
    labs(
      title = ttitle,
      x = "", 
      y = "Index [Mittelwert von 2003-2007 = 100]") +
    geom_errorbar(aes(ymin = STI_lo, ymax = STI_up), width=.1, lwd = 0.3, position = pos)
  ggsave(paste0("Figures/Comparision_Standard_BDM/", ttitle, ".pdf"), width = 6, height = 4)
}

mean(species$Improve)

