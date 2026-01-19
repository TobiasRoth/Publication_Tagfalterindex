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
      legend.title = element_blank(), 
      legend.position = "right", 
      plot.title = element_text(face = "plain", size = 14),
      legend.background = element_rect(colour = "white"),
      plot.background = element_blank())
)
options(ggplot2.discrete.colour= c("#1F78B4", "#FF7F00", "#33A02C", "#E31A1C", "#6A3D9A"))

# Uper limit for y-axis
ylimup <- 180

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Link zur BDM Datenbank
db <- DBI::dbConnect(RSQLite::SQLite(), "~/OneDrive - Hintermann + Weber AG/BDM_DB/DB_BDM_2025-06-02.db")

# Load model results and select relvant models
dat <- read_csv("Tables/results.csv", col_types = "ciddddddddic") %>% 
  filter(runID %in% c("2025_siteoccupancy_v2"))

# Load species list
species <- read_excel("Tables/Appendix_Species-List.xlsx") %>% 
  left_join(tbl(db, "Traits_TF") %>% transmute(aID_SP, EU_Grassland_Indicator), copy = TRUE)

# Load and prepare data from European Grassland Indicator
egi <-  
  tibble(
    year = 1990,
    indicator = 100, 
    ind_gam0 = NA,
    SMOOTH = NA, 
    NSPECIES = NA, 
    LOWsmooth1 = NA, 
    UPPsmooth1 = NA,
  ) %>% rbind(
    read_csv("Data/EU27_grassland_indicator_values.csv")
  )

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Functions ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Function to calculate species trends
getartgruppentrend <- function(d) {
  index <- d %>%
    group_by(year) %>% 
    dplyr::summarise(CI = median(mittel %>% plogis)) 
  ref <- index$CI[!is.na(match(index$year, 2003:2007))] %>% mean
  index$CI <- 100 * index$CI / ref
  nsim <- 1000
  sim <- array(dim = c(nrow(index), nsim))
  for(s in 1:nsim) {
    tt <- rnorm(n = nrow(d), d$mittel, d$sd) %>% plogis
    sim[, s] <- tapply(tt, d$year, median)
  }
  index$lo <- 100 * apply(sim, 1, quantile, probs = 0.025) / ref
  index$hi <- 100 * apply(sim, 1, quantile, probs = 0.975) /ref
  index
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Figure: EU-Grassland Indicator ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Swiss Grassland Indicator
tmp <- dat %>% 
  filter(!is.na(match(NUESP, species$NUESP[!is.na(species$EU_Grassland_Indicator) & species$EU_Grassland_Indicator == 1]))) %>%
  getartgruppentrend
ref <- egi$indicator[egi$year %in% 2003:2007] %>% mean 
rbind(
  tmp %>% transmute(year, indicator = CI, Type = "Swiss grassland indicator"),
  egi %>% transmute(year, indicator = 100 * indicator / ref, Type = "European grassland indicator")
) %>% 
  mutate(Type = factor(Type, levels = c("European grassland indicator", "Swiss grassland indicator"))) %>% 
  ggplot(aes(x = year, y = indicator, col = Type)) +
  geom_abline(slope = 0, intercept = 100, col = "grey", lty = 2) +
  geom_point(cex = 1.5) +
  geom_line() +
  geom_smooth(method = lm,) + 
  ylim(0, ylimup) +
  xlim(1990, 2023) +
  labs(
    x = "", 
    y = "Multi-species index")


# Korrelation
tmp <- tmp %>% 
  left_join(egi)
cor.test(tmp$CI, 100 * egi$indicator / ref)

# Save figure
ggsave("Figures/Fig6-european_grassland_indicator.pdf", width = 7, height = 4)

