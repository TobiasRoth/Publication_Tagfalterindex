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
library(BDM)

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

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Hilfsfunktionen ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Calculate beta diversity
getZ12 <- function(aID_STAO, NUESP) {
  tmp <- tibble(
    aID_STAO = aID_STAO,
    NUESP = NUESP,
    occ = 1
  ) %>% 
    BDM::sim(method = "simpson", listin = TRUE, listout = TRUE)
  mean(tmp$simpson)
}

# Select species with strongest change in change of occupied sites
getspectrend <- function(s) {
  tmp <- survBDM %>% 
    left_join(
      bdm %>% 
        filter(NUESP == s) %>% 
        transmute(aID_KD, occ = as.integer(Ind > 0)) 
    ) %>% 
    replace_na(list(occ = 0)
    )
  mod <- lm(occ ~ year, data = tmp) 
  coef(mod)[2]
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Prepare data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Load BDM Data
load("Data/bdm.RData")
load("Data/survBDM.RData")

# Load species list
species <- read_excel("Tables/Appendix_Species-List_v3.xlsx") 

# Calculate indicators based on all records
res <- survBDM %>% 
  left_join(
    bdm %>% 
      left_join(survBDM) %>% 
      group_by(aID_KD) %>% 
      dplyr::summarise(
        SR = n_distinct(NUESP),
        ind = sum(Ind, na.rm = TRUE)
      ) 
  ) %>% 
  group_by(year) %>% 
  dplyr::summarise(
    SR = mean(SR),
    Ind = mean(ind)
  )

# Calculate indicators based on all records (5-year values)
for(t in 5:nrow(res)){
  tt <- bdm %>% 
    left_join(survBDM) %>% 
    filter(!is.na(NUESP)) %>% 
    filter(year >= (res$year[t]-4) & year <= res$year[t])
  res[t, "year"] <-  res$year[t]
  res[t, "Z12"] = getZ12(tt$aID_STAO, tt$NUESP)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Make figure ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Species richness
p1 <-  res %>% 
  ggplot(aes(x = year, y = SR)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = lm) +
  labs(
    title = "(a) Species richness",
    y = "Number of species per 1km-square",
    x = ""
  ) +
  ylim(0, NA)

# Number of Individuals
p2 <-  res %>% 
  ggplot(aes(x = year, y = Ind)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = lm) +
  labs(
    title = "(b) Total number of individuals",
    y = "Number of individuals per 1km-square",
    x = ""
  ) +
  ylim(0, NA)

# Beta diversity
p3 <-  res %>% 
  ggplot(aes(x = year, y = 100 * Z12)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = lm) +
  labs(
    title = "(c) Beta diversity",
    y = "Occupancy-based Simpson-Index",
    x = ""
  ) +
  ylim(30, 50) +
  scale_x_continuous(
    limits = c(2007, 2023),
    breaks = seq(2007, 2023, 3), 
    labels = paste0(seq(2003, 2019, 3), "-\n", seq(2007, 2023, 3)))

# Save figure
p1 + p2 + p3 + plot_layout(guides = "collect")
ggsave("Figures/Fig5-traditional_indicators.pdf", width = 12, height = 4)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Statistical results ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Temporal Trends
lm(log(SR) ~ year, data = res) %>% summary
lm(log(Ind) ~ year, data = res) %>% summary
lm(logit(Z12) ~ year, data = res) %>% summary

# Korrelation
cor.test(res$SR, res$Ind)
