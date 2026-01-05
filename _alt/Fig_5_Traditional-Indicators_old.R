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

# Remove species with the largest changes
# spectrend <- tibble(
#   NUESP = unique(bdm$NUESP) %>% as.integer()
# )
# for(i in 1:nrow(spectrend)) spectrend[i, "trend"] <- getspectrend(spectrend$NUESP[i])
# spectrend <- spectrend %>% 
#   left_join(
#     species %>% transmute(NUESP, Species_Name)
#   ) %>% 
#   arrange(desc(abs(trend)))
# ausw <- spectrend$NUESP[1:10]

spectrend <- read_csv("Tables/results.csv", col_types = "ciddddddddic") %>% 
  filter(runID %in% c("2025_siteoccupancy_v2")) %>% 
  filter(year >= 2003) %>%
  group_by(NUESP) %>%
  dplyr::summarise(
    nyear = length(year),
    nSTI = length(STI),
    trend = coef(lm(STI ~ year))[2]
  ) %>% 
  left_join(
    species %>% transmute(NUESP, Species_Name, bdm_n_stao)
  ) %>% 
  arrange((abs(trend))) %>% 
  filter(bdm_n_stao >= 100)
mean(spectrend$trend[1:10] > 0)
ausw <- spectrend$NUESP[1:20]

# Calculate indicators without species with largest change
res1 <- survBDM %>% 
  left_join(
    bdm %>% 
      filter(!(NUESP %in% ausw)) %>% 
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

# Calculate indicators without species with largest change (5-year values)
for(t in 5:nrow(res1)){
  tt <- bdm %>% 
    filter(!(NUESP %in% ausw)) %>% 
    left_join(survBDM) %>% 
    filter(!is.na(NUESP)) %>% 
    filter(year >= (res$year[t]-4) & year <= res$year[t])
  res1[t, "year"] <-  res1$year[t]
  res1[t, "Z12"] = getZ12(tt$aID_STAO, tt$NUESP)
}
res <- rbind(
  res %>% mutate(Type = "all"),
  res1 %>% mutate(Type = "without 10")
) %>% 
  as_tibble() %>% 
  mutate(Type = factor(Type))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Make figure ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Species richness
p1 <-  res %>% 
  ggplot(aes(x = year, y = SR, col = Type)) +
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
  ggplot(aes(x = year, y = Ind, col = Type)) +
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
  ggplot(aes(x = year, y = Z12, col = Type)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = lm) +
  labs(
    title = "(c) Beta diversity",
    y = "Simpson-Index",
    x = ""
  ) +
  ylim(0.3, 0.5) +
  scale_x_continuous(
    limits = c(2007, 2023),
    breaks = seq(2007, 2023, 3), 
    labels = paste0(seq(2003, 2019, 3), "-\n", seq(2007, 2023, 3)))

# Save figure
p1 + p2 + p3 + plot_layout(guides = "collect")
ggsave("Figures/Fig5-traditional_indicators.pdf", width = 12, height = 4)

