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

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Link zur BDM Datenbank
db <- DBI::dbConnect(RSQLite::SQLite(), "~/OneDrive - Hintermann + Weber AG/BDM_DB/DB_BDM_2025-06-02.db")

# Load model results and select relvant models
dat <- read_csv("Stanresults/resultate.csv", col_types = "ciddddddddic") %>% 
  filter(runID %in% c("siteoccupancy-sources"))
  # filter(runID %in% c("siteoccupancy-2sources"))

# Load species list
species <- read_excel("Tables/Appendix_Species-List_v1.xlsx") %>% 
  filter(Berechnung_TFI == "ja") %>%
  left_join(tbl(db, "Traits_TF") %>% transmute(aID_SP, Waermetyp, vagabundierend, Nutrient), copy = TRUE)

# Select only 170 species based on trend reviews
# auswspec <- read_xlsx("Data/1750_Review_Arttrends_2020_Resultate_v5.xlsx") %>% 
#   transmute(
#     NUESP = NUESP,
#     Ausschluss = Ausschluss,
#     Begründung = Begründung
#   ) %>% 
#   filter(Ausschluss != "x" | is.na(Ausschluss))
# species <- species %>% filter(!is.na(match(NUESP, unique(auswspec$NUESP))))
# dat <- dat %>% filter(NUESP %in% species$NUESP)

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
# Figure ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Uper limit for y-axis
ylimup <- 160

# All species
tmp <- dat %>% getartgruppentrend 
tcol <- "grey30"
pall <- tmp %>%
  ggplot(aes(x = year, y = CI, ymin = lo, ymax = hi)) +
  geom_abline(slope = 0, intercept = 100, col = "grey", lty = 2) +
  geom_point(cex = 2) +
  geom_line() +
  geom_errorbar(width=.1, lwd = 0.3) +
  geom_smooth(method = lm, , col = tcol, fill = tcol) + 
  ylim(0, ylimup) +
  labs(
    title = "(d) Butterfly species index",
    subtitle = paste(n_distinct(dat$NUESP), "species"),
    x = "", 
    y = "Multi-species index") +
  theme(legend.position = "none")

# Cold adapted species
tcol <- "#56B4E9"
tt <- dat %>% 
  filter(!is.na(match(NUESP, species$NUESP[!is.na(species$Waermetyp) & species$Waermetyp <= 2])))
pkz <- tt %>%
  getartgruppentrend %>% 
  ggplot(aes(x = year, y = CI, ymin = lo, ymax = hi)) +
  geom_abline(slope = 0, intercept = 100, col = "grey", lty = 2) +
  geom_point(cex = 1.5) +
  geom_line() +
  geom_errorbar(width=.1, lwd = 0.3) +
  geom_smooth(method = lm, , col = tcol, fill = tcol) + 
  ylim(0, ylimup) +
  labs(
    title = "(b) Cold-adapted species",
    subtitle = paste(n_distinct(tt$NUESP), "species"),
    x = "", 
    y = "Multi-species index")  

# Warm adapted species
tcol <- "brown2"
tt <- dat %>% 
  filter(!is.na(match(NUESP, species$NUESP[!is.na(species$Waermetyp) & species$Waermetyp >= 4]))) 
pwz <- tt %>% 
  getartgruppentrend %>%
  ggplot(aes(x = year, y = CI, ymin = lo, ymax = hi)) +
  geom_abline(slope = 0, intercept = 100, col = "grey", lty = 2) +
  geom_point(cex = 1.5) +
  geom_line() +
  geom_errorbar(width=.1, lwd = 0.3) +
  geom_smooth(method = lm, , col = tcol, fill = tcol) + 
  ylim(0, ylimup) +
  labs(
    title = "(c) Warm-adapted species",
    subtitle = paste(n_distinct(tt$NUESP), "species"),
    x = "", 
    y = "Multi-species index") 

# Species of oligotrophic habitats
tcol <- "khaki2"
tt <- dat %>% 
  filter(!is.na(match(NUESP, species$NUESP[!is.na(species$Nutrient) & species$Nutrient <= 3])))
pna <- tt %>%
  getartgruppentrend %>% 
  ggplot(aes(x = year, y = CI, ymin = lo, ymax = hi)) +
  geom_abline(slope = 0, intercept = 100, col = "grey", lty = 2) +
  geom_point(cex = 1.5) +
  geom_line() +
  geom_errorbar(width=.1, lwd = 0.3) +
  geom_smooth(method = lm, , col = tcol, fill = tcol) + 
  ylim(0, ylimup) +
  labs(
    title = "(e) Species of oligotrophic habitats",
    subtitle = paste(n_distinct(tt$NUESP), "species"),
    x = "", 
    y = "Multi-species index") 

# Mobile species
tcol <- "mediumpurple1"
tt <- dat %>% 
  filter(!is.na(match(NUESP, species$NUESP[!is.na(species$vagabundierend) & species$vagabundierend == 1]))) 
pma <- tt %>% 
  getartgruppentrend %>%
  ggplot(aes(x = year, y = CI, ymin = lo, ymax = hi)) +
  geom_abline(slope = 0, intercept = 100, col = "grey", lty = 2) +
  geom_point(cex = 1.5) +
  geom_line() +
  geom_errorbar(width=.1, lwd = 0.3) +
  geom_smooth(method = lm, , col = tcol, fill = tcol) + 
  ylim(0, ylimup) +
  labs(
    title = "(f) Mobile species",
    subtitle = paste(n_distinct(tt$NUESP), "species"),
    x = "", 
    y = "Multi-species index") 


# Anteil der Arten mit positiver und negativer Entwicklung
# tcol <- c("#33A02C", "#B2DF8A", "#FB9A99", "#E31A1C")
tcol <- c("#33A02C", "#B2DF8A", "#FF7F00", "#FF7F0080", "grey")
trends <- dat %>% 
  group_by(NUESP) %>% 
  dplyr::summarise(
    trend = coef(lm(mittel ~ year))[2],
    p = summary(lm(mittel ~ year))$coefficients[2, 4]
  )
tmp <- tibble(
  `Species trends` = 
    factor(
      c("positive (significant)", "positive (not significant)",  "negative (significant)", "negative (not significant)", "insufficient data"), 
      levels = c("positive (significant)", "positive (not significant)", "negative (significant)", "negative (not significant)", "insufficient data")),
  value = c(
    sum(trends$trend > 0 & trends$p <= 0.05),
    sum(trends$trend > 0 & trends$p > 0.05),
    sum(trends$trend < 0 & trends$p <= 0.05),
    sum(trends$trend < 0 & trends$p > 0.05),
    34) 
) %>% 
  mutate(label = paste0(value))
tmp$ant = tmp$value / sum(tmp$value)
tmp$ymax = cumsum(tmp$ant)
tmp$ymin = c(0, head(tmp$ymax, n=-1))
tmp$labelPosition <- (tmp$ymax + tmp$ymin) / 2
pdu <- tmp %>% 
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 5, xmin = 4, fill = `Species trends`)) +
  geom_rect() +
  coord_polar(theta = "y") + 
  scale_x_continuous(expand = c(-0.15, 0), limits = c(3, 5)) +
  labs(
    title = "(a) Share of increasing vs. decreasing trends",
    subtitle = paste(sum(tmp$value), "species")
  ) +
  theme_void() +
  theme(
    legend.position = c(1, 0.5),
    legend.justification = c(0, 0.5),
    legend.direction = "vertical", 
    plot.title.position = "plot"
  ) +
  geom_text(x = 4.5, aes(y = labelPosition, label=label), size=4, col = "white") +
  scale_fill_manual(values = tcol) 

# Make figure
layout <- "
AA#BBCC
FFFDDEE
"
layout <- "
A#BC
FFDE
"
pdu + pkz + pwz + pna + pma + pall +
  plot_layout(design = layout)
ggsave("Figures/Fig4-main-results_2sources.pdf", width = 12, height = 6)


