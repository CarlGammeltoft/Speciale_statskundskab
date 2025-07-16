# Indlæser nødvendige pakker og data -----------------------------------------------------------
setwd("/Users/carlgammeltoft/Lokale dokumenter/R_speciale/4_modelling/")
data <- read.csv("1_input/data_finalized.csv")

library(modelsummary)
library(stargazer)
library(fixest)
library(tidyverse)
library(broom)
library(zoo)
library(lfe)
library(pscl)
library(MASS)

# Opdel data ------------------------------------------------------------
data_conservative_15 <- data %>% 
  filter(conservative_15 == 1)

data_liberal_15 <- data %>% 
  filter(liberal_15 == 1)

data_conservative <- data %>% 
  filter(main_ideology == 1)

data_liberal <- data %>% 
  filter(main_ideology == 0)

# ## DV: Antallet af demonstrationer ----------------------------------------------------------

# 15% Liberale ----------------------------------------------------------
# M1: NB - Antallet af demonstrationer
M1_liberal_15_count <- glm.nb(
  communication_window_7 ~ count_palestine_protests, 
  data = data_liberal_15
)

summary(M1_liberal_15_count)

# M2: NB - Antallet af demonstrationer + Kontrollvariabel
M2_liberal_15_count <- glm.nb(
  communication_window_7 ~ count_palestine_protests + protest_window_7_after, 
  data = data_liberal_15
)

summary(M2_liberal_15_count)

# M3: ZINB - Antallet af demonstrationer + Kontrollvariabel + FE
M3_liberal_15_count <- femlm(
  communication_window_7 ~ count_palestine_protests + protest_window_7_after | bioguide_id + date, 
  data = data_liberal_15, 
  dist = "negbin"
)

summary(M3_liberal_15_count)

# 15% Konservative ----------------------------------------------------------
# M1: NB - Antallet af demonstrationer
M1_conservative_15_count <- glm.nb(
  communication_window_7 ~ count_palestine_protests, 
  data = data_conservative_15
)

summary(M1_conservative_15_count)

# M2: NB - Antallet af demonstrationer + Kontrollvariabel
M2_conservative_15_count <- glm.nb(
  communication_window_7 ~ count_palestine_protests + protest_window_7_after, 
  data = data_conservative_15
)

summary(M2_conservative_15_count)

# M3: ZINB - Antallet af demonstrationer + Kontrollvariabel + FE
M3_conservative_15_count <- femlm(
  communication_window_7 ~ count_palestine_protests + protest_window_7_after | bioguide_id + date, 
  data = data_conservative_15, 
  dist = "negbin"
)

summary(M3_conservative_15_count)

# Alle Liberale ----------------------------------------------------------
# M1: NB - Antallet af demonstrationer
M1_liberal_count <- glm.nb(
  communication_window_7 ~ count_palestine_protests, 
  data = data_liberal
)

summary(M1_liberal_count)

# M2: NB - Antallet af demonstrationer + Kontrollvariabel
M2_liberal_count <- glm.nb(
  communication_window_7 ~ count_palestine_protests + protest_window_7_after, 
  data = data_liberal
)

summary(M2_liberal_count)

# M3: ZINB - Antallet af demonstrationer + Kontrollvariabel + FE
M3_liberal_count <- femlm(
  communication_window_7 ~ count_palestine_protests + protest_window_7_after | bioguide_id + date, 
  data = data_liberal, 
  dist = "negbin"
)

summary(M3_liberal_count)

# Alle Konservative ----------------------------------------------------------
# M1: NB - Antallet af demonstrationer
M1_conservative_count <- glm.nb(
  communication_window_7 ~ count_palestine_protests, 
  data = data_conservative
)

summary(M1_conservative_count)

# M2: NB - Antallet af demonstrationer + Kontrollvariabel
M2_conservative_count <- glm.nb(
  communication_window_7 ~ count_palestine_protests + protest_window_7_after, 
  data = data_conservative
)

summary(M2_conservative_count)

# M3: ZINB - Antallet af demonstrationer + Kontrollvariabel + FE
M3_conservative_count <- femlm(
  communication_window_7 ~ count_palestine_protests + protest_window_7_after | bioguide_id + date, 
  data = data_conservative, 
  dist = "negbin"
)

summary(M3_conservative_count)


# Gemmer modellerne -------------------------------------------------------

model_list <- list(
"M1: Liberal 15% - Count" = M1_liberal_15_count,
"M2: Liberal 15% - Count + CV" = M2_liberal_15_count,
"M3: Liberal 15% - Count + FE" = M3_liberal_15_count,
"M1: Conservative 15% - Count" = M1_conservative_15_count,
"M2: Conservative 15% - Count + CV" = M2_conservative_15_count,
"M3: Conservative 15% - Count + FE" = M3_conservative_15_count,
"M1: Liberal - Count" = M1_liberal_count,
"M2: Liberal - Count + CV" = M2_liberal_count,
"M3: Liberal - Count + FE" = M3_liberal_count,
"M1: Conservative - Count" = M1_conservative_count,
"M2: Conservative - Count + CV" = M2_conservative_count,
"M3: Conservative - Count + FE" = M3_conservative_count
)

# Brug modelsummary til at lave en tabel
modelsummary(
  models = model_list,
  stars = TRUE,
  gof_omit = "AIC|BIC|Log.*|Num.*",  # Fjern unødvendige GOF-mål
  coef_map = c(
    "count_palestine_protests" = "Antal demonstrationer",
    "demonstration_intensity_palestine" = "Demonstrationsintensitet",
    "protest_window_7_after" = "Demonstrationer efter (kontrol)"
  ))
