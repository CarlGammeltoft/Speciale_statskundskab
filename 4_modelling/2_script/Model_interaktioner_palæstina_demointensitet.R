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

# ## DV: Demonstrationsintensitet - pro-palestine - INTERAKTIONER ----------------------------------------------------------

# Alle politikere ----------------------------------------------------------
# M1: NB - Demonstrationsintensitet - simpelt estimat
M1_intensity_estimate <- glm.nb(
  communication_window_7 ~ demonstration_intensity_palestine + 
  ideology_percentile * demonstration_intensity_palestine, 
  data = data
)

summary(M1_intensity_estimate)

# M2: NB - Demonstrationsintensitet - simpelt estimat + kontrol
M2_intensity_estimate_control <- glm.nb(
  communication_window_7 ~ demonstration_intensity_palestine + 
  ideology_percentile * demonstration_intensity_palestine +
  demonstration_intensity_total_7_after, 
  data = data
)
summary(M2_intensity_estimate_control)


# M3: NB - Demonstrationsintensitet - simpelt estimat + kontrol + FE
M3_intensity_estimate_control_FE <- femlm(
  communication_window_7 ~ demonstration_intensity_palestine + 
    ideology_percentile * demonstration_intensity_palestine +
    demonstration_intensity_total_7_after | bioguide_id + date, 
  data = data
)

summary(M3_intensity_estimate_control_FE)

# Poisson regression with fixed effects using fixest
M3_poisson <- feglm(
  communication_window_7 ~ demonstration_intensity_palestine + 
    ideology_percentile * demonstration_intensity_palestine + 
    demonstration_intensity_total_7_after | bioguide_id + date, 
  family = poisson(link = "log"),
  data = data_clean
)

# Summary of the model
summary(M3_poisson)
