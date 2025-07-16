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

# ## DV: Demonstrationsintensitet - pro-israel - INTERAKTIONER ----------------------------------------------------------

# Alle politikere ----------------------------------------------------------
# M1: NB - Demonstrationsintensitet - simpelt estimat
M1_intensity_estimate <- glm.nb(
  communication_window_7 ~ demonstration_intensity_israel + 
    ideology_percentile * demonstration_intensity_israel, 
  data = data
)

summary(M1_intensity_estimate)

# M2: NB - Demonstrationsintensitet - simpelt estimat + kontrol
M2_intensity_estimate_control <- glm.nb(
  communication_window_7 ~ demonstration_intensity_israel + 
    ideology_percentile * demonstration_intensity_israel +
    demonstration_intensity_total_7_after, 
  data = data
)
summary(M2_intensity_estimate_control)

# M3: NB - Demonstrationsintensitet - simpelt estimat + kontrol + FE
M3_intensity_estimate_control_FE <- femlm(
  communication_window_7 ~ demonstration_intensity_israel + 
    ideology_percentile * demonstration_intensity_israel +
    demonstration_intensity_total_7_after | bioguide_id + date, 
  data = data_clean
)

summary(M3_intensity_estimate_control_FE)



M3_fixed <- femlm(
  communication_window_7 ~ demonstration_intensity_israel + 
    ideology_percentile * demonstration_intensity_israel +
    demonstration_intensity_total_7_after | bioguide_id + date, 
  data = data_clean,
  dist = "negbin"
)
M2_glmmTMB <- glmmTMB(
  communication_window_7 ~ demonstration_intensity_israel + 
    demonstration_intensity_total_7_after + (1 | bioguide_id) + (1 | date),
  family = nbinom2(link = "log"),
  data = data_clean
)


summary(M3_fixed)


# Check VIF
library(car)
vif_data <- lm(
  communication_window_7 ~ demonstration_intensity_israel + 
    ideology_percentile * demonstration_intensity_israel +
    demonstration_intensity_total_7_after,
  data = data_clean
)
vif(vif_data)

M3_intensity_estimate_control_FE <- femlm(
  communication_window_7 ~ demonstration_intensity_israel + 
    ideology_percentile * demonstration_intensity_israel +
    demonstration_intensity_total_7_after | bioguide_id + date, 
  data = data_clean,
  dist = "negbin"  # Explicitly specify Negative Binomial
)
summary(M3_intensity_estimate_control_FE)

library(glmmTMB)

M3_glmmTMB <- glmmTMB(
  communication_window_7 ~ demonstration_intensity_israel + 
    ideology_percentile * demonstration_intensity_israel +
    demonstration_intensity_total_7_after + (1 | bioguide_id) + (1 | date),
  family = nbinom2(link = "log"),
  data = data_clean
)

summary(M3_glmmTMB)


# Standardize predictors
data <- data %>%
  mutate(
    ideology_percentile_scaled = scale(ideology_percentile),
    demonstration_intensity_israel_scaled = scale(demonstration_intensity_israel),
    demonstration_intensity_total_7_after_scaled = scale(demonstration_intensity_total_7_after)
  )

# Refit with scaled predictors
M3_scaled <- femlm(
  communication_window_7 ~ demonstration_intensity_israel_scaled + 
    ideology_percentile_scaled * demonstration_intensity_israel_scaled + 
    demonstration_intensity_total_7_after_scaled | bioguide_id + date, 
  data = data
)

summary(M3_scaled)

library(glmmTMB)


# Fit Negative Binomial model with random effects
M1_glmmTMB <- glmmTMB(
  communication_window_7 ~ demonstration_intensity_israel + 
    ideology_percentile * demonstration_intensity_israel,
  family = nbinom2(link = "log"),
  data = data
)

summary(M1_glmmTMB)

# Fit Negative Binomial model with random effects
M2_glmmTMB <- glmmTMB(
  communication_window_7 ~ demonstration_intensity_israel + 
    ideology_percentile * demonstration_intensity_israel + 
    demonstration_intensity_total_7_after,
  family = nbinom2(link = "log"),
  data = data
)

summary(M2_glmmTMB)


# Fit Negative Binomial model with random effects
M1_glmmTMB <- glmmTMB(
  communication_window_7 ~ demonstration_intensity_israel + 
    ideology_percentile * demonstration_intensity_israel,
  family = nbinom2(link = "log"),
  data = data
)

summary(M1_glmmTMB)


# Fit Negative Binomial model with random effects
M2_glmmTMB <- glmmTMB(
  communication_window_7 ~ demonstration_intensity_israel + 
    ideology_percentile * demonstration_intensity_israel + 
    demonstration_intensity_total_7_after,
  family = nbinom2(link = "log"),
  data = data
)

summary(M2_glmmTMB)


# Fit Negative Binomial model with random effects
M3_glmmTMB <- glmmTMB(
  communication_window_7 ~ demonstration_intensity_israel + 
    ideology_percentile * demonstration_intensity_israel + 
    demonstration_intensity_total_7_after + (1 | bioguide_id) + (1 | date),
  family = nbinom2(link = "log"),
  data = data
)

summary(M3_glmmTMB)


library(fixest)

# Fixed Effects Model
M3_fixed <- femlm(
  communication_window_7 ~ demonstration_intensity_israel + 
    ideology_percentile * demonstration_intensity_israel + 
    demonstration_intensity_total_7_after | bioguide_id + date,
  data = data,
  dist = "negbin"
)

summary(M3_fixed)

# Coefficients from Random Effects Model
coef_random <- fixef(M3_glmmTMB)

# Coefficients from Fixed Effects Model
coef_fixed <- coef(M3_fixed)


library(plm)
library(plm)

# Define panel data structure
pdata <- pdata.frame(
  data,
  index = c("bioguide_id", "date")  # Replace with your panel identifiers
)

# Fixed Effects Model
fixed_model <- plm(
  communication_window_7 ~ demonstration_intensity_israel + 
    ideology_percentile * demonstration_intensity_israel + 
    demonstration_intensity_total_7_after,
  data = pdata,
  model = "within"  # Fixed effects
)

# Random Effects Model
random_model <- plm(
  communication_window_7 ~ demonstration_intensity_israel + 
    ideology_percentile * demonstration_intensity_israel + 
    demonstration_intensity_total_7_after,
  data = pdata,
  model = "random"  # Random effects
)

# Hausman Test
hausman_test <- phtest(fixed_model, random_model)

# Print the results
print(hausman_test)

# p ≥ 0.05: Fail to reject the null hypothesis → Random effects are acceptable.

# Poisson regression with fixed effects using fixest
M3_poisson <- feglm(
  communication_window_7 ~ demonstration_intensity_israel + 
    ideology_percentile * demonstration_intensity_israel + 
    demonstration_intensity_total_7_after | bioguide_id + date, 
  family = poisson(link = "log"),
  data = data_clean
)


M1_kons_glmmTMB <- glmmTMB(
  communication_window_7 ~ demonstration_intensity_israel,
  family = nbinom2(link = "log"),
  data = data_conservative_15
)

summary(M1_kons_glmmTMB)


M2_kons_glmmTMB <- glmmTMB(
  communication_window_7 ~ demonstration_intensity_israel + 
    demonstration_intensity_total_7_after,
  family = nbinom2(link = "log"),
  data = data_conservative_15
)

summary(M2_kons_glmmTMB)


M3_kons_glmmTMB <- glmmTMB(
  communication_window_7 ~ demonstration_intensity_israel + 
    demonstration_intensity_total_7_after | bioguide_id + date,
  family = nbinom2(link = "log"),
  data = data_conservative_15
)

summary(M3_kons_glmmTMB)


# Summary of the model
summary(M3_poisson)

M3_negbin <- femlm(
  communication_window_7 ~ demonstration_intensity_israel + demonstration_intensity_total_7_after | bioguide_id + date, 
  data = data,
  dist = "negbin"  # Specify negative binomial distribution
)

# Summary of the model
summary(M3_negbin)

# Create dummy variables for fixed effects
data <- data %>%
  mutate(
    bioguide_id_dummy = factor(bioguide_id),
    date_dummy = factor(date)
  )

# Fit ZINB with dummy variables
zinb_model <- zeroinfl(
  communication_window_7 ~ demonstration_intensity_israel + demonstration_intensity_total_7_after +
    bioguide_id_dummy + date_dummy | demonstration_intensity_israel,
  data = data,
  dist = "negbin"
)

# Summary
summary(zinb_model)





