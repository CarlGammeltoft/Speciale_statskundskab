# Poisson eller negbin

## Checker for overspredning
# Calculate mean and variance of the dependent variable
mean_value <- mean(data$communication_window_7, na.rm = TRUE)
variance_value <- var(data$communication_window_7, na.rm = TRUE)

cat("Mean:", mean_value, "\nVariance:", variance_value)

# Overdispersion is present if variance > mean
if (variance_value > mean_value) {
  cat("Overdispersion detected.\n")
} else {
  cat("No overdispersion detected.\n")
}


# Fit Poisson model
poisson_model <- glm(communication_window_7 ~ demonstration_intensity_palestine, 
                     family = poisson(link = "log"), 
                     data = data)

# Check model fit
summary(poisson_model)

# Check residual deviance
residual_deviance <- poisson_model$deviance
degrees_of_freedom <- poisson_model$df.residual

cat("Residual Deviance:", residual_deviance, "\nDegrees of Freedom:", degrees_of_freedom)

# If residual deviance >> degrees of freedom, overdispersion is likely
if (residual_deviance / degrees_of_freedom > 1.5) {
  cat("Overdispersion likely present.\n")
} else {
  cat("No significant overdispersion detected.\n")
}

library(AER)
# Dispersion test for Poisson model
dispersion_test(poisson_model)

# Fit negative binomial model
library(MASS)
nb_model <- glm.nb(communication_window_7 ~ demonstration_intensity_palestine, data = data)

# Compare models using AIC
cat("Poisson AIC:", AIC(poisson_model), "\nNegative Binomial AIC:", AIC(nb_model))
 
# The model with the lower AIC is preferred (negbin!)


# Histogram of the dependent variable
hist(data$communication_window_7, breaks = 20, main = "Distribution of Communication Counts",
     xlab = "Counts", col = "lightblue", border = "black")

# Overlay Poisson distribution (if mean = variance)
pois_fit <- dpois(0:max(data$communication_window_7), lambda = mean_value)
lines(0:max(data$communication_window_7), pois_fit * length(data$communication_window_7), col = "red", lwd = 2)


library(pscl)

# Fit a Zero-Inflated Poisson model
zip_model <- zeroinfl(
  communication_window_7 ~ demonstration_intensity_palestine + ideology_percentile |
    demonstration_intensity_palestine + ideology_percentile_rep,  # Binary process formula
  data = data,
  dist = "poisson"  # ZIP model
)

# View model summary
summary(zip_model)

### TESTER TRE MODELLER
# Fit Poisson model
poisson_model <- glm(
  communication_window_7 ~ demonstration_intensity_palestine + ideology_percentile,
  family = poisson(link = "log"),
  data = data
)

# Fit Negative Binomial model
library(MASS)
nb_model <- glm.nb(
  communication_window_7 ~ demonstration_intensity_palestine + ideology_percentile,
  data = data
)

# Fit Zero-Inflated Poisson model
library(pscl)
zip_model <- zeroinfl(
  communication_window_7 ~ demonstration_intensity_palestine + ideology_percentile |
    demonstration_intensity_palestine,  # Zero-inflation formula
  data = data,
  dist = "poisson"
)

# Fit Zero-Inflated Negative Binomial model
library(pscl)
zinb_model <- zeroinfl(
  communication_window_7 ~ demonstration_intensity_palestine + ideology_percentile |
    demonstration_intensity_palestine,  # Zero-inflation formula
  data = data,
  dist = "negbin"
)

summary(zinb_model)

# Extract AIC values
aic_poisson <- AIC(poisson_model)
aic_nb <- AIC(nb_model)
aic_zip <- AIC(zip_model)
aic_zinb <- AIC(zinb_model)

# Display AIC values
cat("AIC (Poisson):", aic_poisson, "\n")
cat("AIC (Negative Binomial):", aic_nb, "\n")
cat("AIC (Zero-Inflated Poisson):", aic_zip, "\n")
cat("AIC (Zero-Inflated Poisson):", aic_zinb, "\n")

# Create a data frame to summarize AIC values
aic_comparison <- data.frame(
  Model = c("Poisson", "Negative Binomial", "Zero-Inflated Poisson", "ZINB"),
  AIC = c(aic_poisson, aic_nb, aic_zip, aic_zinb)
)

# Sort by AIC
aic_comparison <- aic_comparison[order(aic_comparison$AIC), ]
print(aic_comparison)






