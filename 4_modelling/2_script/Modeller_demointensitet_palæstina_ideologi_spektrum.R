### Modeller_demointensitet_palestine_ideologi_spektrum

# Indlæser nødvendige pakker og data -----------------------------------------------------------
setwd("/Users/carlgammeltoft/Lokale dokumenter/R_speciale/4_modelling/")
data <- read.csv("1_input/data_finalized.csv")

# Load required libraries
library(ggplot2)
library(dplyr)
library(fixest)
library(patchwork)


# PRO-palestine + KONSERVATIVE -----------------------------------------------
# Define percentiles
percentiles <- seq(0.50, 0.95, by = 0.05)  # Percentiles: 0.5, 0.6, 0.7, 0.8, 0.9

# Initialize a data frame to store results
results_palestine <- data.frame(
  percentile = numeric(),
  estimate = numeric(),
  conf_low = numeric(),
  conf_high = numeric()
)

# Loop over each percentile range
for (p in percentiles) {
  
  # Filter data for the current percentile range
  subset_data <- data %>%
    filter(ideology_percentile_rep >= p)           # Filter for ideology percentiles
  #  filter(!is.na(communication_window_7) & communication_window_7 > 0)  # Keep valid outcome data
  
  # Skip if the subset is empty
  if (nrow(subset_data) == 0) {
    print(paste("Skipping percentile:", p, "- No valid data"))
    next
  }
  
  # Run the regression with pro-palestine protests
  model <- tryCatch({
    femlm(
      communication_window_7 ~ demonstration_intensity_palestine + demonstration_intensity_total_7_after | bioguide_id + date, 
      data = subset_data
    )
  }, error = function(e) {
    print(paste("Model failed for percentile:", p, "-", e$message))
    NULL
  })  # Skip on errors
  
  # Skip if the model failed
  if (is.null(model)) {
    next
  }
  
  # Extract coefficients and compute confidence intervals
  coef_table <- summary(model)$coeftable
  
  if ("demonstration_intensity_palestine" %in% rownames(coef_table)) {
    estimate <- coef_table["demonstration_intensity_palestine", "Estimate"]
    std_error <- coef_table["demonstration_intensity_palestine", "Std. Error"]
    
    # Append results
    results_palestine <- rbind(
      results_palestine,
      data.frame(
        percentile = p,
        estimate = estimate,
        conf_low = estimate - 1.96 * std_error,  # Lower 95% confidence interval
        conf_high = estimate + 1.96 * std_error  # Upper 95% confidence interval
      )
    )
  } else {
    print(paste("Coefficient for demonstration_intensity_palestine missing at percentile:", p))
  }
}

# View the results
print(results_palestine)

# Update x-axis labels
filtered_results <- results_palestine %>%
  mutate(
    group = factor(percentile, 
                   levels = c(0.95, 0.90, 0.85, 0.80, 0.75, 0.70, 0.65, 0.60, 0.55, 0.50),
                   labels = c("< 5%", "< 10%", "< 15%", "< 20%", "< 25%", "< 30%", "< 35%", "< 40%", "< 45%", "< 50%"))
  )

filtered_results_kons <- filtered_results


library(dplyr)
library(knitr)
library(kableExtra)

filtered_results_kons %>%
  dplyr::select(
    `Ideologisk rangering` = group,
    Koefficient            = estimate,
    `CI nederst (95%)`     = conf_low,
    `CI øverst (95%)`      = conf_high
  ) %>%
  knitr::kable(
    format    = "latex",
    booktabs  = TRUE,
    caption   = "Effekt af pro-palæstinensiske demonstrationer på kommunikation blandt konservative politikere",
    label     = "tab:C_palestina_kons",
    digits    = 3,
    escape    = FALSE
  ) %>%
  kableExtra::kable_styling(latex_options = "hold_position")

# Final plot
final_plot_kons <- ggplot(filtered_results, aes(
  x = group, 
  y = estimate, 
  ymin = conf_low, 
  ymax = conf_high
)) +
  # Confidence intervals (error bars)
  geom_errorbar(position = position_dodge(width = 0.35), width = 0.15, color = "black") + 
  # Points with confidence intervals
  geom_pointrange(position = position_dodge(width = 0.35), size = 0.5, color = "black", fatten = 4) +
  # Reference line at zero
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  # Labels
  labs(
    title = "Pro-palestineske demonstrationer",
    x = "Ideologisk rangering",
    y = "Koefficient"
  ) +
  # Minimalist theme
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",  # Remove legend
    plot.title = element_text(hjust = 0.5)  # Center title
  )

# Display the plot
print(final_plot_kons)


# PRO-PALÆSTINA + KONSERVATIVE -----------------------------------------------
# Define percentiles
percentiles <- seq(0.05, 0.5, by = 0.05) 

# Initialize a data frame to store results
results_palestine <- data.frame(
  percentile = numeric(),
  estimate = numeric(),
  conf_low = numeric(),
  conf_high = numeric()
)

# Loop over each percentile range
for (p in percentiles) {
  
  # Filter data for the current percentile range
  subset_data <- data %>%
    filter(ideology_percentile_rep <= p)           # Filter for ideology percentiles
  #  filter(!is.na(communication_window_7) & communication_window_7 > 0)  # Keep valid outcome data
  
  # Skip if the subset is empty
  if (nrow(subset_data) == 0) {
    print(paste("Skipping percentile:", p, "- No valid data"))
    next
  }
  
  # Run the regression with pro-palestine protests
  model <- tryCatch({
    femlm(
      communication_window_7 ~ demonstration_intensity_palestine + demonstration_intensity_total_7_after | bioguide_id + date, 
      data = subset_data
    )
  }, error = function(e) {
    print(paste("Model failed for percentile:", p, "-", e$message))
    NULL
  })  # Skip on errors
  
  # Skip if the model failed
  if (is.null(model)) {
    next
  }
  
  # Extract coefficients and compute confidence intervals
  coef_table <- summary(model)$coeftable
  
  if ("demonstration_intensity_palestine" %in% rownames(coef_table)) {
    estimate <- coef_table["demonstration_intensity_palestine", "Estimate"]
    std_error <- coef_table["demonstration_intensity_palestine", "Std. Error"]
    
    # Append results
    results_palestine <- rbind(
      results_palestine,
      data.frame(
        percentile = p,
        estimate = estimate,
        conf_low = estimate - 1.96 * std_error,  # Lower 95% confidence interval
        conf_high = estimate + 1.96 * std_error  # Upper 95% confidence interval
      )
    )
  } else {
    print(paste("Coefficient for demonstration_intensity_palestine missing at percentile:", p))
  }
}

# View the results
print(results_palestine)

# Update x-axis labels (reversed order)
filtered_results <- results_palestine %>%
  mutate(
    group = factor(percentile, 
                   levels = c(0.50, 0.45, 0.40, 0.35, 0.30, 0.25, 0.20, 0.15, 0.10, 0.05),
                   labels = c("< 50%", "< 45%", "< 40%", "< 35%", "< 30%", "< 25%", "< 20%", "< 15%", "< 10%", "< 5%"))
  )

filtered_results_lib <- filtered_results

filtered_results_lib %>%
  dplyr::select(
    `Ideologisk rangering` = group,
    Koefficient            = estimate,
    `CI nederst (95%)`     = conf_low,
    `CI øverst (95%)`      = conf_high
  ) %>%
  knitr::kable(
    format    = "latex",
    booktabs  = TRUE,
    caption   = "Effekt af pro-palæstinensiske demonstrationer på kommunikation blandt liberale politikere",
    label     = "tab:C_palestina_lib",
    digits    = 3,
    escape    = FALSE
  ) %>%
  kableExtra::kable_styling(latex_options = "hold_position")


# Final plot
final_plot_lib <- ggplot(filtered_results, aes(
  x = group, 
  y = estimate, 
  ymin = conf_low, 
  ymax = conf_high
)) +
  # Confidence intervals (error bars)
  geom_errorbar(position = position_dodge(width = 0.35), width = 0.15, color = "black") + 
  # Points with confidence intervals
  geom_pointrange(position = position_dodge(width = 0.35), size = 0.5, color = "black", fatten = 4) +
  # Reference line at zero
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  # Labels
  labs(
    title = "Pro-palestineske demonstrationer",
    x = "Ideologisk rangering",
    y = "Koefficient"
  ) +
  # Minimalist theme
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",  # Remove legend
    plot.title = element_text(hjust = 0.5)  # Center title
  )

# Display the plot
print(final_plot_lib)

# SAMMENSÆTTER PLOTS ------------------------------------------------------

# Ensure consistent y-axis limits
final_plot_kons <- final_plot_kons + 
  scale_y_continuous(limits = c(min(filtered_results_kons$conf_low, filtered_results_lib$conf_low), 
                                max(filtered_results_kons$conf_high, filtered_results_lib$conf_high)))

final_plot_lib <- final_plot_lib + 
  scale_y_continuous(limits = c(min(filtered_results_kons$conf_low, filtered_results_lib$conf_low), 
                                max(filtered_results_kons$conf_high, filtered_results_lib$conf_high)))

# Fjern y-akse label fra det liberale plot
final_plot_lib <- final_plot_lib + 
  labs(y = NULL, title = "Liberale politikere")  # Opdater titel

# Opdater titel for det konservative plot
final_plot_kons <- final_plot_kons + 
  labs(title = "Konservative politikere")  # Opdater titel

# Kombinér plots side om side med fælles overskrift i midten
combined_plot <- final_plot_kons + final_plot_lib +
  plot_layout(ncol = 2, guides = "collect") +  # Justér layout
  plot_annotation(
    title = "Pro-palæstineske demonstrationer",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 16))  # Center og justér titel
  )

# Vis det kombinerede plot
print(combined_plot)

# Gemmer figuren
ggsave(
  filename = "/Users/carlgammeltoft/Lokale dokumenter/R_speciale/4_modelling/3_output/plots_demointensitet_ideologi_kontinuert/pro_palæstina.pdf",
  plot = combined_plot,
  width = 12, height = 6
)


#
library(MASS)
pois <- glm(communication_window_7 ~ demonstration_intensity_total_7_after,
             family = poisson(), data = data)

nb <- glm.nb(communication_window_7 ~ demonstration_intensity_total_7_after, data = data)

library(lmtest)
lrtest(pois, nb)
