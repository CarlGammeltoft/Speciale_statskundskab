# Indlæser nødvendige pakker og data -----------------------------------------------------------
setwd("/Users/carlgammeltoft/Lokale dokumenter/R_speciale/4_modelling/")
data <- read.csv("1_input/data_finalized.csv")
model_list_ <- readRDS("1_input/saved_models_logit.RDS")

library(tidyverse)
library(fixest)
library(patchwork)
library(marginaleffects)
library(margins)

### Laver nye varaible
# Opdel ideologivariablen i kategorier
data <- data %>%
  mutate(
    no_communication = ifelse(communication_window_7 == 0, 1, 0),  # Ingen kommunikation = 1
    communication = ifelse(communication_window_7 == 1, 1, 0),  # Kommunikation = 1
    demonstration_intensity_israel_centered = demonstration_intensity_israel - mean(demonstration_intensity_israel, na.rm = TRUE),
    demo_israel_binary = ifelse(count_israel_protests >= 1, 1, 0),
    demo_palestine_binary = ifelse(count_palestine_protests >= 1, 1, 0),
    ideology_percentile_rep_cat = case_when(
      ideology_percentile_rep >= 0 & ideology_percentile_rep < 0.10 ~ 0.10,
      ideology_percentile_rep >= 0.10 & ideology_percentile_rep < 0.20 ~ 0.20,
      ideology_percentile_rep >= 0.20 & ideology_percentile_rep < 0.30 ~ 0.30,
      ideology_percentile_rep >= 0.30 & ideology_percentile_rep < 0.40 ~ 0.40,
      ideology_percentile_rep >= 0.40 & ideology_percentile_rep < 0.50 ~ 0.50,
      ideology_percentile_rep >= 0.50 & ideology_percentile_rep < 0.60 ~ 0.60,
      ideology_percentile_rep >= 0.60 & ideology_percentile_rep < 0.70 ~ 0.70,
      ideology_percentile_rep >= 0.70 & ideology_percentile_rep < 0.80 ~ 0.80,
      ideology_percentile_rep >= 0.80 & ideology_percentile_rep < 0.90 ~ 0.90,
      ideology_percentile_rep >= 0.90 & ideology_percentile_rep <= 1 ~ 1.00,
      TRUE ~ NA_real_,  # Sætter NA, hvis værdien er udenfor intervallet
    )
  )


# PRO-ISRAEL + ALLE -----------------------------------------------
# Define percentiles
percentiles <- seq(0, 0.9, by = 0.1)  # Percentiles: 0 - 0.9

# Initialize a data frame to store results
results_israel <- data.frame(
  percentile_start = numeric(),
  percentile_end = numeric(),
  estimate = numeric(),
  conf_low = numeric(),
  conf_high = numeric()
)

# Loop over each percentile range
for (p in percentiles) {
  # Define the current range (e.g., 0.00–0.10, 0.10–0.20)
  lower_bound <- p
  upper_bound <- p + 0.10
  
  # Filter data for the current percentile range
  subset_data <- data %>%
    filter(ideology_percentile >= lower_bound & ideology_percentile < upper_bound)
  
  # Skip if the subset is empty
  if (nrow(subset_data) == 0 || all(is.na(subset_data$communication_window_14))) {
    next
  }
  
  # Run the regression with pro-Israel protests
  model <- tryCatch({
    feglm(
      no_communication ~ demo_israel_binary + demonstration_intensity_total_7_after | bioguide_id + date, 
      data = subset_data, 
      family = binomial(link = "logit")
    )
  }, error = function(e) {
    print(paste("Model failed for percentile:", p, "-", e$message))
    NULL
  })  # Skip on errors
  
  # Skip if the model failed
  if (is.null(model)) {
    next
  }
  
  # Get the AME using avg_slopes() after fitting the model
  avg <- avg_slopes(model)
  
  # Append the results (estimate and confidence intervals for AME)
  results_israel <- rbind(
    results_israel,
    data.frame(
      percentile_start = lower_bound,
      percentile_end = upper_bound,
      estimate = avg$estimate[1],
      conf_low = avg$conf.low[1],
      conf_high = avg$conf.high[1]
    )
  )
}

# View the final results
print(results_israel)

# Opret kategorisk variabel for percentiler
results_israel <- results_israel %>%
  mutate(percentile_label = paste0(percentile_start * 100, "-", percentile_end * 100, "%"))

# Plot AMEs med fejlbarer (konfidensintervaller)
final_plot_israel <- ggplot(results_israel, aes(x = factor(percentile_label, levels = unique(percentile_label)), y = estimate)) +
  # Fejlbarer for konfidensintervallet
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.1, color = "black") +
  # Punkter for estimerede AMEs
  geom_point(size = 2, color = "black") +
  # Tilføj stiplet linje ved y = 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  # Læg til labels og plotindstillinger
  labs(
    title = "Pro-israelske demonstrationer",
    x = "Ideologisk rangering",
    y = "Non-kommunikation"
  ) +
  theme_minimal() +
  # Justering af akselabels
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # Læg tekst på skrå for bedre synlighed
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

final_plot_israel

# PRO-PALESTINENSKE + ALLE -----------------------------------------------
# Define percentiles
percentiles <- seq(0, 0.9, by = 0.1)  # Percentiles: 0 - 0.9

# Initialize a data frame to store results
results_palestine <- data.frame(
  percentile_start = numeric(),
  percentile_end = numeric(),
  estimate = numeric(),
  conf_low = numeric(),
  conf_high = numeric()
)

# Loop over each percentile range
for (p in percentiles) {
  # Define the current range (e.g., 0.00–0.10, 0.10–0.20)
  lower_bound <- p
  upper_bound <- p + 0.10
  
  # Filter data for the current percentile range
  subset_data <- data %>%
    filter(ideology_percentile >= lower_bound & ideology_percentile < upper_bound)
  
  # Skip if the subset is empty
  if (nrow(subset_data) == 0 || all(is.na(subset_data$communication_window_14))) {
    next
  }
  
  # Run the regression with pro-Palestine protests
  model <- tryCatch({
    feglm(
      no_communication ~ demo_palestine_binary + demonstration_intensity_total_7_after | bioguide_id + date, 
      data = subset_data, 
      family = binomial(link = "logit")
    )
  }, error = function(e) {
    print(paste("Model failed for percentile:", p, "-", e$message))
    NULL
  })  # Skip on errors
  
  # Skip if the model failed
  if (is.null(model)) {
    next
  }
  
  # Get the AME using avg_slopes() after fitting the model
  avg <- avg_slopes(model)
  
  # Append the results (estimate and confidence intervals for AME)
  results_palestine <- rbind(
    results_palestine,
    data.frame(
      percentile_start = lower_bound,
      percentile_end = upper_bound,
      estimate = avg$estimate[1],
      conf_low = avg$conf.low[1],
      conf_high = avg$conf.high[1]
    )
  )
}

# View the final results
print(results_palestine)

# Opret kategorisk variabel for percentiler
results_palestine <- results_palestine %>%
  mutate(percentile_label = paste0(percentile_start * 100, "-", percentile_end * 100, "%"))

# Plot AMEs med fejlbarer (konfidensintervaller)
final_plot_palestine <- ggplot(results_palestine, aes(x = factor(percentile_label, levels = unique(percentile_label)), y = estimate)) +
  # Fejlbarer for konfidensintervallet
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.1, color = "black") +
  # Punkter for estimerede AMEs
  geom_point(size = 2, color = "black") +
  # Tilføj stiplet linje ved y = 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  # Læg til labels og plotindstillinger
  labs(
    title = "Pro-palæstinensiske demonstrationer",
    x = "Ideologisk rangering",
    y = "Non-kommunikation"
  ) +
  theme_minimal() +
  # Justering af akselabels
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # Læg tekst på skrå for bedre synlighed
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

final_plot_palestine

# Sammensætter plots ------------------------------------------------------
# Combine plots vertically with a shared title, centered and bold

combined_plot <- (final_plot_israel / final_plot_palestine)

# Show the combined plot
combined_plot


# Gemmer figuren
ggsave(
  filename = "/Users/carlgammeltoft/Lokale dokumenter/R_speciale/4_modelling/3_output/plots_logit/kombineret.pdf",
  plot = combined_plot,
  width = 12, height = 6
)



# 1) Tabel for pro-israelske demonstrationer ------------------------------
results_israel %>%
  # Lav en kolonne med tekstlabels “0–10%”, “10–20%” osv.
  mutate(percentile_label = paste0(percentile_start*100, "–", percentile_end*100, "%")) %>%
  # Vælg og omdøb kolonner
  dplyr::select(
    `Ideologisk rangering` = percentile_label,
    Koefficient            = estimate,
    `95\\% CI nederst`     = conf_low,
    `95\\% CI øverst`      = conf_high
  ) %>%
  # Lav LaTeX-tabellen
  kable(
    format   = "latex",
    booktabs = TRUE,
    caption  = "Marginal effekt (AME) af pro-israelske demonstrationer på sandsynligheden for ingen kommunikation (logit)",
    label    = "tab:AME_israel",
    digits   = 3,
    escape   = FALSE,
    align    = c("l","r","r","r")
  ) %>%
  kable_styling(latex_options = c("hold_position","scale_down")) %>%
  add_header_above(c(" " = 1, "95\\% konfidensinterval" = 2)) %>%
  cat()  # printer koden til konsollen

# 2) Tabel for pro-palæstinensiske demonstrationer ------------------------
results_palestine %>%
  mutate(percentile_label = paste0(percentile_start*100, "–", percentile_end*100, "%")) %>%
  dplyr::select(
    `Ideologisk rangering` = percentile_label,
    Koefficient            = estimate,
    `95\\% CI nederst`     = conf_low,
    `95\\% CI øverst`      = conf_high
  ) %>%
  kable(
    format   = "latex",
    booktabs = TRUE,
    caption  = "Marginal effekt (AME) af pro-palæstinensiske demonstrationer på sandsynligheden for ingen kommunikation (logit)",
    label    = "tab:AME_palestine",
    digits   = 3,
    escape   = FALSE,
    align    = c("l","r","r","r")
  ) %>%
  kable_styling(latex_options = c("hold_position","scale_down")) %>%
  add_header_above(c(" " = 1, "95\\% konfidensinterval" = 2)) %>%
  cat()


