# Load nødvendige pakker
library(ggplot2)
library(fixest)
library(tidyverse)

# Indlæser data
setwd("/Users/carlgammeltoft/Lokale dokumenter/R_speciale/4_modelling/")
data <- read.csv("1_input/data_finalized.csv")

# Resultatliste
results <- list()

# Funktion til at køre modeller med ideologiske intervaller
run_model <- function(data, ideology_min = NULL, ideology_max = NULL, relevant_committee_filter = NULL, model_name) {
  
  # Filtrering af data
  test_subset_data <- data
  
  if (!is.null(ideology_min) & !is.null(ideology_max)) {
    test_subset_data <- test_subset_data %>% 
      filter(ideology_percentile > ideology_min, ideology_percentile <= ideology_max)
  }
  
  if (!is.null(relevant_committee_filter)) {
    test_subset_data <- test_subset_data %>% 
      filter(relevant_committee == relevant_committee_filter)
  }
  
  # Kør modellen
  model <- femlm(
    communication_window_7 ~ demonstration_intensity_palestine + demonstration_intensity_total_7_after | bioguide_id + date, 
    family = "negbin",
    data = test_subset_data
  )
  
  # Udtræk resultater
  coef <- summary(model)$coeftable["demonstration_intensity_palestine", ]
  return(data.frame(
    model = model_name,
    estimate = coef["Estimate"],
    conf_low = coef["Estimate"] - 1.96 * coef["Std. Error"],
    conf_high = coef["Estimate"] + 1.96 * coef["Std. Error"]
  ))
}

# Kør modeller for ideologiske intervaller (0-20%, 20-40%, ..., 80-100%)
results <- rbind(
  results,
  run_model(data, ideology_min = 0, ideology_max = 0.2, relevant_committee_filter = "1", model_name = "0-20% - Relevant Komite"),
  run_model(data, ideology_min = 0.2, ideology_max = 0.4, relevant_committee_filter = "1", model_name = "20-40% - Relevant Komite"),
  run_model(data, ideology_min = 0.4, ideology_max = 0.6, relevant_committee_filter = "1", model_name = "40-60% - Relevant Komite"),
  run_model(data, ideology_min = 0.6, ideology_max = 0.8, relevant_committee_filter = "1", model_name = "60-80% - Relevant Komite"),
  run_model(data, ideology_min = 0.8, ideology_max = 1, relevant_committee_filter = "1", model_name = "80-100% - Relevant Komite"),
  run_model(data, ideology_min = 0, ideology_max = 0.2, relevant_committee_filter = NULL, model_name = "0-20% - Alle Komiteer"),
  run_model(data, ideology_min = 0.2, ideology_max = 0.4, relevant_committee_filter = NULL, model_name = "20-40% - Alle Komiteer"),
  run_model(data, ideology_min = 0.4, ideology_max = 0.6, relevant_committee_filter = NULL, model_name = "40-60% - Alle Komiteer"),
  run_model(data, ideology_min = 0.6, ideology_max = 0.8, relevant_committee_filter = NULL, model_name = "60-80% - Alle Komiteer"),
  run_model(data, ideology_min = 0.8, ideology_max = 1, relevant_committee_filter = NULL, model_name = "80-100% - Alle Komiteer")
)

# Konverter resultater til dataframe
results_df <- as.data.frame(results)

# Tilføj kategori (Relevant Komite / Alle Komiteer)
results_df <- results_df %>%
  mutate(category = ifelse(grepl("Relevant Komite", model), "Relevant Komite", "Alle Komiteer"))

# Tilføj ideologiske grupper som faktor
results_df$group <- factor(case_when(
  grepl("0-20%", results_df$model) ~ "0-20%",
  grepl("20-40%", results_df$model) ~ "20-40%",
  grepl("40-60%", results_df$model) ~ "40-60%",
  grepl("60-80%", results_df$model) ~ "60-80%",
  grepl("80-100%", results_df$model) ~ "80-100%"
), levels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"))

# Plot med akademisk stil
ggplot(results_df, aes(x = group, y = estimate, shape = category)) +
  geom_point(position = position_dodge(width = 0.5), size = 3, color = "black") +
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), 
                position = position_dodge(width = 0.5), width = 0.2, color = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    title = "Effekt af Pro-palestine Demonstrationer på Politikerkommunikation",
    x = "Ideologisk Percentilgruppe",
    y = "Estimate (med 95% Konfidensinterval)",
    shape = "Komitetype"
  ) +
  scale_shape_manual(values = c("Relevant Komite" = 16, "Alle Komiteer" = 17)) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(size = 10, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )
