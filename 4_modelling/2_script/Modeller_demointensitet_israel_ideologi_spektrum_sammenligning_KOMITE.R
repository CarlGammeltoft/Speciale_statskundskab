# Load nødvendige pakker
library(ggplot2)
library(fixest)
library(tidyverse)

# Indlæser data
setwd("/Users/carlgammeltoft/Lokale dokumenter/R_speciale/4_modelling/")
data <- read.csv("1_input/data_finalized.csv")

# Resultatliste
results <- list()

# Funktion til modeller
run_model <- function(data, ideology_filter = NULL, relevant_committee_filter = NULL, model_name) {
  
  test_subset_data <- data
  
  # Filtrering for ideologi
  if (!is.null(ideology_filter)) {
    if (ideology_filter == "liberale") {
      test_subset_data <- test_subset_data %>% filter(ideology_percentile <= 0.5)
    } else if (ideology_filter == "konservative") {
      test_subset_data <- test_subset_data %>% filter(ideology_percentile > 0.5)
    }
  }
  
  # Filtrering for komite
  if (!is.null(relevant_committee_filter)) {
    test_subset_data <- test_subset_data %>% filter(relevant_committee == relevant_committee_filter)
  }
  
  # Estimer modellen
  model <- femlm(
    communication_window_7 ~ demonstration_intensity_israel + demonstration_intensity_total_7_after | bioguide_id + date, 
    family = "negbin",
    data = test_subset_data
  )
  
  # Udtræk resultater
  coef <- summary(model)$coeftable["demonstration_intensity_israel", ]
  return(data.frame(
    model = model_name,
    estimate = coef["Estimate"],
    conf_low = coef["Estimate"] - 1.96 * coef["Std. Error"],
    conf_high = coef["Estimate"] + 1.96 * coef["Std. Error"]
  ))
}

# Kør modeller
results <- rbind(
  results,
  run_model(data, ideology_filter = "liberale", relevant_committee_filter = "1", model_name = "relevant komite - 50% Liberale"),
  run_model(data, ideology_filter = NULL, relevant_committee_filter = "1", model_name = "relevant komite - Alle Politikere"),
  run_model(data, ideology_filter = "konservative", relevant_committee_filter = "1", model_name = "relevant komite - 50% Konservative"),
  run_model(data, ideology_filter = "liberale", relevant_committee_filter = NULL, model_name = "alle komiteer - 50% Liberale"),
  run_model(data, ideology_filter = NULL, relevant_committee_filter = NULL, model_name = "alle komiteer - Alle Politikere"),
  run_model(data, ideology_filter = "konservative", relevant_committee_filter = NULL, model_name = "alle komiteer - 50% Konservative")
)

# Resultater som dataframe
results_df <- as.data.frame(results)

# Definer rækkefølge for modeller
results_df$model <- factor(results_df$model, levels = c(
  "alle komiteer - 50% Liberale",
  "alle komiteer - 50% Konservative",
  "alle komiteer - Alle Politikere",
  "relevant komite - 50% Liberale",
  "relevant komite - 50% Konservative",
  "relevant komite - Alle Politikere"
))

# Tilføj kategori
results_df <- results_df %>%
  mutate(category = ifelse(grepl("relevant komite", model), "Relevante komiteer", "Alle komiteer"))

# Gruppe-definition
results_df$group <- factor(case_when(
  grepl("Liberale", results_df$model) ~ "Liberale",
  grepl("Konservative", results_df$model) ~ "Konservative",
  grepl("Alle Politikere", results_df$model) ~ "Alle Politikere"
), levels = c("Liberale", "Konservative", "Alle Politikere"))

# Plot estimater med konfidensintervaller
israel_plot <- ggplot(results_df, aes(x = group, y = estimate, shape = category)) +
  # Punkter
  geom_point(position = position_dodge(width = 0.35), size = 2.5, color = "black") +
  # Konfidensintervaller
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), 
                position = position_dodge(width = 0.35), width = 0.1, color = "black") +
  # Stiplet reference linje
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  annotate("rect", xmin = 2.5, xmax = 3.5, ymin = -Inf, ymax = Inf, fill = "lightgrey", alpha = 0.4) +
  # Labels
  labs(
    title = "Pro-israelske demonstrationer",
    x = "Ideologisk gruppe",
    y = "Koefficient",
    shape = "Komitetype"
  ) +
  # Former til punkter
  scale_shape_manual(values = c("Relevante komiteer" = 15, "Alle komiteer" = 16)) +
  # Minimalistisk tema
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(size = 10, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12, color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    panel.grid.major = element_blank(),  # Fjern gridlinjer for et renere look
    panel.grid.minor = element_blank(),  # Fjern mindre gridlinjer
    legend.position = "bottom",          # Flyt legend til bunden
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10)
  )

israel_plot
