# Indlæser data og pakker ---------------------------------------------------
setwd("/Users/carlgammeltoft/Lokale dokumenter/R_speciale/4_modelling/")
data <- read.csv("1_input/data_finalized.csv")

library(ggplot2)
library(dplyr)
library(fixest)

# Definer funktion til at køre modeller -------------------------------------
run_model <- function(data, percentiles, normative_filter = FALSE) {
  results <- data.frame(
    percentile = numeric(),
    estimate = numeric(),
    conf_low = numeric(),
    conf_high = numeric(),
    demo_type = character()  # Tilføj en kolonne for typen af demonstrationer
  )
  
  for (p in percentiles) {
    # Filter data for den aktuelle percentil
    subset_data <- data %>%
      filter(ideology_percentile_rep >= p)  # Filter for konservative (kan justeres for liberale)
    
    if (normative_filter) {
      subset_data <- subset_data %>%
        filter((count_protest == 0) | (count_protest > 0 & non_normative_palestine_protests != 0)) %>%  # Filter for non-normative demonstrationer
        filter((count_protest == 0) | (count_protest > 0 & non_normative_israel_protests != 0))
    }
    
    if (nrow(subset_data) == 0) {
      print(paste("Skipping percentile:", p, "- No valid data"))
      next
    }
    
    # Kør regression
    model <- tryCatch({
      femlm(
        communication_window_7 ~ demonstration_intensity_palestine + demonstration_intensity_total_7_after | bioguide_id + date, 
        data = subset_data
      )
    }, error = function(e) {
      print(paste("Model failed for percentile:", p, "-", e$message))
      NULL
    })
    
    if (is.null(model)) next
    
    # Udtræk koefficienter
    coef_table <- summary(model)$coeftable
    if ("demonstration_intensity_palestine" %in% rownames(coef_table)) {
      estimate <- coef_table["demonstration_intensity_palestine", "Estimate"]
      std_error <- coef_table["demonstration_intensity_palestine", "Std. Error"]
      
      # Tilføj resultater
      results <- rbind(
        results,
        data.frame(
          percentile = p,
          estimate = estimate,
          conf_low = estimate - 1.96 * std_error,
          conf_high = estimate + 1.96 * std_error,
          demo_type = ifelse(normative_filter, "Ikke-normative", "Alle")
        )
      )
    }
  }
  return(results)
}

# Kør modeller for alle og normative demonstrationer -------------------------
percentiles_kons <- seq(0.50, 1, by = 0.1)
percentiles_lib <- seq(0.1, 0.50, by = 0.1)

# Konservative
results_kons_alle <- run_model(data, percentiles_kons, normative_filter = FALSE)
results_kons_normative <- run_model(data, percentiles_kons, normative_filter = TRUE)

# Liberale
results_lib_alle <- run_model(data, percentiles_lib, normative_filter = FALSE)
results_lib_normative <- run_model(data, percentiles_lib, normative_filter = TRUE)

# Kombiner resultater -------------------------------------------------------
results_kons <- rbind(results_kons_alle, results_kons_normative) %>%
  mutate(group = factor(percentile, 
                        levels = rev(percentiles_kons),
                        labels = paste0("< ", seq(0, 50, by = 10), "%")))

results_lib <- rbind(results_lib_alle, results_lib_normative) %>%
  mutate(group = factor(percentile, 
                        levels = percentiles_lib,
                        labels = paste0("< ", seq(50, 0.1, by = -10), "%")))

# Plot for konservative -----------------------------------------------------
plot_kons <- ggplot(results_kons, aes(
  x = group, 
  y = estimate, 
  ymin = conf_low, 
  ymax = conf_high,
  shape = demo_type
)) +
  geom_errorbar(position = position_dodge(width = 0.4), width = 0.2, color = "black") +
  geom_point(position = position_dodge(width = 0.4), size = 2.5, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Konservative politikere",
    x = "Ideologisk rangering",
    y = "Koefficient",
    shape = "Demonstrationstype"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

# Plot for liberale ---------------------------------------------------------
plot_lib <- ggplot(results_lib, aes(
  x = group, 
  y = estimate, 
  ymin = conf_low, 
  ymax = conf_high,
  shape = demo_type
)) +
  geom_errorbar(position = position_dodge(width = 0.4), width = 0.2, color = "black") +
  geom_point(position = position_dodge(width = 0.4), size = 2.5, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Liberale politikere",
    x = "Ideologisk rangering",
    y = NULL,
    shape = "Demonstrationstype"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

# Kombinér plots med fælles legend -------------------------------------------
# Beregn fælles y-aksegrænser
common_y_limits <- c(
  min(results_kons$conf_low, results_lib$conf_low, na.rm = TRUE),
  max(results_kons$conf_high, results_lib$conf_high, na.rm = TRUE)
)

# Opdater y-aksegrænser for de to plots
plot_kons <- plot_kons + 
  scale_y_continuous(limits = common_y_limits) +
  labs(title = "Konservative politikere")

plot_lib <- plot_lib + 
  scale_y_continuous(limits = common_y_limits) +
  labs(y = NULL, title = "Liberale politikere")  # Fjern y-akse etiketten for liberale

# Kombinér plots side om side med fælles titel og legend i bunden
library(patchwork)

combined_plot <- plot_kons + plot_lib +
  plot_layout(ncol = 2, guides = "collect") +  # Ensartet layout og fælles legend
  plot_annotation(
    title = "Pro-palæstinensiske demonstrationer",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 16))  # Center titel
  ) &
  theme(
    legend.position = "bottom",            # Placér legend i bunden
    panel.grid.major = element_blank(),    # Fjern primære gitterlinjer
    panel.grid.minor = element_blank()     # Fjern sekundære gitterlinjer
  )

# Vis det kombinerede plot
print(combined_plot)


# Gem figuren
ggsave(
  filename = "/Users/carlgammeltoft/Lokale dokumenter/R_speciale/4_modelling/3_output/plots_non_normative/pro-palæstinensiske.pdf",
  plot = combined_plot,
  width = 12, height = 6
)

# 1) LaTeX‐tabel for *normative* pro-palæstinensiske demonstrationer blandt konservative
results_kons_normative %>%
  # Giv procent‐intervaller pæne labels
  mutate(
    group = factor(
      percentile,
      levels = rev(percentiles_kons),
      labels = paste0("< ", rev(percentiles_kons * 100), "%")
    )
  ) %>%
  # Vælg og omdøb kolonner
  dplyr::select(
    `Ideologisk rangering` = group,
    Koefficient            = estimate,
    `CI nederst (95%)`     = conf_low,
    `CI øverst (95%)`      = conf_high
  ) %>%
  # Lav LaTeX‐tabel
  kable(
    format    = "latex",
    booktabs  = TRUE,
    caption   = "Effekt af pro-palæstinensiske *normative* demonstrationer på politisk kommunikation blandt konservative politikere",
    label     = "tab:C_normative_palestina_kons",
    digits    = 3,
    escape    = FALSE,
    align     = c("l","r","r","r")
  ) %>%
  add_header_above(c(" " = 1, "95\\% konfidensinterval" = 2)) %>%
  kable_styling(latex_options = c("hold_position","scale_down")) %>%
  cat()


# 2) LaTeX‐tabel for *normative* pro-palæstinensiske demonstrationer blandt liberale
results_lib_normative %>%
  mutate(
    group = factor(
      percentile,
      levels = rev(percentiles_lib),
      labels = paste0("< ", rev(percentiles_lib * 100), "%")
    )
  ) %>%
  dplyr::select(
    `Ideologisk rangering` = group,
    Koefficient            = estimate,
    `CI nederst (95%)`     = conf_low,
    `CI øverst (95%)`      = conf_high
  ) %>%
  kable(
    format    = "latex",
    booktabs  = TRUE,
    caption   = "Effekt af pro-palæstinensiske *normative* demonstrationer på politisk kommunikation blandt liberale politikere",
    label     = "tab:C_normative_palestina_lib",
    digits    = 3,
    escape    = FALSE,
    align     = c("l","r","r","r")
  ) %>%
  add_header_above(c(" " = 1, "95\\% konfidensinterval" = 2)) %>%
  kable_styling(latex_options = c("hold_position","scale_down")) %>%
  cat()
