# Indlæser nødvendige pakker og data -----------------------------------------------------------
setwd("/Users/carlgammeltoft/Lokale dokumenter/R_speciale/4_modelling/")
data <- read.csv("1_input/data_finalized.csv")

library(fixest)
library(tidyverse)
library(patchwork)

# Definer kommunikationskanaler ---------------------------------------------------------------
communication_vars <- c(
  "communication_window_7",
  "communication_window_7_floor",
  "communication_window_7_newsletter",
  "communication_window_7_statements"
)

# Funktion til at køre modeller ---------------------------------------------------------------
run_model <- function(data, communication_var, ideology_filter, model_name) {
  # Filtrér baseret på ideologi
  subset_data <- data
  if (ideology_filter == "liberale") {
    subset_data <- subset_data %>% filter(ideology_percentile_rep <= 0.50)
  } else if (ideology_filter == "konservative") {
    subset_data <- subset_data %>% filter(ideology_percentile_rep > 0.50)
  } else if (ideology_filter == "mest_liberale") {
    subset_data <- subset_data %>% filter(ideology_percentile_rep <= 0.15)
  } else if (ideology_filter == "mest_konservative") {
    subset_data <- subset_data %>% filter(ideology_percentile_rep >= 0.85)
  } else if (ideology_filter == "alle_politikere") {
    # Ingen filtrering, tager hele datasættet
    subset_data <- subset_data
  }
  
  # Tjek for tomt datasæt
  if (nrow(subset_data) == 0) return(NULL)
  
  # Kør model
  model <- tryCatch({
    femlm(
      as.formula(paste(communication_var, "~ demonstration_intensity_palestine + demonstration_intensity_total_7_after | bioguide_id + date")),
      data = subset_data,
      family = "negbin"
    )
  }, error = function(e) NULL)
  
  if (is.null(model)) return(NULL)
  
  # Udtræk resultater
  coef <- summary(model)$coeftable["demonstration_intensity_palestine", ]
  return(data.frame(
    communication_var = communication_var,
    ideology_group = ideology_filter,  # This is correct
    model_name = model_name,
    estimate = coef["Estimate"],
    conf_low = coef["Estimate"] - 1.96 * coef["Std. Error"],
    conf_high = coef["Estimate"] + 1.96 * coef["Std. Error"]
  ))
}

# Kør modeller og saml resultater -------------------------------------------------------------
results <- list()
for (comm_var in communication_vars) {
  results <- bind_rows(
    results,
    run_model(data, comm_var, "alle_politikere", paste("Alle Politikere -", comm_var)),
    run_model(data, comm_var, "liberale", paste("Liberale -", comm_var)),
    run_model(data, comm_var, "konservative", paste("Konservative -", comm_var)),
    run_model(data, comm_var, "mest_liberale", paste("Mest Liberale -", comm_var)),
    run_model(data, comm_var, "mest_konservative", paste("Mest Konservative -", comm_var))
  )
}

# Saml resultater i en dataframe --------------------------------------------------------------
results_df <- as.data.frame(results)

# Tilføj labels for kommunikationstyper
results_df$communication_label <- recode(results_df$communication_var,
                                         "communication_window_7" = "Samlet kommunikation",
                                         "communication_window_7_floor" = "Talerstolen",
                                         "communication_window_7_newsletter" = "Nyhedsbreve",
                                         "communication_window_7_statements" = "Officielle udtalelser"
)

# Sortér ideologi-grupper for bedre plot -------------------------------------------------------
results_df <- results_df %>%
  mutate(
    ideology_group = recode(
      ideology_group,
      "mest_liberale" = "Mest liberale",
      "liberale" = "Liberale",
      "konservative" = "Konservative",
      "mest_konservative" = "Mest konservative",
      "alle_politikere" = "Alle politikere"
    )
  )

results_df$ideology_group <- factor(
  results_df$ideology_group,
  levels = c("Mest liberale", "Liberale", "Konservative", "Mest konservative", "Alle politikere")
)

# Plot resultater ---------------------------------------------------------------------------
# Adjusted plot to match the provided example
final_plot_palestine <- ggplot(results_df, aes(
  x = ideology_group, 
  y = estimate, 
  ymin = conf_low, 
  ymax = conf_high, 
  shape = communication_label
)) +
  geom_errorbar(
    position = position_dodge(width = 0.35), 
    width = 0.15, 
    color = "black"
  ) +
  geom_pointrange(
    position = position_dodge(width = 0.35), 
    size = 0.5, 
    color = "black", 
    fatten = 4
  ) +
  geom_hline(
    yintercept = 0, 
    linetype = "dashed", 
    color = "black"
  ) +
  annotate(
    "rect", 
    xmin = 4.5, xmax = 5.5, 
    ymin = -Inf, ymax = Inf, 
    fill = "lightgrey", 
    alpha = 0.4
  ) +
  labs(
    title = "Pro-palæstinenske demonstrationer",
    x = "Ideologisk gruppe",
    y = "Koefficient",
    shape = "Kommunikationstype"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)  # Center the title
  )

# Display the plot
print(final_plot_palestine)

# Gemmer figuren
ggsave(
  filename = "/Users/carlgammeltoft/Lokale dokumenter/R_speciale/4_modelling/3_output/plots_kommunikation/pro_palestine_kommunikation.pdf",
  plot = final_plot_palestine,
  width = 12, height = 6
)


# 1) Transformer til bredt format
wide_palestine <- results_df %>%
  dplyr::select(ideology_group, communication_label, estimate, conf_low, conf_high) %>%
  pivot_wider(
    id_cols    = ideology_group,
    names_from = communication_label,
    values_from = c(estimate, conf_low, conf_high),
    names_sep  = "_"
  )

# 2) Print LaTeX‐kode i landscape
wide_palestine %>%
  kable(
    format    = "latex",
    booktabs  = TRUE,
    caption   = "Effektestimater (±95\\% CI) for demonstrationintensitet på forskellige kommunikationskanaler efter ideologisk gruppe (pro-palæstinensiske modeller)",
    label     = "tab:B2",
    digits    = 3,
    escape    = FALSE,
    align     = c("l", rep("r", 12))
  ) %>%
  add_header_above(c(
    " " = 1,
    "Samlet kommunikation"  = 3,
    "Talerstolen"           = 3,
    "Nyhedsbreve"           = 3,
    "Officielle udtalelser" = 3
  )) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down", "landscape")
  ) %>%
  cat()







