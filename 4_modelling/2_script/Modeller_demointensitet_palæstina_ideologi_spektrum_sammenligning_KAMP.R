### Modeller_demointensitet_palæstina_ideologi_spektrum_sammenligning_KAMP

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

### MODELLER FOR PRO-PALÆSTINA DEMONSTRATIONER

# Resultatliste til at samle resultater
results <- list()

# Funktion til at køre modeller
run_model <- function(data, ideology_filter = NULL, comp_filter = NULL, model_name) {
  
  # Filtrering af data
  test_subset_data <- data
  
  if (!is.null(ideology_filter)) {
    if (ideology_filter == "liberale") {
      test_subset_data <- test_subset_data %>% filter(ideology_percentile <= 0.5)
    } else if (ideology_filter == "konservative") {
      test_subset_data <- test_subset_data %>% filter(ideology_percentile > 0.5)
    }
  }
  
  if (!is.null(comp_filter)) {
    test_subset_data <- test_subset_data %>% filter(comp == comp_filter)
  }
  
  # Kør modellen
  model <- femlm(
    communication_window_7 ~ demonstration_intensity_palestine + demonstration_intensity_total_7_after | bioguide_id + date, 
    family = "negbin",
    data = test_subset_data
  )
  
  # Udtræk koefficient for demonstration_intensity_palestine
  coef <- summary(model)$coeftable["demonstration_intensity_palestine", ]
  
  # Gem resultater
  return(data.frame(
    model = model_name,
    estimate = coef["Estimate"],
    conf_low = coef["Estimate"] - 1.96 * coef["Std. Error"],
    conf_high = coef["Estimate"] + 1.96 * coef["Std. Error"]
  ))
}

# Kør modellerne og tilføj resultater til listen
results <- rbind(
  results,
  run_model(data, ideology_filter = "liberale", comp_filter = "1", model_name = "Komp Ræs - 50% Liberale (Palæstina)"),
  run_model(data, ideology_filter = NULL, comp_filter = "1", model_name = "Komp Ræs - Alle Politikere (Palæstina)"),
  run_model(data, ideology_filter = "konservative", comp_filter = "1", model_name = "Komp Ræs - 50% Konservative (Palæstina)"),
  run_model(data, ideology_filter = "liberale", comp_filter = NULL, model_name = "Alle Ræs - 50% Liberale (Palæstina)"),
  run_model(data, ideology_filter = NULL, comp_filter = NULL, model_name = "Alle Ræs - Alle Politikere (Palæstina)"),
  run_model(data, ideology_filter = "konservative", comp_filter = NULL, model_name = "Alle Ræs - 50% Konservative (Palæstina)")
)

# Udskriv resultaterne
print(results)

### Plotter resultaterne
results_df <- as.data.frame(results)

# Definer rækkefølgen for modellerne
results_df$model <- factor(results_df$model, levels = c(
  "Alle Ræs - 50% Liberale (Palæstina)",
  "Alle Ræs - 50% Konservative (Palæstina)",
  "Alle Ræs - Alle Politikere (Palæstina)",
  "Komp Ræs - 50% Liberale (Palæstina)",
  "Komp Ræs - 50% Konservative (Palæstina)",
  "Komp Ræs - Alle Politikere (Palæstina)"
))

# Tilføj en ny kolonne for kategori (Komp Ræs / Alle Ræs)
results_df <- results_df %>%
  mutate(category = ifelse(grepl("Komp", model), "Konkurrenceprægede valgdistrikter", "Alle valgdistrikter"))

# Definer rækkefølgen for de specifikke grupper
results_df$group <- factor(case_when(
  grepl("Liberale", results_df$model) ~ "Liberale",
  grepl("Konservative", results_df$model) ~ "Konservative",
  grepl("Alle Politikere", results_df$model) ~ "Alle Politikere"
), levels = c("Liberale", "Konservative", "Alle Politikere"))

# Plot estimater med konfidensintervaller
ggplot(results_df, aes(x = group, y = estimate, shape = category)) +
  geom_point(position = position_dodge(width = 0.5), size = 3, color = "black") + # Punkter i sort
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), 
                position = position_dodge(width = 0.5), width = 0.2, color = "grey40") + # Konfidensintervaller i grå
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") + # Stiplet linje ved y = 0
  labs(
    title = "Effekt af Pro-Palæstina Demonstrationer på Politikerkommunikation",
    x = "Gruppe",
    y = "Estimate (med 95% Konfidensinterval)",
    shape = "Modeltype"
  ) +
  scale_shape_manual(values = c("Komp Ræs" = 16, "Alle Ræs" = 17)) + # Former til punkter
  theme_minimal(base_size = 12) + # Minimalistisk tema
  theme(
    axis.text.x = element_text(size = 10, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )

# Plot estimater med konfidensintervaller
palestine_plot <- ggplot(results_df, aes(x = group, y = estimate, shape = category)) +
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
    title = "Pro-palæstina demonstrationer",
    x = "Ideologisk gruppe",
    y = "Koefficient",
    shape = "Modeltype"
  ) +
  # Former til punkter
  scale_shape_manual(values = c("Alle valgdistrikter" = 16, "Konkurrenceprægede valgdistrikter" = 15)) +
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

palestine_plot


# Sørg for at have disse pakker loaded
library(dplyr)
library(knitr)
library(kableExtra)

# Antag at du allerede har kørt din run_model_palestine‐pipeline og har results_palestine

# 1) Gør data klar til LaTeX‐tabel
table_palestine <- results_df %>%
  dplyr::select(
    Model            = model,
    Gruppe           = group,
    Modeltype        = category,
    Koefficient      = estimate,
    `95\\% CI nederst` = conf_low,
    `95\\% CI øverst`  = conf_high
  )

# 2) Print LaTeX‐kode
table_palestine %>%
  kable(
    format    = "latex",
    booktabs  = TRUE,
    caption   = "Sammenligning af negative binomial‐modellers estimater for pro-palæstinensiske demonstrationer på politisk kommunikation",
    label     = "tab:comp_palestina_ideologi",
    digits    = 3,
    escape    = FALSE,
    align     = c("l","l","l","r","r","r")
  ) %>%
  add_header_above(c(" " = 3, "95\\% konfidensinterval" = 2)) %>%
  kable_styling(latex_options = c("hold_position","scale_down")) %>%
  cat()

