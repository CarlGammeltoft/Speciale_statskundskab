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

# Opdel data ------------------------------------------------------------
data_conservative_15 <- data %>% 
  filter(conservative_15 == 1)

data_liberal_15 <- data %>% 
  filter(liberal_15 == 1)

data_conservative <- data %>% 
  filter(main_ideology == 1)

data_liberal <- data %>% 
  filter(main_ideology == 0)

# ## DV: Demonstrationsintensitet ----------------------------------------------------------

# 15% Liberale ----------------------------------------------------------
# M1: NB - Demonstrationsintensitet
M1_liberal_15_intensity <- glm.nb(
  communication_window_7 ~ demonstration_intensity_israel, 
  data = data_liberal_15
)

summary(M1_liberal_15_intensity)

# M2: NB - Demonstrationsintensitet + Kontrollvariabel
M2_liberal_15_intensity <- glm.nb(
  communication_window_7 ~ demonstration_intensity_israel + demonstration_intensity_total_7_after, 
  data = data_liberal_15
)

summary(M2_liberal_15_intensity)

# M3: ZINB - Demonstrationsintensitet + Kontrollvariabel + FE
M3_liberal_15_intensity <- femlm(
  communication_window_7 ~ demonstration_intensity_israel + demonstration_intensity_total_7_after | bioguide_id + date, 
  data = data_liberal_15, 
  dist = "negbin"
)

summary(M3_liberal_15_intensity)

# 15% Konservative ----------------------------------------------------------
# M1: NB - Demonstrationsintensitet
M1_conservative_15_intensity <- glm.nb(
  communication_window_7 ~ demonstration_intensity_israel, 
  data = data_conservative_15
)

summary(M1_conservative_15_intensity)

# M2: NB - Demonstrationsintensitet + Kontrollvariabel
M2_conservative_15_intensity <- glm.nb(
  communication_window_7 ~ demonstration_intensity_israel + demonstration_intensity_total_7_after, 
  data = data_conservative_15
)

summary(M2_conservative_15_intensity)

# M3: ZINB - Demonstrationsintensitet + Kontrollvariabel + FE
M3_conservative_15_intensity <- femlm(
  communication_window_7 ~ demonstration_intensity_israel + demonstration_intensity_total_7_after | bioguide_id + date, 
  data = data_conservative_15, 
  dist = "negbin"
)

summary(M3_conservative_15_intensity)

# Alle Liberale ----------------------------------------------------------
# M1: NB - Demonstrationsintensitet
M1_liberal_intensity <- glm.nb(
  communication_window_7 ~ demonstration_intensity_israel, 
  data = data_liberal
)

summary(M1_liberal_intensity)

# M2: NB - Demonstrationsintensitet + Kontrollvariabel
M2_liberal_intensity <- glm.nb(
  communication_window_7 ~ demonstration_intensity_israel + demonstration_intensity_total_7_after, 
  data = data_liberal
)

summary(M2_liberal_intensity)

# M3: ZINB - Demonstrationsintensitet + Kontrollvariabel + FE
M3_liberal_intensity <- femlm(
  communication_window_7 ~ demonstration_intensity_israel + demonstration_intensity_total_7_after | bioguide_id + date, 
  data = data_liberal, 
  dist = "negbin"
)

summary(M3_liberal_intensity)

# Alle Konservative ----------------------------------------------------------
# M1: NB - Demonstrationsintensitet
M1_conservative_intensity <- glm.nb(
  communication_window_7 ~ demonstration_intensity_israel, 
  data = data_conservative
)

summary(M1_conservative_intensity)

# M2: NB - Demonstrationsintensitet + Kontrollvariabel
M2_conservative_intensity <- glm.nb(
  communication_window_7 ~ demonstration_intensity_israel + demonstration_intensity_total_7_after, 
  data = data_conservative
)

summary(M2_conservative_intensity)

# M3: ZINB - Demonstrationsintensitet + Kontrollvariabel + FE
M3_conservative_intensity <- femlm(
  communication_window_7 ~ demonstration_intensity_israel + demonstration_intensity_total_7_after | bioguide_id + date, 
  data = data_conservative, 
  dist = "negbin"
)

summary(M3_conservative_intensity)

# Alle politikere ----------------------------------------------------------
# M1: NB - Demonstrationsintensitet
M1_intensity <- glm.nb(
  communication_window_7 ~ demonstration_intensity_israel, 
  data = data
)

summary(M1_intensity)

# M2: NB - Demonstrationsintensitet + Kontrollvariabel
M2_intensity <- glm.nb(
  communication_window_7 ~ demonstration_intensity_israel + demonstration_intensity_total_7_after, 
  data = data
)

summary(M2_intensity)

# M3: ZINB - Demonstrationsintensitet + Kontrollvariabel + FE
M3_intensity <- femlm(
  communication_window_7 ~ demonstration_intensity_israel + demonstration_intensity_total_7_after | bioguide_id + date, 
  data = data, 
  dist = "negbin"
)

summary(M3_intensity)

# Samler modellerne -------------------------------------------------------

model_list <- list(
  "M1: Liberal 15% - Intensity" = M1_liberal_15_intensity,
  "M2: Liberal 15% - Intensity + CV" = M2_liberal_15_intensity,
  "M3: Liberal 15% - Intensity + FE" = M3_liberal_15_intensity,
  "M1: Conservative 15% - Intensity" = M1_conservative_15_intensity,
  "M2: Conservative 15% - Intensity + CV" = M2_conservative_15_intensity,
  "M3: Conservative 15% - Intensity + FE" = M3_conservative_15_intensity,
  "M1: Liberal - Intensity" = M1_liberal_intensity,
  "M2: Liberal - Intensity + CV" = M2_liberal_intensity,
  "M3: Liberal - Intensity + FE" = M3_liberal_intensity,
  "M1: Conservative - Intensity" = M1_conservative_intensity,
  "M2: Conservative - Intensity + CV" = M2_conservative_intensity,
  "M3: Conservative - Intensity + FE" = M3_conservative_intensity,
  "M1: Intensity" = M1_intensity,
  "M2: Intensity + CV" = M2_intensity,
  "M3: Intensity + FE" = M3_intensity
  )

# Brug modelsummary til at lave en tabel
modelsummary(
  models = model_list,
  stars = TRUE,
  gof_omit = "AIC|BIC|Log.*|Num.*",  # Fjern unødvendige GOF-mål
  coef_map = c(
    "count_israel_protests" = "Antal demonstrationer",
    "demonstration_intensity_israel" = "Demonstrationsintensitet",
    "protest_window_7_after" = "Demonstrationer efter (kontrol)"
  ),
  output = "latex") 
)


# VISUALISERING -----------------------------------------------------------

# Eksempel på model-listen
model_list <- list(
  "Estimat (15% liberale)" = M1_liberal_15_intensity,
  "Kontrolvariable (15% liberale" = M2_liberal_15_intensity,
  "Fixed Effects (15% liberale)" = M3_liberal_15_intensity,
  "Estimat (15% konservative)" = M1_conservative_15_intensity,
  "Kontrolvariable (15% konservative)" = M2_conservative_15_intensity,
  "Fixed Effects (15% konservative)" = M3_conservative_15_intensity,
  "Estimat (alle liberale)" = M1_liberal_intensity,
  "Kontrolvariable (alle liberale)" = M2_liberal_intensity,
  "Fixed Effects (alle liberale)" = M3_liberal_intensity,
  "Estimat (alle konservative)" = M1_conservative_intensity,
  "Kontrolvariable (alle konservative)" = M2_conservative_intensity,
  "Fixed Effects (alle konservative)" = M3_conservative_intensity,
  "Estimat (alle politikere)" = M1_intensity,
  "Kontrolvariable (alle politikere)" = M2_intensity,
  "Fixed Effects (alle politikere)" = M3_intensity
)

# Udtræk estimater og konfidensintervaller
model_results <- map_dfr(
  model_list, 
  ~tidy(.x, conf.int = TRUE), 
  .id = "model"
)

# Tilføj underkategorier baseret på model-navne
model_results <- model_results %>%
  mutate(
    group = case_when(
      grepl("15% konservative", model) ~ "Mest konservative",
      grepl("15% liberal", model) ~ "Mest liberale",
      grepl("alle konservative", model) ~ "Konservative",
      grepl("alle liberale", model) ~ "Liberale",
      grepl("alle politikere", model) ~ "Alle politikere"
    ),
    model_type = case_when(
      grepl("Estimat", model) ~ "Baseline-model",
      grepl("Kontrolvariable", model) ~ "Model med kontrolvariable",
      grepl("Fixed Effects", model) ~ "Model med fixed effects og kontrolvariable"
    )
  )

# Set the factor levels to control the order in the plot
model_results <- model_results %>%
  mutate(
    group = factor(group, levels = c("Mest liberale", "Liberale", "Konservative", "Mest konservative", "Alle politikere")),
    model_type = factor(model_type, levels = c("Baseline-model", "Model med kontrolvariable", "Model med fixed effects og kontrolvariable"))
  )

# Filtrer kun relevante variabler
filtered_results <- model_results %>%
  filter(term == "demonstration_intensity_israel")

# Samlet plot
final_plot <- ggplot(filtered_results, aes(x = group, y = estimate, ymin = conf.low, ymax = conf.high, shape = model_type)) +
  geom_errorbar(position = position_dodge(width = 0.35), width = 0.15, color = "black") + 
  geom_pointrange(position = position_dodge(width = 0.35), size = 0.5, color = "black", fatten = 4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  annotate("rect", xmin = 4.5, xmax = 5.5, ymin = -Inf, ymax = Inf, fill = "lightgrey", alpha = 0.4) +
  labs(
    title = "Pro-israelske demonstrationer",
    x = "Ideologisk gruppe",
    y = "Koefficient",
    shape = "Modeltype"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)  # Centrerer titlen
  )

print(final_plot)

# Gemmer figuren
ggsave(
  filename = "/Users/carlgammeltoft/Lokale dokumenter/R_speciale/4_modelling/3_output/plots_demointensitet_ideologi_kategorisk/pro_israel.pdf",
  plot = final_plot,
  width = 12, height = 6
)
