### Deskriptiv sammenligning - kongruente politikere - binær demo

library(tidyverse)
library(cowplot)
library(gridExtra)

# Indlæser data -----------------------------------------------------------
setwd("/Users/carlgammeltoft/Lokale dokumenter/R_speciale/4_modelling/")
data <- read.csv("1_input/data_finalized.csv")

data <- data %>% 
  filter(date >= "2023-09-01")

data <- data %>% 
  mutate(communication_window_2 = communication_window_1)

# BARPLOT - SAMMENLIGNING -----------------------------------------------------------------
# Add quantile-based groupings and "Alle politikere"
combined_data <- data %>%
  mutate(
    group = case_when(
      ideology_percentile_rep <= quantile(ideology_percentile_rep, 0.15, na.rm = TRUE) ~ "Mest liberale",
      ideology_percentile_rep >= quantile(ideology_percentile_rep, 0.85, na.rm = TRUE) ~ "Mest konservative",
      main_ideology == 0 ~ "Liberale",
      main_ideology == 1 ~ "Konservative",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(communication_window_2)) %>% # Remove NA in communication_window_2 but keep all groups
  mutate(
    Group = case_when(
      count_palestine_protests > 0 ~ "Med demonstration",
      count_palestine_protests == 0 ~ "Uden demonstration",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(group, Group) %>%
  summarise(
    MeanCommunication = mean(communication_window_2, na.rm = TRUE),
    .groups = "drop"
  )

# Add "Alle politikere" as a group
all_politicians <- data %>%
  mutate(Group = case_when(
    count_palestine_protests > 0 ~ "Med demonstration",
    count_palestine_protests == 0 ~ "Uden demonstration",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(communication_window_2)) %>%
  group_by(Group) %>%
  summarise(
    group = "Alle politikere", # Assign to "Alle politikere"
    MeanCommunication = mean(communication_window_2, na.rm = TRUE),
    .groups = "drop"
  )

# Combine with existing data
combined_data <- bind_rows(combined_data, all_politicians)

# Fjern NA-kategorien og tilføj ekstra mellemrum før "Alle politikere"
combined_data <- combined_data %>%
  filter(!is.na(group)) %>% # Fjern NA
  mutate(group = factor(group, levels = c("Mest liberale", "Liberale", "Konservative", "Mest konservative", "", "Alle politikere")))

# Barplot med baggrundsfarve til "Alle politikere"
gnm_2_pal_plot <- ggplot(combined_data, aes(x = group, y = MeanCommunication, fill = Group)) +
  # Baggrundsfarve til "Alle politikere"
  annotate("rect", xmin = 4.5, xmax = 5.5, ymin = -Inf, ymax = Inf, fill = "lightgrey", alpha = 0.8) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_hline(yintercept = 0) +
  labs(
    title = "Gennemsnitlig kommunikation efter demonstrationsstatus (pro-palæstinensisk)",
    x = "Ideologisk gruppe",
    y = "Gennemsnitlig kommunikation (2 dage)",
    fill = "Demonstrationsstatus"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Med demonstration" = "black", "Uden demonstration" = "grey")) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank()
  )

print(gnm_2_pal_plot)


# Tilføj kvantilbaserede grupperinger og inkluder "Alle politikere"
combined_israel_data <- data %>%
  mutate(
    group = case_when(
      ideology_percentile_rep <= quantile(ideology_percentile_rep, 0.15, na.rm = TRUE) ~ "Mest liberale",
      ideology_percentile_rep >= quantile(ideology_percentile_rep, 0.85, na.rm = TRUE) ~ "Mest konservative",
      main_ideology == 0 ~ "Liberale",
      main_ideology == 1 ~ "Konservative",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(communication_window_2)) %>% # Fjern NA i kommunikation
  mutate(
    Group = case_when(
      count_israel_protests > 0 ~ "Med demonstration",
      count_israel_protests == 0 ~ "Uden demonstration",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(group, Group) %>%
  summarise(
    MeanCommunication = mean(communication_window_2, na.rm = TRUE),
    .groups = "drop"
  )

# Tilføj "Alle politikere" som ekstra gruppe
all_politicians_israel <- data %>%
  mutate(Group = case_when(
    count_israel_protests > 0 ~ "Med demonstration",
    count_israel_protests == 0 ~ "Uden demonstration",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(communication_window_2)) %>%
  group_by(Group) %>%
  summarise(
    group = "Alle politikere", # Tildel "Alle politikere"
    MeanCommunication = mean(communication_window_2, na.rm = TRUE),
    .groups = "drop"
  )

# Kombiner data med "Alle politikere"
combined_israel_data <- bind_rows(combined_israel_data, all_politicians_israel) %>%
  filter(!is.na(group)) %>% # Fjern NA
  mutate(group = factor(group, levels = c("Mest liberale", "Liberale", "Konservative", "Mest konservative", "", "Alle politikere")))

# Barplot for pro-israelske demonstrationer
gnm_2_isr_plot <- ggplot(combined_israel_data, aes(x = group, y = MeanCommunication, fill = Group)) +
  # Baggrundsfarve til "Alle politikere"
  annotate("rect", xmin = 4.5, xmax = 5.5, ymin = -Inf, ymax = Inf, fill = "lightgrey", alpha = 0.8) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_hline(yintercept = 0) + 
  labs(
    title = "Gennemsnitlig kommunikation efter demonstrationsstatus (pro-israelsk)",
    x = "Ideologisk gruppe",
    y = "Gennemsnitlig kommunikation (2 dage)",
    fill = "Demonstrationsstatus"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Med demonstration" = "black", "Uden demonstration" = "grey")) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank()
  )

# Vis plot
print(gnm_2_isr_plot)

# Funktion til at ekstrahere én legend
get_single_legend <- function(a.gplot) {
  tmp <- ggplotGrob(a.gplot)
  legend <- tmp$grobs[[which(sapply(tmp$grobs, function(x) x$name) == "guide-box")]]
  return(legend)
}

# Fjern legender og tilføj underoverskrifter
gnm_2_pal_plot <- gnm_2_pal_plot +
  labs(subtitle = "Pro-palæstinensiske demonstrationer", title = NULL) +
  theme(legend.position = "none")

gnm_2_isr_plot <- gnm_2_isr_plot +
  labs(subtitle = "Pro-israelske demonstrationer", title = NULL) +
  theme(legend.position = "none")

# Ekstraher fælles legend
legend <- get_single_legend(
  gnm_2_pal_plot + theme(legend.position = "bottom")
)

# Kombiner de to plots vertikalt
combined_plots <- plot_grid(
  gnm_2_pal_plot, 
  gnm_2_isr_plot, 
  ncol = 1, 
  align = "v",
  rel_heights = c(1, 1)
)

# Tilføj overskrift og fælles legend
final_plot <- plot_grid(
  ggdraw() + draw_label("Gennemsnitlig kommunikation efter demonstrationsstatus",
                        fontface = 'bold', size = 14, hjust = 0.5),
  combined_plots,
  legend,
  ncol = 1,
  rel_heights = c(0.1, 1, 0.1) # Justér højdeforhold
)

# Vis det samlede plot
print(final_plot)

# Gemmer barplots som en enkelt fil
ggsave(
  filename = "/Users/carlgammeltoft/Lokale dokumenter/R_speciale/4_modelling/3_output/plots_deskriptiv_sammenligning_kongruente_politikere_2dage/barplots_combined_2_days.png",
  plot = final_plot,
  width = 10, height = 8
)


# POINT-PLOT --------------------------------------------------------------

# ### Plot med hele det ideologiske spektrum på x-aksen -------------------
# Nu kan jeg plotte dem på en akse med ideology på x-aksen for at se forskellen i kommunikation
# over hele spektrummet på y-aksen (altså forskellen i deres kommunikation alt efter 
# om der har været en demo eller ej)

# Add ideology percentile groups (25 bins)
combined_data <- data %>%
  mutate(ideology_percentile_group = ntile(ideology_percentile_rep, 25))  # Divide into 25 groups

# Calculate differences for each ideology percentile
communication_diff_pale <- combined_data %>%
  mutate(
    Group = case_when(
      count_palestine_protests > 0 ~ "Med demonstration",
      count_palestine_protests == 0 ~ "Uden demonstration",
      TRUE ~ NA_character_
    )) %>% 
  group_by(ideology_percentile_group, Group) %>%
  summarise(
    MeanCommunication = mean(communication_window_2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = Group, values_from = MeanCommunication) %>%
  mutate(
    Difference = `Med demonstration` - `Uden demonstration`,
    Percentile = (ideology_percentile_group - 1) / 24  # Convert groups to values from 0 to 1
  ) %>%
  filter(!is.na(Difference))  # Remove rows with NA differences

# Entire ideological spectrum
communication_pale_data <- data %>%
  group_by(ideology_percentile_rep, bioguide_id) %>%
  summarise(
    mean_communication_with_pale_demo = mean(communication_window_2[count_palestine_protests > 0], na.rm = TRUE),
    mean_communication_without_pale_demo = mean(communication_window_2[count_palestine_protests == 0], na.rm = TRUE),
    .groups = "drop" # Remove grouping to avoid unintended behavior
  ) %>%
  pivot_longer(
    cols = c(mean_communication_with_pale_demo, mean_communication_without_pale_demo),
    names_to = "Group",
    values_to = "MeanCommunication"
  ) %>%
  mutate(
    Group = ifelse(Group == "mean_communication_with_pale_demo", "Med demonstration", "Uden demonstration")
  )

# Replace NA's with 0
communication_pale_data <- communication_pale_data %>% 
  mutate(MeanCommunication = ifelse(is.nan(MeanCommunication), 0, MeanCommunication))

communication_diff <- communication_pale_data %>%
  group_by(ideology_percentile_rep, bioguide_id) %>%
  summarise(
    Difference = MeanCommunication[Group == "Med demonstration"] - MeanCommunication[Group == "Uden demonstration"],
    .groups = "drop"
  )

# Combined plot with adjusted bar plots
gnm_pal_2_ideo_plot <- ggplot() +
  # Add bar plot for ideology percentile groups (adjusted visibility)
  geom_col(data = communication_diff_pale, aes(x = Percentile, y = Difference), 
           fill = "#EE2A35", alpha = 0.6, width = 0.04) +
  # Add scatter plot for individual politicians
  geom_point(data = communication_diff, aes(x = ideology_percentile_rep, y = Difference), 
             alpha = 0.4, size = 2, color = "black") +
  # Add horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  # Labels and titles
  labs(
    title = "Forskel i kommunikation (Med demonstration vs. Uden demonstration)",
    subtitle = "Pro-palæstinensiske demonstrationer",
    x = "Ideologisk position",
    y = "Forskel i kommunikation (2 dage)"
  ) +
  # Use x-axis from scatter plot with percentiles
  scale_x_continuous(
    breaks = seq(0, 1, by = 0.2),
    labels = function(x) ifelse(x == 0, "0 = Max liberal", ifelse(x == 1, "1 = Max konservativ", as.character(x)))
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # Easy-to-read x-axis labels
    plot.title = element_text(hjust = 0.5),              # Center title
    plot.subtitle = element_text(hjust = 0.5)           # Center subtitle
  )

print(gnm_pal_2_ideo_plot)


# ### Plot med hele det ideologiske spektrum på x-aksen -------------------
# Nu kan jeg plotte dem på en akse med ideology på x-aksen for at se forskellen i kommunikation
# over hele spektrummet på y-aksen (altså forskellen i deres kommunikation alt efter 
# om der har været en demo eller ej)

# Add ideology percentile groups (25 bins)
combined_data <- data %>%
  mutate(ideology_percentile_group = ntile(ideology_percentile_rep, 25))  # Divide into 25 groups

# Calculate differences for each ideology percentile
communication_diff_isr <- combined_data %>%
  mutate(
    Group = case_when(
      count_israel_protests > 0 ~ "Med demonstration",
      count_israel_protests == 0 ~ "Uden demonstration",
      TRUE ~ NA_character_
    )) %>% 
  group_by(ideology_percentile_group, Group) %>%
  summarise(
    MeanCommunication = mean(communication_window_2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = Group, values_from = MeanCommunication) %>%
  mutate(
    Difference = `Med demonstration` - `Uden demonstration`,
    Percentile = (ideology_percentile_group - 1) / 24  # Convert groups to values from 0 to 1
  ) %>%
  filter(!is.na(Difference))  # Remove rows with NA differences

# Entire ideological spectrum
communication_isr_data <- data %>%
  group_by(ideology_percentile_rep, bioguide_id) %>%
  summarise(
    mean_communication_with_isr_demo = mean(communication_window_2[count_israel_protests > 0], na.rm = TRUE),
    mean_communication_without_isr_demo = mean(communication_window_2[count_israel_protests == 0], na.rm = TRUE),
    .groups = "drop" # Remove grouping to avoid unintended behavior
  ) %>%
  pivot_longer(
    cols = c(mean_communication_with_isr_demo, mean_communication_without_isr_demo),
    names_to = "Group",
    values_to = "MeanCommunication"
  ) %>%
  mutate(
    Group = ifelse(Group == "mean_communication_with_isr_demo", "Med demonstration", "Uden demonstration")
  )

# Replace NA's with 0
communication_isr_data <- communication_isr_data %>% 
  mutate(MeanCommunication = ifelse(is.nan(MeanCommunication), 0, MeanCommunication))

communication_diff <- communication_isr_data %>%
  group_by(ideology_percentile_rep, bioguide_id) %>%
  summarise(
    Difference = MeanCommunication[Group == "Med demonstration"] - MeanCommunication[Group == "Uden demonstration"],
    .groups = "drop"
  )

# Combined plot with adjusted bar plots
gnm_isr_2_ideo_plot <- ggplot() +
  # Add bar plot for ideology percentile groups (adjusted visibility)
  geom_col(data = communication_diff_isr, aes(x = Percentile, y = Difference), 
           fill = "#0038b8", alpha = 0.6, width = 0.04) +
  # Add scatter plot for individual politicians
  geom_point(data = communication_diff, aes(x = ideology_percentile_rep, y = Difference), 
             alpha = 0.4, size = 2, color = "black") +
  # Add horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  # Labels and titles
  labs(
    title = "Forskel i kommunikation (Med demonstration vs. Uden demonstration)",
    subtitle = "Pro-israelske demonstrationer",
    x = "Ideologisk position",
    y = "Forskel i kommunikation (2 dage)"
  ) +
  # Use x-axis from scatter plot with percentiles
  scale_x_continuous(
    breaks = seq(0, 1, by = 0.2),
    labels = function(x) ifelse(x == 0, "0 = Max liberal", ifelse(x == 1, "1 = Max konservativ", as.character(x)))
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # Easy-to-read x-axis labels
    plot.title = element_text(hjust = 0.5),              # Center title
    plot.subtitle = element_text(hjust = 0.5)           # Center subtitle
  )

# Fjern individuelle overskrifter og behold underoverskrifter
gnm_pal_2_ideo_plot <- gnm_pal_2_ideo_plot +
  labs(title = NULL) +  # Fjern overskrift
  theme(plot.title = element_blank())  # Sørg for ingen titel vises

gnm_isr_2_ideo_plot <- gnm_isr_2_ideo_plot +
  labs(title = NULL) +  # Fjern overskrift
  theme(plot.title = element_blank())  # Sørg for ingen titel vises

# Kombiner ideologi-plots
combined_ideoplots <- plot_grid(
  gnm_pal_2_ideo_plot,  # Første plot (Pro-palæstinensisk)
  gnm_isr_2_ideo_plot,  # Andet plot (Pro-israelsk)
  ncol = 1,             # Vertikal opsætning
  align = "v",          # Vertikal justering
  rel_heights = c(1, 1) # Lige højde til begge plots
)

# Tilføj fælles overskrift
final_ideoplots <- plot_grid(
  ggdraw() + draw_label("Forskel i kommunikation (med demonstration - uden demonstration)", 
                        fontface = 'bold', size = 14, hjust = 0.5),
  combined_ideoplots,
  ncol = 1,
  rel_heights = c(0.1, 1) # Justér højde til overskrift og plots
)

# Vis det samlede plot
print(final_ideoplots)

# Gemmer ideologi-plots som en enkelt fil
ggsave(
  filename = "/Users/carlgammeltoft/Lokale dokumenter/R_speciale/4_modelling/3_output/plots_deskriptiv_sammenligning_kongruente_politikere_2dage/ideoplots_combined_2_days.png",
  plot = final_ideoplots,
  width = 12, height = 8
)

