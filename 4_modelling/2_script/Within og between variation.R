library(tidyverse)

# Beregn kommunikation med og uden demonstrationer for hver bioguide_id
within_variation_data <- data %>%
  group_by(bioguide_id) %>%
  summarise(
    mean_comm_with_demo = mean(communication_window_14[count_protest >= 1], na.rm = TRUE),
    mean_comm_without_demo = mean(communication_window_14[count_protest == 0], na.rm = TRUE),
    party = first(party),
    ideology_percentile_rep = first(ideology_percentile_rep)
  ) %>%
  mutate(
    Difference = mean_comm_with_demo - mean_comm_without_demo
  )

# Scatter plot af within-variation
ggplot(within_variation_data, aes(x = mean_comm_without_demo, y = mean_comm_with_demo, color = party)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Within-variation: Kommunikation med vs. uden demonstration",
    x = "Gennemsnitlig kommunikation uden demonstration",
    y = "Gennemsnitlig kommunikation med demonstration",
    color = "Parti"
  ) +
  theme_minimal()

# Bar plot af forskellen i kommunikation
ggplot(within_variation_data, aes(x = reorder(bioguide_id, Difference), y = Difference, fill = party)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Within-variation: Forskel i kommunikation med vs. uden demonstration",
    x = "Bioguide ID",
    y = "Forskel i gennemsnitlig kommunikation (med - uden)",
    fill = "Parti"
  ) +
  theme_minimal()


# IDEOLOGI
ggplot(within_variation_data, aes(x = ideology_percentile_rep, y = Difference, fill = party)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Within-variation: Forskel i kommunikation med vs. uden demonstration",
    x = "Ideologi Percentil (Republikansk)",
    y = "Forskel i gennemsnitlig kommunikation (med - uden)",
    fill = "Parti"
  ) +
  theme_minimal()

ggplot(within_variation_data, aes(x = ideology_percentile_rep, y = Difference, color = party)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Within-variation: Forskel i kommunikation med vs. uden demonstration",
    x = "Ideologi Percentil (Republikansk)",
    y = "Forskel i gennemsnitlig kommunikation (med - uden)",
    color = "Parti"
  ) +
  theme_minimal()


library(tidyverse)

# Opsummer data over tid
time_series_data <- data %>%
  group_by(date = as.Date(date)) %>%
  summarise(
    total_statements = sum(count_statement, na.rm = TRUE),
    total_protests = sum(count_protest, na.rm = TRUE)
  )

# Omform data til langt format til barplot
time_series_long <- time_series_data %>%
  pivot_longer(
    cols = c(total_statements, total_protests),
    names_to = "Variable",
    values_to = "Count"
  )

# Barplot
ggplot(time_series_long, aes(x = date, y = Count, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  scale_fill_manual(
    values = c("total_statements" = "blue", "total_protests" = "red"),
    labels = c("Kommunikation (count_statement)", "Demonstrationer (count_protest)")
  ) +
  labs(
    title = "Udvikling af kommunikation og demonstrationer over tid",
    x = "Dato",
    y = "Antal",
    fill = "Variabel"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotér datoetiketter for bedre læsbarhed

time_series_data <- data %>%
  mutate(month = format(as.Date(date), "%Y-%m")) %>%
  group_by(month) %>%
  summarise(
    total_statements = sum(count_statement, na.rm = TRUE),
    total_protests = sum(count_protest, na.rm = TRUE)
  )

time_series_data <- data %>%
  mutate(
    date = as.Date(date),
    week = format(date, "%Y-%U") # Gruppér pr. uge
  ) %>%
  filter(date >= as.Date("2023-06-01")) %>%
  group_by(week) %>%
  summarise(
    total_statements = sum(count_statement, na.rm = TRUE),
    total_protests = sum(count_protest, na.rm = TRUE)
  )

time_series_long <- time_series_data %>%
  pivot_longer(
    cols = c(total_statements, total_protests),
    names_to = "Variable",
    values_to = "Count"
  )

ggplot(time_series_long, aes(x = week, y = Count, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  scale_fill_manual(
    values = c("total_statements" = "blue", "total_protests" = "red"),
    labels = c("Kommunikation (count_statement)", "Demonstrationer (count_protest)")
  ) +
  labs(
    title = "Udvikling af kommunikation og demonstrationer (ugentlig, fra 1. september 2023)",
    x = "Uge",
    y = "Antal",
    fill = "Variabel"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



