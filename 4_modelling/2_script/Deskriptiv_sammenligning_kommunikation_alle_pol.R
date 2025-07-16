### Deskriptiv sammenligning af alle politikeres kommunikation 14 dage efter dag x med/uden demo

# Indlæser data -----------------------------------------------------------
setwd("/Users/carlgammeltoft/Lokale dokumenter/R_speciale/4_modelling/")

data <- read.csv("1_input/data_finalized.csv")

library(tidyverse)


# På tværs af politikere - kommunikation med/uden demo --------------------

# Her skal jeg undersøge hypotesen om, at politikere, der har været udsat for en
# demonstration har en højere kommunikation end hvis de ikke var blevet udsat for
# en demonstration.

# Det kan både gøres within og between (altså for hver enkelt politiker eller
# på tværs af poltikere)

## Sammenligning af gennemsnitlig kommunikation 14 dage efter hhv. demo og ikke-demo
# Måler for hver enkelt politiker, hvis der har været en demonstration på en dag
# hvor meget de så taler om konflikten 14 dage efter og tager gennemsnittet af dette. 
# Og sammenligner med en dag, hvor der ikke har været en demonstration og ser på
# hvor meget de i gennemsnit taler om det 14 dage efter.

# Beregn gennemsnittet af kommunikation 14 dage efter for begge grupper
mean_communication_with_demo <- mean(data$communication_window_14[data$count_protest > 0], na.rm = TRUE)
mean_communication_without_demo <- mean(data$communication_window_14[data$count_protest == 0], na.rm = TRUE)

# Print resultaterne
cat("Gennemsnitlig kommunikation 14 dage efter en demonstration:", mean_communication_with_demo, "\n")
cat("Gennemsnitlig kommunikation 14 dage uden demonstration:", mean_communication_without_demo, "\n")

# Sammenlign forskellen mellem grupperne
difference <- mean_communication_with_demo - mean_communication_without_demo
cat("Forskel i gennemsnitlig kommunikation:", difference, "\n")

# Beregn gennemsnit for hver kombination af gruppe og parti
communication_data <- data %>%
  group_by(party) %>%
  summarise(
    mean_comm_with_demo = mean(communication_window_14[count_protest > 0], na.rm = TRUE),
    mean_comm_without_demo = mean(communication_window_14[count_protest == 0], na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = c(mean_comm_with_demo, mean_comm_without_demo),
    names_to = "Group",
    values_to = "MeanCommunication"
  ) %>%
  mutate(Group = ifelse(Group == "mean_comm_with_demo", "Med demonstration", "Uden demonstration"))

# Fjerner NA's
communication_data <- communication_data %>% 
  filter(!(is.na(party)))

# Simpelt søjlediagram med parti grupperet
ggplot(communication_data, aes(x = party, y = MeanCommunication, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(
    title = "Gennemsnitlig kommunikation 14 dage efter demonstration vs. uden demonstration",
    x = "Parti",
    y = "Gennemsnitlig kommunikation",
    fill = "Gruppe"
  ) +
  theme_minimal()


## Sammenligning af gennemsnitlig kommunikation 14 dage efter hhv. demo og ikke-demo
## fordelt på politikere
comm_pol_14_days <- data %>%
  group_by(bioguide_id) %>%
  summarise(
    mean_comm_with_demo = mean(communication_window_14[count_protest > 0], na.rm = TRUE),
    mean_comm_without_demo = mean(communication_window_14[count_protest == 0], na.rm = TRUE),
    party = max(party)
  )

# Scatter plot
# Find maks-værdien for både x og y for at sikre samme grænser
max_value <- max(c(comm_pol_14_days$mean_comm_with_demo, comm_pol_14_days$mean_comm_without_demo), na.rm = TRUE)

# Scatter plot med farve efter parti
ggplot(data = comm_pol_14_days, aes(x = mean_comm_without_demo, y = mean_comm_with_demo, color = party)) +
  geom_point(size = 2.5, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Sammenligning af gennemsnitlig kommunikation 14 dage efter demo vs. uden demo",
    x = "Gennemsnitlig kommunikation 14 dage uden demonstration",
    y = "Gennemsnitlig kommunikation 14 dage efter demonstration",
    color = "Parti"
  ) +
  scale_x_continuous(limits = c(0, max_value)) +
  scale_y_continuous(limits = c(0, max_value)) +
  scale_color_discrete(na.translate = FALSE) +
  theme_minimal()





