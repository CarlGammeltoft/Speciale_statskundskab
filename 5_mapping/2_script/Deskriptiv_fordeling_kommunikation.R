### Deskriptiv_fordeling_statements

# Indlæser nødvendige pakker og data -----------------------------------------------------------
setwd("/Users/carlgammeltoft/Lokale dokumenter/R_speciale/4_modelling/")
data <- read.csv("1_input/data_finalized.csv")

library(tidyverse)

# Databehandling ----------------------------------------------------------
# Group the data and count the occurrences of each antal_statements
data_sum <- data %>%
  group_by(bioguide_id) %>%
  summarise(antal_statements = sum(count_statement)) %>%
  ungroup() %>%
  group_by(antal_statements) %>%
  summarise(count_bioguide_ids = n())  # Count how many bioguide_ids per antal_statements

# Create the histogram ----------------------------------------------------
plot <- ggplot(data_sum, aes(x = antal_statements, y = count_bioguide_ids)) +
  geom_bar(stat = "identity", color = "black", fill = "grey", alpha = 0.9) +
  labs(x = "Samlet antal udtalelser", y = "Antal politikere") +
  scale_y_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70)) +
  theme_minimal()
  # theme(panel.grid = element_blank()) # Removes all grid lines

print(plot)

# Gemmer figuren
ggsave(
  filename = "/Users/carlgammeltoft/Lokale dokumenter/R_speciale/5_mapping/3_output/fordeling_kommunikation.pdf",
  plot = plot,
  width = 12, height = 6
)


mean(data$count_statement)
var(data$count_statement)

library(AER)

poisson_model <- glm(count_statement ~ )
