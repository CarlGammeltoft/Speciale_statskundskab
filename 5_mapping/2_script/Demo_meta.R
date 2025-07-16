### DEMO META
library(scales) 
library(tidyverse)

setwd("/Users/carlgammeltoft/Lokale dokumenter/R_speciale/5_mapping/1_input/")
data_CCC <- read.csv("CCC_classified_all.csv")


# Graf for Palæstina-demonstrationer over tid -----------------------------

### Samlede antal
pro_pale <- data_CCC %>% 
  group_by(date) %>% 
  summarise(antal = sum(palestine_protest))

# Ensure 'date' is a Date type
pro_pale <- pro_pale %>%
  mutate(date = as.Date(date))  # Convert to Date if not already

# Create the bar plot
pale_demo <- ggplot(pro_pale, aes(x = date, y = antal)) +
  geom_bar(stat = "identity", fill = "grey40", color = "black") +  # Use 'identity' because y already has counts
  geom_hline(yintercept = 0) + 
    labs(
    title = "Pro-palæstinensiske demonstrationer",
    x = "",
    y = "Antal demonstrationer"
  ) +
  theme_minimal(base_size = 12) +  # Clean minimalist style
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # Rotate x-axis for readability
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid = element_blank()
  )

print(pale_demo)

### Deltagere
# filtrerer priden ud... (i NY og Saint Louis)
pro_pale_part <- data_CCC %>% 
  filter(size_mean < 350000)

pro_pale_part <- pro_pale_part %>% 
  group_by(date) %>% 
  filter(palestine_protest == 1) %>% 
  summarise(antal_deltagere = sum(size_mean))

# Ensure 'date' is a Date type
pro_pale_part <- pro_pale_part %>%
  mutate(date = as.Date(date))  # Convert to Date if not already

# Create the bar plot
pale_deltagere <- ggplot(pro_pale_part, aes(x = date, y = antal_deltagere)) +
  geom_bar(stat = "identity", fill = "grey40", color = "black") +  # Use 'identity' because y already has counts
  scale_y_continuous(labels = comma) +
  geom_hline(yintercept = 0) +
   labs(
    title = "Pro-palæstinensiske demonstranter",
    x = "",
    y = "Antal demonstranter"
  ) +
  theme_minimal(base_size = 12) +  # Clean minimalist style
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # Rotate x-axis for readability
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid = element_blank()
  )

print(pale_deltagere)

# Graf for Israel-demonstrationer over tid -----------------------------

### Samlede antal
pro_isr <- data_CCC %>% 
  group_by(date) %>% 
  summarise(antal = sum(israel_protest))

# Ensure 'date' is a Date type
pro_isr <- pro_isr %>%
  mutate(date = as.Date(date))  # Convert to Date if not already

# Create the bar plot
isr_demo <- ggplot(pro_isr, aes(x = date, y = antal)) +
  geom_bar(stat = "identity", fill = "grey40", color = "black") +  # Use 'identity' because y already has counts
  geom_hline(yintercept = 0) + 
  labs(
    title = "Pro-israelske demonstrationer",
    x = "",
    y = "Antal demonstrationer"
  ) +
  theme_minimal(base_size = 12) +  # Clean minimalist style
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # Rotate x-axis for readability
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid = element_blank()
  )

print(isr_demo)

### Deltagere
# Manuelt defineret fælles datointerval og breaks
manual_date_range <- as.Date(c("2023-09-01", "2024-09-05"))
manual_breaks <- as.Date(c("2023-10-01", "2024-01-01", "2024-04-01", "2024-07-01"))  # Definerede breaks

# Deltagere for pro-israelske demonstrationer
pro_isr_part <- data_CCC %>% 
  filter(size_mean < 350000) %>%  # Filtrer store begivenheder (f.eks. i NY og Saint Louis)
  group_by(date) %>% 
  filter(israel_protest == 1) %>% 
  summarise(antal_deltagere = sum(size_mean)) %>%
  mutate(date = as.Date(date))  # Sørg for, at 'date' er korrekt

# Create the bar plot
isr_deltagere <- ggplot(pro_isr_part, aes(x = date, y = antal_deltagere)) +
  geom_bar(stat = "identity", fill = "grey40", color = "black") +  # Brug 'identity', da y allerede har optællinger
  scale_y_continuous(labels = comma) +
  scale_x_date(limits = manual_date_range, breaks = manual_breaks, date_labels = "%b %Y") +  # Synkroniseret datoakse med breaks
  geom_hline(yintercept = 0) +
  labs(
    title = "Pro-israelske demonstranter",
    x = "",
    y = "Antal demonstranter"
  ) +
  theme_minimal(base_size = 12) +  # Minimalistisk stil
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # Centreret x-akse tekst
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid = element_blank()
  )

# Print plottet
print(isr_deltagere)


# Samler plots ------------------------------------------------------------

# Load necessary library
library(patchwork)

# Kombiner plot for demonstrationer (lodret layout)
combined_demo <- pale_demo / isr_demo +
  plot_annotation(
    # title = "Pro-palæstinensiske og pro-israelske demonstrationer",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
    )
  )

# Print samlet plot for demonstrationer
print(combined_demo)

# Kombiner plot for deltagere (lodret layout)
combined_deltagere <- pale_deltagere / isr_deltagere +
  plot_annotation(
  #  title = "Pro-palæstinensiske og pro-israelske demonstranter",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
    )
  )

# Print samlet plot for deltagere
print(combined_deltagere)

# Gem plottende som PDF
ggsave(
  filename = "/Users/carlgammeltoft/Lokale dokumenter/R_speciale/5_mapping/3_output/combined_demo_vertical.pdf",
  plot = combined_demo,
  device = "pdf",
  width = 12,  # Juster bredden, så den passer til lodret layout
  height = 8  # Øg højden for at give plads til begge plots
)

ggsave(
  filename = "/Users/carlgammeltoft/Lokale dokumenter/R_speciale/5_mapping/3_output/combined_deltagere_vertical.pdf",
  plot = combined_deltagere,
  device = "pdf",
  width = 12,  # Juster bredden, så den passer til lodret layout
  height = 8  # Øg højden for at give plads til begge plots
)

### Samlede antal normative og ikke-normative demonstrationer
data_normative <- data_CCC %>% 
  filter(non_normative == 0)

data_non_normative <- data_CCC %>% 
  filter(non_normative == 1)

data_nn_isr <- data_non_normative %>% 
  filter(israel_protest == 1)

data_nn_pal <- data_non_normative %>% 
  filter(palestine_protest == 1)

data_n_isr <- data_normative %>% 
  filter(israel_protest == 1)


 
data_CCC$non_normative

