### Kommunikation meta

library(tidyverse)

setwd("/Users/carlgammeltoft/Lokale dokumenter/R_speciale/5_mapping/1_input/")
data_PRL <- read.csv("data_finalized.csv")

# Begrænser tidsperioden --------------------------------------------------

# Filtrerer data til kun at have obs. med count_statement > 0
data_PRL <- data_PRL %>% 
  filter(count_statement > 0)

# Indstiller den rette dato for classified
data_PRL <- data_PRL %>% 
  filter(date >= "2023-09-01")

# Graf for udtalelser over tid -----------------------------

statement_party <- data_PRL %>% 
  group_by(date, party) %>%
  summarise(antal = sum(count_statement),
            party = first(party),
            bioguide_id = first(bioguide_id))

# Ensure 'date' is a Date type
statement_party <- statement_party %>%
  mutate(date = as.Date(date))  # Convert to Date if not already

# Create the bar plot (fordelt på partier)
ggplot(statement_party, aes(x = date, y = antal, fill = party)) + 
  geom_bar(stat = "identity", color = "grey60") +  # Use 'identity' because y already has counts
  labs(
    title = "Antal konfliktrelaterede udtalelser over tid",
    x = "Dato",
    y = "Antal udtalelser"
  ) +
  theme_minimal(base_size = 12) +  # Clean minimalist style
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # Rotate x-axis for readability
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  scale_fill_manual(values = c("blue", "red"))  # Assign specific colors for each party

# Create the bar plot (samlet)
bar_plot <- ggplot(statement_party, aes(x = date, y = antal)) + 
  geom_bar(stat = "identity", color = "black") +  # Use 'identity' because y already has counts
  geom_hline(yintercept = 0) +
  labs(
   #  title = "",
    x = "",
    y = "Antal udtalelser"
  ) +
  theme_minimal(base_size = 12) +  # Clean minimalist style
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # Rotate x-axis for readability
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid = element_blank()
  )

print(bar_plot)

# Gemmer plottet
# Save the plot
ggsave(filename = "/Users/carlgammeltoft/Lokale dokumenter/R_speciale/5_mapping/3_output/Daglige_udtalelser.pdf",  # Replace with your desired path
       plot = bar_plot,
       device = "pdf",
       width = 12,  # Width in inches
       height = 4)  # Height in inches

# Kernel plot - fordelt på parti ----------------------------------------------

# Create the scatter + boxplot combination
ggplot(statement_party, aes(x = party, y = antal, color = party)) +
  geom_boxplot(width = 0.4, outlier.shape = NA, alpha = 0.3) +  # Boxplot without outliers
  geom_jitter(position = position_jitter(width = 0.2, height = 0), size = 1.5, alpha = 0.6) +  # Jittered points
  labs(
    title = "Distribution of Statements by Party",
    x = "Party",
    y = "Number of Statements"
  ) +
  scale_color_manual(values = c("Republican" = "red", "Democrat" = "blue")) +  # Custom colors
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none",  # Hide legend if unnecessary
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )

library(ggplot2)

# Convert 'antal' to numeric and 'party' to a factor
statement_party$antal <- as.numeric(statement_party$antal)
statement_party$party <- factor(statement_party$party, levels = c("Democrat", "Republican"))

# Plot: Combined density curve, boxplot, and points
violin <- ggplot(statement_party, aes(x = party, y = antal, fill = party)) + 
  geom_violin(aes(fill = party), alpha = 0.6, color = NA, scale = "width", trim = FALSE) +  # Density curve (violin)
  geom_boxplot(width = 0.2, outlier.shape = NA, position = position_dodge(width = 0.9)) +  # Overlaid boxplot
  geom_jitter(aes(color = party), position = position_jitter(width = 0.15, height = 0), size = 1.2, alpha = 0.4) +  # Points
  scale_fill_manual(values = c("Democrat" = "#377EB8", "Republican" = "#E41A1C")) +  # Blue and red for parties
  scale_color_manual(values = c("Democrat" = "#377EB8", "Republican" = "#E41A1C")) +
  labs(
    # title = "Kernel Density and Boxplot of Daily Communication by Party",
    x = "Parti",
    y = "Antal udtalelser"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "none",  # Hide legend if unnecessary
    panel.grid = element_blank()
  )

print(violin)


statement_party$communication_status <- ifelse(statement_party$antal == 0, "No Communication", "With Communication")
ggplot(statement_party, aes(x = party, y = antal, fill = communication_status)) +
  geom_violin(scale = "width", trim = FALSE, alpha = 0.3, position = position_dodge(width = 0.9)) +
  geom_boxplot(width = 0.2, position = position_dodge(width = 0.9), outlier.shape = NA) +
  scale_fill_manual(values = c("No Communication" = "grey80", "With Communication" = "#E41A1C")) +
  labs(title = "Communication Distribution by Party", y = "Number of Communications") +
  theme_minimal(base_size = 14)

ggplot(statement_party, aes(x = party, y = antal, fill = party)) +
  geom_violin(trim = FALSE, scale = "width", alpha = 0.3) +
  geom_boxplot(width = 0.2, outlier.shape = NA) +
  scale_y_continuous(trans = "log1p") +  # Log transformation with +1 to include zeros
  labs(title = "Log-Transformed Communication Distribution", y = "Log(Number of Communications)") +
  theme_minimal(base_size = 14)

ggplot(statement_party, aes(x = antal, fill = party)) +
  geom_histogram(binwidth = 5, color = "black", alpha = 0.7) +
  facet_wrap(~ party, scales = "free_y") +
  labs(title = "Distribution of Communication by Party", x = "Number of Communications") +
  theme_minimal(base_size = 14)




# Rename the 'party' variable values
statement_party <- statement_party %>%
  mutate(party = case_when(
    party == "Republican" ~ "Republikanere",
    party == "Democrat" ~ "Demokrater",
    TRUE ~ party  # Keep other values unchanged
  ))

# Convert 'antal' to numeric and 'party' to a factor with the updated levels
statement_party$antal <- as.numeric(statement_party$antal)
statement_party$party <- factor(statement_party$party, levels = c("Demokrater", "Republikanere"))

# Plot: Combined density curve, boxplot, and points
violin <- ggplot(statement_party, aes(x = party, y = antal, fill = party)) + 
  geom_violin(aes(fill = party), alpha = 0.6, color = NA, scale = "width", trim = FALSE) +  # Density curve (violin)
  geom_boxplot(width = 0.2, outlier.shape = NA, position = position_dodge(width = 0.9)) +  # Overlaid boxplot
  geom_jitter(aes(color = party), position = position_jitter(width = 0.15, height = 0), size = 1.2, alpha = 0.4) +  # Points
  scale_fill_manual(values = c("Demokrater" = "#377EB8", "Republikanere" = "#E41A1C")) +  # Blue and red for parties
  scale_color_manual(values = c("Demokrater" = "#377EB8", "Republikanere" = "#E41A1C")) +
  labs(
    x = "Parti",
    y = "Antal udtalelser"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "none",  # Hide legend if unnecessary
    panel.grid = element_blank()
  )

print(violin)

# Gemmer plottet
# Save the plot
ggsave(filename = "/Users/carlgammeltoft/Lokale dokumenter/R_speciale/5_mapping/3_output/Daglige_udtalelser_violin.pdf",  # Replace with your desired path
       plot = violin,
       device = "pdf",
       width = 12,  # Width in inches
       height = 6)  # Height in inches
