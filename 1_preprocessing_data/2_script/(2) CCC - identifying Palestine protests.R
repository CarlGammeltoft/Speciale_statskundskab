# CCC - identifying Palestine protests

# Loading libraries + setting wd ------------------------------------------
library(tidyverse)

# Sætter working directory
setwd("/Users/carlgammeltoft/Lokale dokumenter/R_speciale/1_preprocessing_data")

# Indlæser data
data <- read.csv("1_input/CCC_preprocessed.csv")


# Arbejder med data -------------------------------------------------------
# Define the phrases you're looking for
palestine_phrases <- c(
  "for Palestinian liberation",
  "against genocide in Gaza",
  "for ceasefire in Gaza",
  "against Israel's occupation of Palestine",
  "in solidarity with the people of Palestine"
)

# Create a new column 'palestine_protest' and apply conditions
data$palestine_protest <- apply(data, 1, function(row) {
  # Check if any phrase in 'palestine_phrases' is present in the 'claims' column
  if (any(sapply(palestine_phrases, function(phrase) grepl(phrase, row['claims'], ignore.case = TRUE)))) {
    return(1)
  } else {
    return(0)
  }
})

# Tæller antallet af Palæstina-demonstrationer
# Count the number of conflict-related statements
num_protests <- sum(data$palestine_protest)

# Print the result
print(num_protests)

## Visualiserer antallet af gange politikerne referer til konflikten
# Ensure the date column is of Date type
data$date <- as.Date(data$date)

# Summarize the number of conflict-related statements by date
protests_counts <- data %>%
  group_by(date) %>%
  summarize(protests = sum(palestine_protest)) %>%
  ungroup() 

# Ændrer variabeltyper
protests_counts$protests <- as.integer(protests_counts$protests)
protests_counts$date <- as.Date(protests_counts$date)

# Create the plot
ggplot(protests_counts, aes(x = date, y = protests)) +
  geom_line(size = 0.25) +
  theme_minimal()



# Laver variabel for om demo var non-normative ----------------------------
data <- data %>%
  mutate(non_normative = if_else(arrests_any == 1 | 
                                   injuries_crowd_any == 1 | 
                                   injuries_police_any == 1 | 
                                   property_damage_any == 1 | 
                                   chemical_agents == 1, 1, 0))

# Udregner antallet af non-normative protester for Palæstina-demonstrationer
non_normative_palestine_count <- data %>%
  filter(non_normative == 1, palestine_protest == 1) %>%
  nrow()

non_normative_palestine_count


# Udregner selvstændigt gennemsnitsantallet -------------------------------

# Calculate the median of size_mean where palestine_protest == 1, ignoring NAs
median_size_palestine <- data %>%
  filter(palestine_protest == 1) %>%
  summarise(median_size = median(size_mean, na.rm = TRUE)) %>%
  pull(median_size)

# Replace NA values in size_mean where palestine_protest == 1 with the calculated median
data <- data %>%
  mutate(size_mean = if_else(palestine_protest == 1 & is.na(size_mean), median_size_palestine, size_mean))

# Gemmer data -------------------------------------------------------------

# Save the filtered dataset as a CSV file
write.csv(data, "3_output/CCC_classified.csv", row.names = FALSE)
write.csv(data, "1_input/CCC_classified.csv", row.names = FALSE)

