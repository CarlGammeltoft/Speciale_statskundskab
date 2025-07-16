# CCC - identifying Palestine AND ISRAEL protests

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
  "in solidarity with the people of Palestine",
  "for ending U.S. aid to Israel",
  "for lifting Israel's siege on Gaza",
  "against the suffering of families and children in Gaza",
  "stop arming Israel",
  "Palestine will be free",
  "free Palestine",
  "against U.S. aid and weapons sales to Israel",
  "for arms embargo on Israel",
  "No bombs for Israel",
  "for divesting from Israel",
  "for humanitarian assistance to Palestinians",
  "against killing Palestinians",
  "against U.S. aid to Israel"
)

neutral_phrases <- c(
  "for a ceasefire in Israel and Palestine",
  "for the release of all hostages and prisoners",
  "for humanitarian aid to all who are suffering in the war-torn regions"
)

israel_phrases <- c(
  "for release of Israeli hostages held by Hamas",
  "in solidarity with Israel",
  "for supporting Israel",
  "for Israel defending itself against Hamas"
)

# Samlede phrases
total_phrases <- c(israel_phrases, neutral_phrases, palestine_phrases)

# Create a new column 'palestine_protest' and apply conditions
data$palestine_protest <- apply(data, 1, function(row) {
  # Check if any phrase in 'palestine_phrases' is present in the 'claims' column
  if (any(sapply(palestine_phrases, function(phrase) grepl(phrase, row['claims'], ignore.case = TRUE)))) {
    return(1)
  } else {
    return(0)
  }
})


# Create a new column 'israel_protest' and apply conditions
data$israel_protest <- apply(data, 1, function(row) {
  # Check if any phrase in 'palestine_phrases' is present in the 'claims' column
  if (any(sapply(israel_phrases, function(phrase) grepl(phrase, row['claims'], ignore.case = TRUE)))) {
    return(1)
  } else {
    return(0)
  }
})

# Create a new column 'neutral_protest' and apply conditions
data$neutral_protest <- apply(data, 1, function(row) {
  # Check if any phrase in 'palestine_phrases' is present in the 'claims' column
  if (any(sapply(neutral_phrases, function(phrase) grepl(phrase, row['claims'], ignore.case = TRUE)))) {
    return(1)
  } else {
    return(0)
  }
})


# Create a new column 'total_protest' and apply conditions
data$total_protest <- apply(data, 1, function(row) {
  # Check if any phrase in 'palestine_phrases' is present in the 'claims' column
  if (any(sapply(total_phrases, function(phrase) grepl(phrase, row['claims'], ignore.case = TRUE)))) {
    return(1)
  } else {
    return(0)
  }
})

# Tæller antallet af Palæstina-demonstrationer
# Count the number of conflict-related statements
num_protests <- sum(data$total_protest)
num_pal_protests <- sum(data$palestine_protest)
num_isr_protests <- sum(data$israel_protest)
num_neu_protests <- sum(data$neutral_protest)

# Print the result
print(num_protests)

## Visualiserer antallet af gange politikerne referer til konflikten
# Ensure the date column is of Date type
data$date <- as.Date(data$date)

# Summarize the number of conflict-related statements by date
protests_counts <- data %>%
  group_by(date) %>%
  summarize(protests = sum(total_protest),
            pal_protests = sum(palestine_protest),
            isr_protests = sum(israel_protest),
            neu_protests = sum(neutral_protest)) %>%
  ungroup() 

# Ensure correct variable types
protests_counts$protests <- as.integer(protests_counts$protests)
protests_counts$pal_protests <- as.integer(protests_counts$pal_protests)
protests_counts$isr_protests <- as.integer(protests_counts$isr_protests)
protests_counts$neu_protests <- as.integer(protests_counts$neu_protests)
protests_counts$date <- as.Date(protests_counts$date)

# Reshape data to long format
protests_counts_long <- protests_counts %>%
  pivot_longer(cols = c(pal_protests, isr_protests, neu_protests), 
               names_to = "protest_type", 
               values_to = "count")

# Filter the data to include only dates on or after July 1, 2023
protests_counts_filtered <- protests_counts_long %>%
  filter(date >= as.Date("2023-07-01"))

# Reorder protest_type levels
protests_counts_filtered$protest_type <- factor(protests_counts_filtered$protest_type, 
                                                levels = c("pal_protests", "isr_protests", "neu_protests"))

ggplot(protests_counts_filtered, aes(x = date, y = count, fill = protest_type)) +
  geom_col(size = 1, width = 1) +  # Adjust bar width
  scale_fill_manual(values = c("pal_protests" = "#1f77b4",  # Custom colors
                               "isr_protests" = "#ff7f0e",
                               "neu_protests" = "#2ca02c")) +
  theme_minimal() +
  labs(x = "Date", y = "Number of Protests", fill = "Protest Type",
       title = "Stacked Bar Chart of Protests by Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels



# Laver variabel for om demo var non-normative ----------------------------
data <- data %>%
  mutate(non_normative = if_else(arrests_any == 1 | 
                                   injuries_crowd_any == 1 | 
                                   injuries_police_any == 1 | 
                                   property_damage_any == 1 | 
                                   chemical_agents == 1, 1, 0),
         violent = if_else(injuries_crowd_any == 1 | 
                             injuries_police_any == 1 | 
                             property_damage_any == 1, 1, 0))

# Udregner antallet af non-normative protester for Palæstina-demonstrationer
non_normative_total_count <- data %>%
  filter(non_normative == 1, total_protest == 1) %>%
  nrow()

non_normative_total_count

violent_count <- data %>%
  filter(violent == 1, total_protest == 1) %>%
  nrow()

violent_count

# Udregner selvstændigt gennemsnitsantallet -------------------------------

# Calculate the median of size_mean where palestine_protest == 1, ignoring NAs
median_size_total <- data %>%
  filter(total_protest == 1) %>%
  summarise(median_size = median(size_mean, na.rm = TRUE)) %>%
  pull(median_size)

# Udregner gennemsnittet 
mean_size_total <- data %>%
  filter(total_protest == 1) %>%
  summarise(mean_size = mean(size_mean, na.rm = TRUE)) %>%
  pull(mean_size)


# Replace NA values in size_mean where palestine_protest == 1 with the calculated median
data <- data %>%
  mutate(size_mean = if_else(total_protest == 1 & is.na(size_mean), median_size_total, size_mean))

# Gemmer data -------------------------------------------------------------

# Save the filtered dataset as a CSV file
write.csv(data, "3_output/CCC_classified.csv", row.names = FALSE)
write.csv(data, "1_input/CCC_classified.csv", row.names = FALSE)

