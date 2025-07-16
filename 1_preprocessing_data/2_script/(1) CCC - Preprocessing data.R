# CCC - Preprocessing data

# Loading libraries + setting wd ------------------------------------------
library(tidyverse)

# Sætter working directory
setwd("/Users/carlgammeltoft/Lokale dokumenter/R_speciale/1_preprocessing_data/")

# Indlæser data
data_p1 <- read.csv("1_input/raw/ccc_compiled_2017-2020.csv")
data_p2 <- read.csv("1_input/raw/ccc_compiled_2021-present.csv")

# Sammensætter data
data <- rbind(data_p1, data_p2)

# Preprocessing for further work -------------------------------------------------

## Indstiller perioden til start 2023
# Convert 'date' column to Date format if it's not already in that format
data_preprocessed <- data %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) # Adjust the format to match your date format

# Filter out rows with missing dates and those before the threshold date
threshold_date <- as.Date("2023-01-01")

data_preprocessed <- data_preprocessed %>%
  filter(!is.na(date)) %>%  # Remove rows with invalid or missing dates
  filter(date >= threshold_date)

# Tjekker mindste observation / tidligste dato
min_date <- min(data_preprocessed$date) #passer med 01-01-2023

## Tjekker issues
# Starter med at tælle antallet af gange hvert issue fremgår. 

# Split the 'issues' column by ";", unnest into individual rows, and count occurrences
issue_counts <- data_preprocessed %>%
  # Split the 'issues' column into a list of issues
  mutate(issues_split = str_split(issues, ";")) %>%
  # Unnest the list into rows (each issue gets its own row)
  unnest(issues_split) %>%
  # Trim any extra whitespace
  mutate(issues_split = trimws(issues_split)) %>%
  # Count the occurrences of each issue
  count(issues_split, sort = TRUE) %>%
  # Rename column for clarity
  rename(issue = issues_split, occurrences = n)

# View the issue counts
issue_counts

## Filtrerer data så det kun handler om følgende emner: "foreign affairs", "military", "religion", "democracy"
# Define the list of relevant issues
relevant_issues <- c("foreign affairs", "military", "religion", "democracy")

# Filter rows where any of the relevant issues are present
filtered_data <- data_preprocessed %>%
  filter(str_detect(issues, paste(relevant_issues, collapse = "|")))

# Laver et udsnit af data,som gør det nemmere at arbejde videre med i Colab
random_sample <- filtered_data %>% 
  sample_n(150)

# Gemmer data -------------------------------------------------------------

# Save the filtered dataset as a CSV file
write.csv(filtered_data, "3_output/CCC_preprocessed.csv", row.names = FALSE)
write.csv(filtered_data, "1_input/CCC_preprocessed.csv", row.names = FALSE)

# Save the subset dataset as a CSV file
write.csv(random_sample, "3_output/CCC_preprocessed_short.csv", row.names = FALSE)
write.csv(random_sample, "1_input/CCC_preprocessed_short.csv", row.names = FALSE)


