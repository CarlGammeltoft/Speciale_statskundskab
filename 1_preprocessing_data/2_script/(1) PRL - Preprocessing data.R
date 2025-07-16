### Script - preprocessing PRL-data ###

# 1 Indlæser data og libraries --------------------------------------------------------
# Indlæser libraries

library(tidyverse)

# Indlæser data 
setwd("/Users/carlgammeltoft/Lokale dokumenter/R_speciale/1_preprocessing_data")

# Data 2023
data_2023 <- read.csv("1_input/raw/2023.csv")
# Data 2024
data_2024 <- read.csv("1_input/raw/2024.csv")


# 2 Behandler data ----------------------------------------------

# Fjerner tweets, da de ikke indeholder tekst
# Fjerner udsagn, der ikke har med politik at gøre

data_2023_T <- data_2023 %>% 
  filter(source != "tweets",
         policy == 1)
data_2024_T <- data_2024 %>% 
  filter(source != "tweets",
         policy == 1)

# Sammensætter datasættet for 2023 og 2024
PRL_data <- rbind(data_2023_T, data_2024_T)

# Renser policy_area kolonnen for at kunne lave optælling af policy-emner
Seperated_data <- PRL_data %>%
  mutate(policy_split = str_split(policy_area, ", ")) %>% # Splitter stregene
  mutate(policy_split_clean = gsub("\\[|\\]|'", "", policy_split))

# Renser videre
Seperated_data$policy_split_clean_ <- gsub('c\\(|\\)|\\"', '', Seperated_data$policy_split_clean)

## Tæller policy-areas
# Split the policy areas by comma and unlist to create a long vector of all individual policy areas
policy_areas <- unlist(strsplit(Seperated_data$policy_split_clean_, ", "))

# Create a table to count the occurrences of each policy area
policy_area_counts <- table(policy_areas)

# Convert the table to a dataframe for easier viewing and sorting
policy_area_counts_df <- as.data.frame(policy_area_counts)

# Sort the dataframe by frequency in descending order
policy_area_counts_df <- policy_area_counts_df[order(-policy_area_counts_df$Freq), ]

# View the results
print(policy_area_counts_df)

# Bruger ovenstående til at få en idé om, hvilke policy-områder politikerne taler om.
# Jeg finder, at der er tre kategorier, der er relevante for de demonstrationer,
# jeg ønsker at undersøge, herunder 'Armed Forces and National Security', 
# 'International Affairs','Foreign Trade and International Finance'

## Filtrerer data
# 'Armed Forces and National Security', 'International Affairs','Foreign Trade and International Finance'
PRL_data_foreign <- PRL_data %>%
  filter(grepl("Armed Forces and National Security|International Affairs|Foreign Trade and International Finance", policy_area))

# Omrking 135.000 udtalelser omhandler ét af de tre policy-områder.

# Laver et udsnit af data, som gør det nemmere at arbejde videre med i Colab
random_sample <- PRL_data_foreign %>% 
  sample_n(150)

# Gemmer data -------------------------------------------------------------
# Save the filtered dataset as a CSV file
write.csv(PRL_data_foreign, "3_output/PRL_preprocessed.csv", row.names = FALSE)
write.csv(PRL_data_foreign, "1_input/PRL_preprocessed.csv", row.names = FALSE)

# Save the subset dataset as a CSV file
write.csv(random_sample, "3_output/PRL_preprocessed_short.csv", row.names = FALSE)
write.csv(random_sample, "1_input/PRL_preprocessed_short.csv", row.names = FALSE)
