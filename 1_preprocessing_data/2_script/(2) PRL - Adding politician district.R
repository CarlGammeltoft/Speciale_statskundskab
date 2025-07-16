# PRL - Adding politician district

# Indlæser data og libraries --------------------------------------------------------
# Indlæser libraries

library(tidyverse)

# Indlæser data 
setwd("/Users/carlgammeltoft/Lokale dokumenter/R_speciale/1_preprocessing_data")

# Data
data <- read.csv("1_input/PRL_classified.csv")
legislator_data <- read.csv("1_input/raw/legislators-current.csv")
district_data <- dget("/Users/carlgammeltoft/Lokale dokumenter/R_speciale/1_preprocessing_data/1_input/district_data.txt")

# Tilføjer politikernes distrikt til data -------------------------------------------------------

# Udvælger relevante kolonner fra legislator_data
legislator_data_sel <- legislator_data %>% 
  select(district, party, senate_class, gender, birthday, bioguide_id)

# Merger på bioguide_id
data_merged <- merge(data, legislator_data_sel, by = "bioguide_id")

## data_merged består nu af politikerne og deres distrikt.

# Tilføjer distrikternes geografi til data --------------------------------

# Starter med at lave ny kolonne, hvor jeg kombinerer stat og distrikt for data_merged (politiker-data)
# Gør det kun for politikere i repræsentanternes hus... Ellers får de bare staten
data_merged <- data_merged %>%
  mutate(state_district = if_else(is.na(district), state, paste0(state, district)))

# Gemmer data -------------------------------------------------------------
write.csv(data_merged, "3_output/PRL_districts.csv")
write.csv(data_merged, "1_input/PRL_districts.csv")

# Gemmer til næste step
write.csv(data_merged, "/Users/carlgammeltoft/Lokale dokumenter/R_speciale/2_joining_data/1_input/PRL_districts.csv")
