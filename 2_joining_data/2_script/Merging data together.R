### Merging data together

# Loading libraries + setting wd ------------------------------------------
library(tidyverse)
library(sf)

# Sætter working directory
setwd("/Users/carlgammeltoft/Lokale dokumenter/R_speciale/2_joining_data/1_input/")

# Indlæser data
PRL <- read.csv("PRL_districts.csv")
CCC <- dget("CCC_districts_assigned.txt")
Profiles <- read.csv("profiles.csv")


# Formål med script -------------------------------------------------------

# Jeg har nu to dataframes. En for CCC-data (protest-data) og en for politikernes
# retorik (PRL). Begge scripts indeholder informationer om, hvorvidt retorikken/
# demonstrationen er relateret til konflikten.

# Jeg vil gerne have en endelig dataframe, der for hver dato for hver politiker
# måler hvor mange demonstrationer, der er i vedkommendes distrikt og hvor ofte 
# politikeren omtaler konflikten.


# Sammenkobling af de to datasæt ------------------------------------------

## 1 - Filterering
# Da jeg kun ønsker at undersøge palæstina-demo og palæstina udtalelser, starter 
# jeg med at filtrere de to datasæt, så de kun indeholder palæstina-udtalelser og
# demonstrationer

# Filter out rows with empty geometries
CCC_filtered <- CCC %>% filter(!st_is_empty(geometry))

PRL_filtered <- PRL %>% 
  filter(conflict_related == 1)

## 2 - Summarizer de to df hver for sig og laver en optælling - 
# For CCC - Pr. dato pr. politiker hvor mange gange er der demo
# For PRL - Pr. dato pr. politiker hvor mange gange udtaleler vedkommende sig om konflikten

CCC_summarized <- CCC_filtered %>% 
  group_by(date, bioguide_id) %>% 
  summarize(count_protest = sum(total_protest == 1, na.rm = TRUE), .groups = "drop",
            count_protest_size = sum(size_mean, na.rm = T),
            count_non_normative = sum(non_normative, na.rm = T))

# Starter med at fjerne senatorerne i PRL-data (de kan altid medtages igen...)
PRL_filtered <- PRL_filtered %>% 
  filter(type == "rep")

PRL_summarized <- PRL_filtered %>% 
  # Convert conflict_related to numeric, making non-numeric values NA
  mutate(conflict_related = suppressWarnings(as.numeric(conflict_related))) %>%
  # Group and summarize
  group_by(date, bioguide_id) %>% 
  summarize(count_statement = sum(conflict_related == 1, na.rm = TRUE), .groups = "drop",
            sum_legislative_discussion = sum(policy_legislative_discussion == 1, na.rm = TRUE),
            sum_bipartisan = sum(outcome_bipartisanship == 1, na.rm = TRUE),
            sum_attack = sum(attack == 1, na.rm = TRUE),
            sum_floor = sum(source == "floor", na.rm = T),
            sum_newsletters = sum(source == "newsletters", na.rm = T),
            sum_source_statements = sum(source == "statements", na.rm = T)) 

## Merger de to dataframes sammen pba. "date"
data <- merge(CCC_summarized, PRL_summarized, by = c("date", "bioguide_id"), all = T)

# Erstatter NA's med 0, da det jo svarer til, at der ikke har været nogen demonstrationer
# eller udtalelser i distriktet...
data <- data %>% 
  mutate(count_protest = ifelse(is.na(count_protest), 0, count_protest),
         count_statement = ifelse(is.na(count_statement), 0, count_statement))

# Sorterer efter dato
data <- data %>% ungroup() %>% arrange(date)
data <- data %>% arrange(bioguide_id, date)

# Sorterer NA's fra (det er de distrikter, der ikke har nogen politiker - og DC)
data <- data %>% 
  filter(!(is.na(bioguide_id)))

# Summarizer hvor mange protester og udtalelser, der har været i alt for hver politiker
# for at se, om der er noget, der ligner en sammenhæng
data_summarized <- data %>% 
  group_by(bioguide_id) %>% 
  summarise(
    all_protests = sum(count_protest, na.rm = TRUE),
    all_statements = sum(count_statement, na.rm = TRUE)
  )


# Tilføjer politiker-oplysninger vha. bioguide_id -------------------------

# Her skal jeg altså merge de relevante kolonner fra PRL med de relevante
# kolonner fra data

# Jeg starter med at finde de relevante kolonner fra PRL
PRL_rel <- PRL %>% 
  dplyr::select(bioguide_id, gender, birthday, party, type, first_name, last_name) %>% 
  group_by(bioguide_id) %>% 
  summarize(gender = max(gender),
            birthday = max(birthday),
            party = max(party),
            type = max(type),
            first_name = max(first_name),
            last_name = max(last_name))

# filtrer så det kun er rep
PRL_rel <- PRL_rel %>% 
  filter(type == "rep")

## Merger sammen med data 
data_full <- merge(data, PRL_rel, by = "bioguide_id")

# Erstatter NA's med 0'er
data_full <- data_full %>% 
  mutate(sum_legislative_discussion = ifelse(is.na(sum_legislative_discussion), 0, data_full$sum_legislative_discussion),
         sum_bipartisan = ifelse(is.na(sum_bipartisan), 0, data_full$sum_bipartisan),
         sum_attack = ifelse(is.na(sum_attack), 0, data_full$sum_attack),
         count_protest_size = ifelse(is.na(count_protest_size), 0, data_full$count_protest_size),
         count_non_normative = ifelse(is.na(count_non_normative), 0, data_full$count_non_normative),
         sum_newsletters = ifelse(is.na(sum_newsletters), 0, data_full$sum_newsletters),
         sum_floor = ifelse(is.na(sum_floor), 0, data_full$sum_floor),
         sum_source_statements = ifelse(is.na(sum_source_statements), 0, data_full$sum_source_statements))

# Laver en aldersvariabel
today <- Sys.Date()

data_full <- data_full %>% 
  mutate(age = as.numeric(today - as.Date(birthday)))  # Calculate age in days

# If you want age in years, divide by 365.25 (accounting for leap years)
data_full <- data_full %>%
  mutate(age_years = age / 365.25) %>%   # Calculate age in years
  dplyr::select(-age)


# Tilføjer en masse 0'er --------------------------------------------------

# Tilføjer alle datoer til datosættet - dvs. jeg tilføjer en masse 0'er...
# Ensure your date column is in Date format
data_full <- data_full %>%
  mutate(date = as.Date(date))

# Set the start date to January 1, 2023
min_date <- as.Date("2023-01-01")

# Find the maximum date in the dataset
max_date <- max(data_full$date, na.rm = TRUE)

# For each bioguide_id, set the max value for each remaining variable
# List the variables that need to be zero-filled
vars_zero <- c("count_protest", "count_statement", "sum_legislative_discussion", 
               "sum_bipartisan", "sum_attack", "count_non_normative", "count_protest_size",
               "sum_newsletters", "sum_floor", "sum_source_statements")

# Create a full sequence of dates for each politician (bioguide_id)
data_full <- data_full %>%
  group_by(bioguide_id) %>%
  
  # Fill in missing dates from 2023-01-01 to max_date
  complete(date = seq(min_date, max_date, by = "day")) %>%
  
  # Replace missing dates' specified variables with 0
  replace_na(list(count_protest = 0, 
                  count_statement = 0, 
                  sum_legislative_discussion = 0, 
                  sum_bipartisan = 0, 
                  sum_attack = 0,
                  count_protest_size = 0,
                  count_non_normative = 0,
                  sum_newsletters = 0,
                  sum_floor = 0,
                  sum_source_statements = 0)) %>%
  
  # Fill the other variables with the max value for each bioguide_id, but handle NA values
  mutate(across(-c(date, all_of(vars_zero)), 
                ~ ifelse(is.na(.), max(., na.rm = TRUE), max(., na.rm = TRUE)))) %>%  # Replace `NA` with 0 in max calculation
  
  # Ungroup to return to the original structure
  ungroup()


## Introducerer et lag
# Ensure the date column is of Date class
# data_full <- data_full %>%
#  mutate(date = as.Date(date))  # Convert to Date format if necessary

# Function to calculate the 14-day average
# calculate_avg_statements <- function(data) {
#  data %>%
#    mutate(avg_statements_14days = sapply(date, function(d) {
#      # Filter data to get records in the next 14 days for the same bioguide_id
#      next_14_days <- filter(data, 
#                             bioguide_id == bioguide_id[1] & 
#                               date >= d & 
#                               date <= (d + 14))  # Adding 14 days
#     # Calculate total number of statements over the next 14 days
#      sum(next_14_days$count_statement, na.rm = TRUE)
#    }))
#}

# Apply this function for each bioguide_id
#data_full_lagged <- data_full %>%
#  group_by(bioguide_id) %>%
#  do(calculate_avg_statements(.)) %>%
#  ungroup()

# Remove rows with missing values in the relevant columns
#data_full_lagged_clean <- data_full_lagged %>%
#  drop_na(count_protest, count_statement)

#data_full_clean <- data_full %>%
#  drop_na(count_protest, count_statement)


# Laver variabel for vægtede størrelse ------------------------------------
#data_full_lagged_clean <- data_full_lagged_clean %>%
#  mutate(log_protest_size = log1p(count_protest_size))

#data_full_clean <- data_full_clean %>%
#  mutate(log_protest_size = log1p(count_protest_size))

# Tilføjer (statisk) politiker data fra PRL -----------------------------------------
Profiles_selected <- Profiles %>% 
  dplyr::select(bioguide_id, 
         communication_policy_mean,
         communication_policy_legislative_discussion_mean,
         communication_attack_personal_mean,
         communication_outcome_creditclaiming_mean,
         communication_outcome_bipartisanship_mean,
         ideology_ideology,
         communication_count)

# Omdøber kolonnerne, så de giver bedre mening ift. data
Profiles_selected <- Profiles_selected %>% 
  rename(policy_discussion = communication_policy_mean,
         constructive_debate = communication_policy_legislative_discussion_mean,
         personal_attack = communication_attack_personal_mean, 
         accomplishment = communication_outcome_creditclaiming_mean,
         bipartisan = communication_outcome_bipartisanship_mean, 
         ideology = ideology_ideology,
         total_communication_all_time = communication_count)

# Merger data på
data_full_clean <- merge(data_full, Profiles_selected, by = "bioguide_id")
# Fjerner communication_attack_mean, da den er na

#data_full_clean <- data_full_clean %>% 
#  dplyr::select(-communication_attack_mean)


# Tilføjer distrikt data --------------------------------------------------
district_data <- dget("district_data.txt")

# Udvælger relevante kolonner
district_data_sel <- district_data %>% 
  dplyr::select("bioguide_id", "district", "state")

# Merger det på data_full_clean
data_full_clean <- merge(data_full_clean, district_data_sel, by = "bioguide_id")

## Indlæser populationsdata
pop_data <- read.csv("US_census_pop_data.csv")
pop_meta_data <- read.csv("US_pop_metadata.csv")

# Udvælger relevante kolonner
pop_data_sel <- pop_data %>% 
  dplyr::select("S0101_C01_001E", "S0101_C01_028E", "S0101_C01_032E", "S0101_C01_033E", "NAME") %>% 
  transmute(total_pop = S0101_C01_001E, over_60 = S0101_C01_028E, median_age = S0101_C01_032E, 
         sex_ratio_men_pr_female = S0101_C01_033E, district = NAME)

# Fjerner første række:
pop_data_sel <- pop_data_sel[-1, ]

# Laver variabel, der måler andelen af befolkningen over 60 år
pop_data_sel <- pop_data_sel %>% 
  mutate(total_pop = as.numeric(total_pop),
         over_60 = as.numeric(over_60)) %>% 
  mutate(share_over_60 = over_60 / total_pop * 100)

# Laver ny variabel for stat og district
pop_data_sel <- pop_data_sel %>%
  mutate(
    state = str_extract(district, "[A-Za-z ]+$") %>% str_trim(),
    district = str_extract(district, "\\d+")
  )

# Ændrer variablene så der bruges forkortelser i stedet for hele statens navn
# Define the named vector for state abbreviations
state_abbreviations <- c(
  "Alabama" = "AL", "Alaska" = "AK", "Arizona" = "AZ", "Arkansas" = "AR",
  "California" = "CA", "Colorado" = "CO", "Connecticut" = "CT", "Delaware" = "DE",
  "Florida" = "FL", "Georgia" = "GA", "Hawaii" = "HI", "Idaho" = "ID",
  "Illinois" = "IL", "Indiana" = "IN", "Iowa" = "IA", "Kansas" = "KS",
  "Kentucky" = "KY", "Louisiana" = "LA", "Maine" = "ME", "Maryland" = "MD",
  "Massachusetts" = "MA", "Michigan" = "MI", "Minnesota" = "MN", "Mississippi" = "MS",
  "Missouri" = "MO", "Montana" = "MT", "Nebraska" = "NE", "Nevada" = "NV",
  "New Hampshire" = "NH", "New Jersey" = "NJ", "New Mexico" = "NM", "New York" = "NY",
  "North Carolina" = "NC", "North Dakota" = "ND", "Ohio" = "OH", "Oklahoma" = "OK",
  "Oregon" = "OR", "Pennsylvania" = "PA", "Rhode Island" = "RI", "South Carolina" = "SC",
  "South Dakota" = "SD", "Tennessee" = "TN", "Texas" = "TX", "Utah" = "UT",
  "Vermont" = "VT", "Virginia" = "VA", "Washington" = "WA", "West Virginia" = "WV",
  "Wisconsin" = "WI", "Wyoming" = "WY"
)

# Ændrer distrikter 118 til 0
# Update the district column and replace state names with abbreviations
pop_data_sel <- pop_data_sel %>%
  mutate(
    district = ifelse(district == "118", "0", district),
    state = state_abbreviations[state]
  )

# Jeg merger populationsdata på min samlede dataframe
data_full_clean <- merge(data_full_clean, pop_data_sel, by = c("district", "state"), all = TRUE)

# Tester om alt data er kommet rigtigt på
TEST <- data_full_clean %>% 
  group_by(district, state) %>% 
  summarize(pop = max(total_pop)) %>% 
  arrange(state)


# Tilføjer komite-data ----------------------------------------------------
komite <- readxl::read_excel("komiteer.xlsx")

komite <- komite %>% 
  rename(appr = `House Appropriations Committee`,
          arme = `House Armed Services Committee` ,
          fore = `House Foreign Affairs Committee`)

# Seperer name-kolonnen
komite <- komite %>%
  separate(Name, into = c("first_name", "last_name"), sep = " ", extra = "merge", fill = "right")

# Beholder kun det faktiske last_name
komite <- komite %>%
  mutate(last_name = word(last_name, -1))

# Indlæser legislator_data
legislator_data <- read.csv("legislators-current.csv")

# Bruger legislator_data til at matche med
legislator_data_slim <- legislator_data %>% 
  dplyr::select(first_name, last_name, bioguide_id)

# Matcher de to datasæt
matched_data <- komite %>%
  left_join(legislator_data_slim, by = c("first_name", "last_name"))

# Extracter unikke bioguide id og laver kolonne, der indikerer at de er med i relevant
# komite
committee_unique_bioguide <- matched_data %>%
  distinct(bioguide_id) %>%
  mutate(relevant_committee = 1) %>% 
  filter(!(is.na(bioguide_id)))

# Nu kan jeg sætte denne kolonne på min data_full_clean
# Join the data_full_clean dataframe with committee_unique_bioguide on bioguide_id
data_full_clean <- data_full_clean %>%
  left_join(committee_unique_bioguide, by = "bioguide_id")

# View the updated dataframe
print(data_full_clean)


# Gemmer data -------------------------------------------------------------
write.csv(data_full_clean, "/Users/carlgammeltoft/Lokale dokumenter/R_speciale/2_joining_data/3_output/data_full.csv")
write.csv(data_full_clean, "/Users/carlgammeltoft/Lokale dokumenter/R_speciale/3_modelling/1_input/data_full.csv")
write.csv(data_full_clean, "/Users/carlgammeltoft/Documents/R_speciale_løbende/data_17102024.csv")
