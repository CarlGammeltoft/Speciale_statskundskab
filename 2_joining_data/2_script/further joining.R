### Futher joining ###

#### DENNE FIL ER NOK OVERFLØDIG NU ####


# Sætter working directory
library(tidyverse)
setwd("/Users/carlgammeltoft/Lokale dokumenter/R_speciale/2_joining_data/")

# Indlæser data
data <- read.csv("3_output/data_full.csv")
profiles <- read.csv("1_input/profiles.csv")

# Finder relevante variable i profiles data og omdøber dem -------------------
## Policy discussion
max(profiles$communication_policy_mean)

## Constructive debate
max(profiles$communication_policy_legislative_discussion_mean)

## Personal attacks
max(profiles$communication_attack_personal_mean)

## Accomplishment
max(profiles$communication_outcome_creditclaiming_mean)

## Bipartisant
max(profiles$communication_outcome_bipartisanship_mean)

profiles_selected <- profiles %>% 
  dplyr::select(communication_policy_mean, communication_policy_legislative_discussion_mean,
         communication_attack_personal_mean, communication_outcome_creditclaiming_mean,
         communication_outcome_bipartisanship_mean, ideology_ideology, bioguide_id)

profiles_selected <- profiles_selected %>% 
  rename(policy_discussion = communication_policy_mean,
         constructive_debate = communication_policy_legislative_discussion_mean,
         personal_attack = communication_attack_personal_mean, 
         accomplishment = communication_outcome_creditclaiming_mean,
         bipartisan = communication_outcome_bipartisanship_mean, 
         ideology = ideology_ideology)

# Fjerner kolonnerne fra data
data <- data %>% 
  dplyr::select(- c(ideology_ideology,
                    communication_policy_legislative_discussion_mean,
                    communication_attack_personal_mean,
                    communication_outcome_creditclaiming_mean,
                    communication_outcome_bipartisanship_mean,
                    communication_attack_policy_mean))

# Og sætter nu kolonnerne fra profiles selected på:
data <- merge(data, profiles_selected, by = "bioguide_id")

### Gemmer data
# Gemmer data -------------------------------------------------------------
write.csv(data, "/Users/carlgammeltoft/Lokale dokumenter/R_speciale/2_joining_data/3_output/data_full_new.csv")
write.csv(data, "/Users/carlgammeltoft/Lokale dokumenter/R_speciale/3_modelling/1_input/data_full_new.csv")
write.csv(data, "/Users/carlgammeltoft/Documents/R_speciale_løbende/data_17102024.csv")



