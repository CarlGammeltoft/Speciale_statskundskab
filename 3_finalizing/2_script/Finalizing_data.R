### Finalizing_data


# Indlæser data -----------------------------------------------------------
setwd("/Users/carlgammeltoft/Lokale dokumenter/R_speciale/3_finalizing/")

data <- read.csv("1_input/data_full.csv")
comp_races <- read.csv("1_input/Complete_Indexed_Dataset_of_Districts_and_States.csv")

library(tidyverse)
library(zoo)

# Kommunikationsvariablen --------------------------------------------------
# Convert date to Date type
data$date <- base::as.Date(data$date)

# Create the dependent variable: Sum of communication from day t to t+x
data <- data %>%
  group_by(bioguide_id, district) %>%
  arrange(date) %>%
  mutate(
    communication_window_1 = rollapplyr(count_statement, width = 2, sum, fill = NA, align = "left"),  
    communication_window_7 = rollapplyr(count_statement, width = 8, sum, fill = NA, align = "left"),
    communication_window_14 = rollapplyr(count_statement, width = 15, sum, fill = NA, align = "left"),
    communication_window_21 = rollapplyr(count_statement, width = 22, sum, fill = NA, align = "left"),
    communication_window_30 = rollapplyr(count_statement, width = 31, sum, fill = NA, align = "left")
  )

# For floor-kommunikation
data <- data %>%
  group_by(bioguide_id, district) %>%
  arrange(date) %>%
  mutate(
    communication_window_1_floor = rollapplyr(sum_floor, width = 2, sum, fill = NA, align = "left"),  
    communication_window_7_floor = rollapplyr(sum_floor, width = 8, sum, fill = NA, align = "left"),
    communication_window_14_floor = rollapplyr(sum_floor, width = 15, sum, fill = NA, align = "left"),
    communication_window_21_floor = rollapplyr(sum_floor, width = 22, sum, fill = NA, align = "left"),
    communication_window_30_floor = rollapplyr(sum_floor, width = 31, sum, fill = NA, align = "left")
  )

# For newsletter kommunikation
data <- data %>%
  group_by(bioguide_id, district) %>%
  arrange(date) %>%
  mutate(
    communication_window_1_newsletter = rollapplyr(sum_newsletters, width = 2, sum, fill = NA, align = "left"),  
    communication_window_7_newsletter = rollapplyr(sum_newsletters, width = 8, sum, fill = NA, align = "left"),
    communication_window_14_newsletter = rollapplyr(sum_newsletters, width = 15, sum, fill = NA, align = "left"),
    communication_window_21_newsletter = rollapplyr(sum_newsletters, width = 22, sum, fill = NA, align = "left"),
    communication_window_30_newsletter = rollapplyr(sum_newsletters, width = 31, sum, fill = NA, align = "left")
  )

# For statement kommunikation
data <- data %>%
  group_by(bioguide_id, district) %>%
  arrange(date) %>%
  mutate(
    communication_window_1_statements = rollapplyr(sum_source_statements, width = 2, sum, fill = NA, align = "left"),  
    communication_window_7_statements = rollapplyr(sum_source_statements, width = 8, sum, fill = NA, align = "left"),
    communication_window_14_statements = rollapplyr(sum_source_statements, width = 15, sum, fill = NA, align = "left"),
    communication_window_21_statements = rollapplyr(sum_source_statements, width = 22, sum, fill = NA, align = "left"),
    communication_window_30_statements = rollapplyr(sum_source_statements, width = 31, sum, fill = NA, align = "left")
  )

# Demonstrationsvariablen -------------------------------------------------

# Create the independent variable: Sum of protests from day t to t+x
data <- data %>%
  group_by(bioguide_id, district) %>%
  arrange(date) %>%
  mutate(
    protest_window_1 = rollapplyr(count_protest, width = 2, sum, fill = NA, align = "left"),  
    protest_window_7 = rollapplyr(count_protest, width = 8, sum, fill = NA, align = "left"),
    protest_window_14 = rollapplyr(count_protest, width = 15, sum, fill = NA, align = "left"),
    protest_window_21 = rollapplyr(count_protest, width = 22, sum, fill = NA, align = "left"),
    protest_window_30 = rollapplyr(count_protest, width = 31, sum, fill = NA, align = "left")
  )

# Create the independent variable: Sum of protests from day t+1 to t+x (excluding the current day)
data <- data %>%
  group_by(bioguide_id, district) %>%
  arrange(date) %>%
  mutate(
    protest_window_1_after = rollapplyr(count_protest, width = 2, sum, fill = NA, align = "left") - count_protest,  
    protest_window_7_after = rollapplyr(count_protest, width = 8, sum, fill = NA, align = "left") - count_protest,
    protest_window_14_after = rollapplyr(count_protest, width = 15, sum, fill = NA, align = "left") - count_protest,
    protest_window_21_after = rollapplyr(count_protest, width = 22, sum, fill = NA, align = "left") - count_protest,
    protest_window_30_after = rollapplyr(count_protest, width = 31, sum, fill = NA, align = "left") - count_protest
  )

# Create the indepent variable for palestine protests from day t to t+x
data <- data %>%
  group_by(bioguide_id, district) %>%
  arrange(date) %>%
  mutate(
    palestine_protest_window_1 = rollapplyr(count_palestine_protests, width = 2, sum, fill = NA, align = "left"),  
    palestine_protest_window_7 = rollapplyr(count_palestine_protests, width = 8, sum, fill = NA, align = "left"),
    palestine_protest_window_14 = rollapplyr(count_palestine_protests, width = 15, sum, fill = NA, align = "left"),
    palestine_protest_window_21 = rollapplyr(count_palestine_protests, width = 22, sum, fill = NA, align = "left"),
    palestine_protest_window_30 = rollapplyr(count_palestine_protests, width = 31, sum, fill = NA, align = "left")
  )

# Create the independent variable for Palestine protests excluding the current day
data <- data %>%
  group_by(bioguide_id, district) %>%
  arrange(date) %>%
  mutate(
    # Exclude the current day and include the next X days
    palestine_protest_window_1_after = rollapplyr(count_palestine_protests, width = 2, sum, fill = NA, align = "left") - count_palestine_protests,
    palestine_protest_window_7_after = rollapplyr(count_palestine_protests, width = 8, sum, fill = NA, align = "left") - count_palestine_protests,
    palestine_protest_window_14_after = rollapplyr(count_palestine_protests, width = 15, sum, fill = NA, align = "left") - count_palestine_protests,
    palestine_protest_window_21_after = rollapplyr(count_palestine_protests, width = 22, sum, fill = NA, align = "left") - count_palestine_protests,
    palestine_protest_window_30_after = rollapplyr(count_palestine_protests, width = 31, sum, fill = NA, align = "left") - count_palestine_protests
  )

# Create the independent variable for israel protests from day t to t+x
data <- data %>%
  group_by(bioguide_id, district) %>%
  arrange(date) %>%
  mutate(
    israel_protest_window_1 = rollapplyr(count_israel_protests, width = 2, sum, fill = NA, align = "left"),  
    israel_protest_window_7 = rollapplyr(count_israel_protests, width = 8, sum, fill = NA, align = "left"),
    israel_protest_window_14 = rollapplyr(count_israel_protests, width = 15, sum, fill = NA, align = "left"),
    israel_protest_window_21 = rollapplyr(count_israel_protests, width = 22, sum, fill = NA, align = "left"),
    israel_protest_window_30 = rollapplyr(count_israel_protests, width = 31, sum, fill = NA, align = "left")
  )

# Create the independent variable for israel protests from day t+1 to t+x (excluding the current day)
data <- data %>%
  group_by(bioguide_id, district) %>%
  arrange(date) %>%
  mutate(
    israel_protest_window_1_after = rollapplyr(count_israel_protests, width = 2, sum, fill = NA, align = "left") - count_israel_protests,  
    israel_protest_window_7_after = rollapplyr(count_israel_protests, width = 8, sum, fill = NA, align = "left") - count_israel_protests,
    israel_protest_window_14_after = rollapplyr(count_israel_protests, width = 15, sum, fill = NA, align = "left") - count_israel_protests,
    israel_protest_window_21_after = rollapplyr(count_israel_protests, width = 22, sum, fill = NA, align = "left") - count_israel_protests,
    israel_protest_window_30_after = rollapplyr(count_israel_protests, width = 31, sum, fill = NA, align = "left") - count_israel_protests
  )


# Uafhængig variabel - demonstrationsintensitet ---------------------------

# Create variabel for protest intensity
data <- data %>%
  mutate(demonstration_intensity_palestine = log1p(size_palestine_protests),
         demonstration_intensity_israel = log1p(size_israel_protests),
         demonstration_intensity_total = log1p(count_protest_size))

# Create the independent variable: Sum of protests from day t to t+x (Palestine)
data <- data %>%
  group_by(bioguide_id, district) %>%
  arrange(date) %>%
  mutate(
    demonstration_intensity_palestine_1 = rollapplyr(demonstration_intensity_palestine, width = 2, sum, fill = NA, align = "left"),  
    demonstration_intensity_palestine_7 = rollapplyr(demonstration_intensity_palestine, width = 8, sum, fill = NA, align = "left"),
    demonstration_intensity_palestine_14 = rollapplyr(demonstration_intensity_palestine, width = 15, sum, fill = NA, align = "left"),
    demonstration_intensity_palestine_21 = rollapplyr(demonstration_intensity_palestine, width = 22, sum, fill = NA, align = "left"),
    demonstration_intensity_palestine_30 = rollapplyr(demonstration_intensity_palestine, width = 31, sum, fill = NA, align = "left")
  )

# Create the independent variable: Sum of protests from day t+1 to t+x (Palestine)
data <- data %>%
  group_by(bioguide_id, district) %>%
  arrange(date) %>%
  mutate(
    demonstration_intensity_palestine_1_after = rollapplyr(demonstration_intensity_palestine, width = 2, sum, fill = NA, align = "left") - demonstration_intensity_palestine,  
    demonstration_intensity_palestine_7_after = rollapplyr(demonstration_intensity_palestine, width = 8, sum, fill = NA, align = "left") - demonstration_intensity_palestine,
    demonstration_intensity_palestine_14_after = rollapplyr(demonstration_intensity_palestine, width = 15, sum, fill = NA, align = "left") - demonstration_intensity_palestine,
    demonstration_intensity_palestine_21_after = rollapplyr(demonstration_intensity_palestine, width = 22, sum, fill = NA, align = "left") - demonstration_intensity_palestine,
    demonstration_intensity_palestine_30_after = rollapplyr(demonstration_intensity_palestine, width = 31, sum, fill = NA, align = "left") - demonstration_intensity_palestine
  )

# Create the independent variable: Sum of protests from day t to t+x (Israel)
data <- data %>%
  group_by(bioguide_id, district) %>%
  arrange(date) %>%
  mutate(
    demonstration_intensity_israel_1 = rollapplyr(demonstration_intensity_israel, width = 2, sum, fill = NA, align = "left"),  
    demonstration_intensity_israel_7 = rollapplyr(demonstration_intensity_israel, width = 8, sum, fill = NA, align = "left"),
    demonstration_intensity_israel_14 = rollapplyr(demonstration_intensity_israel, width = 15, sum, fill = NA, align = "left"),
    demonstration_intensity_israel_21 = rollapplyr(demonstration_intensity_israel, width = 22, sum, fill = NA, align = "left"),
    demonstration_intensity_israel_30 = rollapplyr(demonstration_intensity_israel, width = 31, sum, fill = NA, align = "left")
  )

# Create the independent variable: Sum of protests from day t+1 to t+x (Israel)
data <- data %>%
  group_by(bioguide_id, district) %>%
  arrange(date) %>%
  mutate(
    demonstration_intensity_israel_1_after = rollapplyr(demonstration_intensity_israel, width = 2, sum, fill = NA, align = "left") - demonstration_intensity_israel,  
    demonstration_intensity_israel_7_after = rollapplyr(demonstration_intensity_israel, width = 8, sum, fill = NA, align = "left") - demonstration_intensity_israel,
    demonstration_intensity_israel_14_after = rollapplyr(demonstration_intensity_israel, width = 15, sum, fill = NA, align = "left") - demonstration_intensity_israel,
    demonstration_intensity_israel_21_after = rollapplyr(demonstration_intensity_israel, width = 22, sum, fill = NA, align = "left") - demonstration_intensity_israel,
    demonstration_intensity_israel_30_after = rollapplyr(demonstration_intensity_israel, width = 31, sum, fill = NA, align = "left") - demonstration_intensity_israel
  )

# Create the independent variable: Sum of protests from day t to t+x (total)
data <- data %>%
  group_by(bioguide_id, district) %>%
  arrange(date) %>%
  mutate(
    demonstration_intensity_total_1 = rollapplyr(demonstration_intensity_total, width = 2, sum, fill = NA, align = "left"),  
    demonstration_intensity_total_7 = rollapplyr(demonstration_intensity_total, width = 8, sum, fill = NA, align = "left"),
    demonstration_intensity_total_14 = rollapplyr(demonstration_intensity_total, width = 15, sum, fill = NA, align = "left"),
    demonstration_intensity_total_21 = rollapplyr(demonstration_intensity_total, width = 22, sum, fill = NA, align = "left"),
    demonstration_intensity_total_30 = rollapplyr(demonstration_intensity_total, width = 31, sum, fill = NA, align = "left")
  )

# Create the independent variable: Sum of protests from day t+1 to t+x (total)
data <- data %>%
  group_by(bioguide_id, district) %>%
  arrange(date) %>%
  mutate(
    demonstration_intensity_total_1_after = rollapplyr(demonstration_intensity_total, width = 2, sum, fill = NA, align = "left") - demonstration_intensity_total,  
    demonstration_intensity_total_7_after = rollapplyr(demonstration_intensity_total, width = 8, sum, fill = NA, align = "left") - demonstration_intensity_total,
    demonstration_intensity_total_14_after = rollapplyr(demonstration_intensity_total, width = 15, sum, fill = NA, align = "left") - demonstration_intensity_total,
    demonstration_intensity_total_21_after = rollapplyr(demonstration_intensity_total, width = 22, sum, fill = NA, align = "left") - demonstration_intensity_total,
    demonstration_intensity_total_30_after = rollapplyr(demonstration_intensity_total, width = 31, sum, fill = NA, align = "left") - demonstration_intensity_total
  )

# Kampdistrikt-variabel ---------------------------------------------------
# Starter med at merge data sammen
comp_races <- comp_races %>% 
  mutate(comp = 1)

data <- data %>%
  left_join(comp_races, by = c("district" = "district", "state" = "state"), keep = TRUE)

data <- data %>% 
  mutate(comp = ifelse(is.na(comp), 0, data$comp),
         district = district.x)


# Ideologi-variabel -------------------------------------------------------
# Create the empirical cumulative distribution function for `ideology`
ecdf_func <- ecdf(data$ideology)

# Apply the ECDF function to calculate percentiles
data$ideology_percentile_rep<- ecdf_func(data$ideology)

# Plotter den nye ideologi-variabel
ggplot(data = data, aes(x = ideology_percentile_rep)) +
  geom_point(stat = "count")


# Ideologi opdelinger -----------------------------------------------------
data <- data %>% 
  mutate(main_ideology = ifelse(ideology_percentile_rep <= 0.50, 0, 1),
         conservative_25 = ifelse(ideology_percentile_rep >= 0.75, 1, 0),
         conservative_15 = ifelse(ideology_percentile_rep >= 0.85, 1, 0),
         liberal_25 = ifelse(ideology_percentile_rep <= 0.25, 1, 0),
         liberal_15 = ifelse(ideology_percentile_rep <= 0.15, 1, 0))


# Begrænser perioden ------------------------------------------------------
data <- data %>% 
  filter(date >= "2023-09-01")


# Gemmer data -------------------------------------------------------------
write.csv(data, "/Users/carlgammeltoft/Lokale dokumenter/R_speciale/3_finalizing/3_output/data_finalized.csv")
write.csv(data, "/Users/carlgammeltoft/Lokale dokumenter/R_speciale/4_modelling/1_input/data_finalized.csv")
