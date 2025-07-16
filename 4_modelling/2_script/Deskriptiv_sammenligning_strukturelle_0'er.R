### Deskriptiv sammenligning strukturelle 0'er


# Indlæser data og libraries ----------------------------------------------
library(tidyverse)
library(cowplot)
library(gridExtra)

# Indlæser data -----------------------------------------------------------
setwd("/Users/carlgammeltoft/Lokale dokumenter/R_speciale/4_modelling/")
data <- read.csv("1_input/data_finalized.csv")

data <- data %>% 
  filter(date >= "2023-09-01")


# Databehandling ----------------------------------------------------------

# Finder de strukturelle 0'er
data_summarized <- data %>% 
  group_by(bioguide_id) %>% 
  summarise(antal_statements = sum(count_statement)) %>% 
  mutate(strukturelt0 = ifelse(antal_statements == 0, 1, 0))

# Sammensætter dem med den store datafil
data <- left_join(data_summarized, data, by = c("bioguide_id" = "bioguide_id"))

# Finder antallet af politikere i hver gruppe
zero <- data %>% group_by(bioguide_id) %>% filter(strukturelt0.x == 1) %>% summarize(sum()) %>% summarize(n()) %>% pull()
more <- data %>% group_by(bioguide_id) %>% filter(strukturelt0.x == 0) %>% summarize(sum()) %>% summarize(n()) %>% pull()

# Finder gennemsnitligt antal demonstrationer for de to grupper
data_groups <- data %>% 
  group_by(strukturelt0.x) %>% 
  summarise(antal_demoer = sum(count_protest),
            antal_deltagere = sum(count_protest_size)) %>% 
  mutate(antal_politikere = c(363, 54)) %>% 
  mutate(demoer_pr_pol = antal_demoer / antal_politikere,
         deltagere_pr_pol = antal_deltagere / antal_politikere)

## SE DATA


