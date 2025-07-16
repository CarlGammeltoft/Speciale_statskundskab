## Preparing data

# Sammenligner data -------------------------------------------------------
## Indlæser data
Classified <- read.csv("/Users/carlgammeltoft/Lokale dokumenter/R_speciale/5_mapping/1_input/CCC_classified.csv")
ccc <- read.csv("/Users/carlgammeltoft/Lokale dokumenter/R_speciale/5_mapping/1_input/ccc_compiled_2021-present.csv")

# Indstiller den rette dato for classified
Classified_filtered <- Classified %>% 
  filter(date >= "2023-09-01")

# Tæller demoer
Sum_demo <- Classified_filtered %>% 
  dplyr::select(palestine_protest, israel_protest, neutral_protest) %>% 
  summarise(antal_pal = sum(palestine_protest),
            antal_isr = sum(israel_protest),
            antal_neu = sum(neutral_protest))

# Gør den kompatibel med resten af koden
Classified_filtered <- Classified_filtered %>%
  mutate(fips_code = as.character(fips_code))  # Convert fips_code to character

# Filtrerer så det kun er relevante demonstrationer med i data
Classified_filtered <- Classified_filtered %>% 
  filter(palestine_protest == 1 | israel_protest == 1 | neutral_protest == 1)

# Gemmer data i ny fil
write.csv(Classified_filtered, "/Users/carlgammeltoft/Lokale dokumenter/R_speciale/5_mapping/1_input/CCC_classified_all.csv")
