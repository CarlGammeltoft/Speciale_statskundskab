### KODE FRA DORFF!
### Inspiration fra Dorff et al. (2023)

### Henter kortet
# Load the map object
map <- readRDS("/Users/carlgammeltoft/Lokale dokumenter/R_speciale/5_mapping/3_output/map_US.RDS")

# Display the map (optional)
print(map)

# Save the map as a PDF
ggsave(filename = "/Users/carlgammeltoft/Lokale dokumenter/R_speciale/5_mapping/3_output/map_US.pdf",
       plot = map,
       device = "pdf",
       width = 12,  # Specify the width in inches
       height = 8)  # Specify the height in inches

# Indlæser data og libraries ----------------------------------------------
library(tidyverse)
library(lubridate)
library(haven)
library(ggplot2)
library(kableExtra)
library(ggpubr)
library(patchwork)
library(viridis)
library(maps)
library(mapproj)
library(ggthemes)
library(ggmap)
library(usmap)
library(socviz)
library(tidycensus)
library(tidygeocoder)

# set user's data folder path
pathData <- "/Users/carlgammeltoft/Lokale dokumenter/R_speciale/5_mapping/1_input/"

ccc <- readr::read_csv(paste0(pathData,"CCC_classified_all.csv"),
                       col_types = cols(
                         .default = "c",
                         county = "c",
                         organizations = "c",
                         date = col_date(format = ""),
                         online = col_double(),
                         valence = col_double(),
                         size_low = col_double(),
                         size_high = col_double(),
                         size_mean = col_double(),
                         size_cat = col_double(),
                         arrests = col_double(),
                         arrests_any = col_double(),
                         injuries_crowd = col_double(),
                         injuries_crowd_any = col_double(),
                         injuries_police = col_double(),
                         injuries_police_any = col_double(),
                         property_damage = col_double(),
                         property_damage_any = col_double(),
                         chemical_agents = col_logical(),
                         participant_deaths = 'd',
                         police_deaths = 'd',
                         participant_measures = "c",
                         notes = "c",
                         participants = "c",
                         police_measures = "c",
                         title = "c",
                         final = col_double(),
                         lat = col_double(),
                         lon = col_double()
                       )
)

# map data
states <- readr::read_csv(paste0(pathData,"states.csv")) %>%
  janitor::clean_names() %>% 
  dplyr::select(state_full = state, everything())

# state level
us_states_poly <- map_data("state") %>% 
  rename(state = region) %>% 
  mutate(state = str_to_title(state)) %>% 
  left_join(., states, by = c("state" = "state_full"))

# get county level map data
county_map_fips <- county_map %>% rename(fips = id) %>%
  dplyr::select(fips, long, lat, group) %>% 
  mutate(fips = ifelse(nchar(fips) < 5, paste0("0", fips), fips))

# population by fips
fips_pop <- read_csv(paste0(pathData,"population_by_fips.csv")) %>% 
  mutate(fips_code = as.character(fips_code))


# Analyse -----------------------------------------------------------------

## coverage(CCC)
ccc %>%
  group_by(resolved_locality, resolved_county, resolved_state) %>% 
  summarize(n = n(), .groups = "keep") %>%
  nrow()

## median number of events per location (CCC)
ccc %>%
  group_by(resolved_locality, resolved_county, resolved_state) %>% 
  summarize(n = n(), .groups = "keep") %>% 
  pull(n) %>% 
  summary()

## proportion of locations with 3 events or less (CCC)
ccc %>%
  group_by(resolved_locality, resolved_county, resolved_state) %>% 
  summarize(n = n(), .groups = "keep") %>% 
  mutate(n_event = ifelse(n <= 3, 1, 0)) %>% 
  group_by(n_event) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n/sum(n))

# CCC: Use FIPS code directly
# number of events by FIPS
ccc_by_county <- ccc %>% 
  # filter(date >= ymd("2020-01-01"), date < ymd("2021-08-01")) %>% 
  mutate(
    fips_code = as.character(fips_code),  # Ensure fips_code is character
    fips_code = if_else(!is.na(fips_code) & nchar(fips_code) < 5, 
                        paste0("0", fips_code), 
                        fips_code)
  ) %>% 
  group_by(fips_code) %>% 
  summarize(num_events = n()) %>% 
  # Add population info
  left_join(., fips_pop, by = c("fips_code" = "fips_code")) %>% 
  # Calculate rate
  mutate(
    num_events = if_else(is.na(num_events), 0L, num_events),  # Replace NA counts with 0
    rate = (num_events / pop) * 1000000
  ) %>% 
  # Add geometry info
  right_join(., county_map_fips, by = c("fips_code" = "fips")) %>% 
  dplyr::select(fips_code, lat, long, group, pop, num_events, rate)


# plot
figure5 <- ccc_by_county %>% 
  mutate(rate = ifelse(is.na(rate), 0, rate)) %>% 
  mutate(rate_bckt = case_when(
    rate < 1 ~ "0",
    rate >= 1 & rate < 10 ~ "< 10",
    rate >= 10 & rate < 20 ~ "10-20",
    rate >= 20 & rate < 30 ~ "20-30",
    rate >= 30 & rate < 40 ~ "30-40",
    rate >= 40 & rate < 50 ~ "40-50",
    rate >= 50 & rate < 100 ~ "50-100",
    TRUE ~ "100+" )) %>% 
  mutate(rate_bckt = fct_relevel(rate_bckt, "0", "< 10", "10-20", "20-30", "30-40", "40-50", "50-100", "100+")) %>% 
  
  ggplot(., aes(x = long, y = lat, fill = rate_bckt, group = group)) +
  geom_polygon(color = "gray90", linewidth = 0.15) +
  coord_equal() +
  scale_fill_brewer(palette = "Greys") +
  labs(fill = "Antal demonstrationer pr. million indbyggere") +
  guides(fill = guide_legend(nrow = 1)) + 
  theme_map() +
  theme(legend.position = "bottom")

figure5


# Gemmer kortet -----------------------------------------------------------


saveRDS(figure5, "/Users/carlgammeltoft/Lokale dokumenter/R_speciale/5_mapping/3_output/map_US.RDS")
