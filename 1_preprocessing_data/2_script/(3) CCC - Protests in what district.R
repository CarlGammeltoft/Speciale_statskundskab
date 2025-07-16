# CCC - Protests in what district


# Indlæser data og libraries --------------------------------------------------------
# Indlæser libraries

library(tidyverse)
library(sf)

# Indlæser data 
setwd("/Users/carlgammeltoft/Lokale dokumenter/R_speciale/1_preprocessing_data")

# Data
CCC <- read.csv("1_input/CCC_classified.csv")
district_data <- dget("1_input/district_data.txt")


# Formål ------------------------------------------------------------------

# Formålet med dette script er at undersøge i hvilke distrikter demonstrationerne
# fandt sted. Jeg har "district_data", som viser politikerne og deres distrikter
# (i koordinater).

# Og jeg har CCC_classified, som jo er demonstrationsdata - med koordinator for 
# hver demonstration.

# Udfordringen er at få merged de to datasæt, så der for hver demonstration i CCC-data
# Kan findes en tilhørende politiker. I første omgang skal jeg dog bare finde distriktet,
# hvilket det her script skal hjælpe til.

# Det er en meget tung opgave, da jeg skal have fundet matchende koordinator for
# hver demonstration. Min ide er, at jeg jo i hvert fald kender staten for 
# hver demonstration og derfor forhåbentlig kan snævre det lidt ind.


# Behandler data ----------------------------------------------------------

# Starter med at udvælge relevante kolonner for hver df:
district_data_sel <- district_data %>% 
  select(bioguide_id, state, district, geometry)

# Indekserer først CCC_data, så jeg kan merge tilbage, når jeg har resultaterne
CCC <- CCC %>% 
  mutate(id = 1:31334)

# Udvælger relevante kolonner for CCC data samtidig med, at jeg filtrerer, så jeg
# kun ser palæstinademonstrationerne
CCC_palestine_sel <- CCC %>% 
  filter(total_protest == 1) %>% 
  select(total_protest, id, date, state, lat, lon, size_mean, non_normative)


# Behandler data ----------------------------------------------------------

# Step 1: Filter out rows with missing lat/lon values - in protest data
CCC_palestine_sel_clean <- CCC_palestine_sel %>% 
  filter(!is.na(lat) & !is.na(lon))
# der var åbenbart én observation/demonstration, der manglede koordinater.

# Step 2: Convert CCC protest coordinates into sf points
CCC_palestine_sel_sf <- CCC_palestine_sel_clean %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)  # Assuming lat/lon uses WGS84 (EPSG:4326)

# Step 3: Ensure the districts data is an sf object
districts_sf <- st_as_sf(district_data_sel, crs = 4326, sf_column_name = "geometry")

# Step 4: Continue with the spatial join as before
protest_districts <- st_join(CCC_palestine_sel_sf, districts_sf, join = st_within)

# Step 5: Count the number of times the district column is NA grouped by state.x
na_district_by_state <- protest_districts %>%
  filter(is.na(district)) %>%  # Filter rows where district is NA
  group_by(state.x) %>%        # Group by the state.x column
  tally(name = "na_count")     # Count the number of such rows for each state.x

# View the result
print(na_district_by_state)

# 854 demonstrationer kunne ikke findes i ét distrikt...
# Af dem 528 i DC, som ikke er en stat og derfor ikke har medlemmer i kongressen.
# Af dem 184 i TX, som jo har et ledigt sæde i 18 district.
# NJ 32 - har to ledige sæder.
# WI 13 - har også et ledigt sæde...

# Hvor mange blev succesfuldt assignet?
has_district_by_state <- protest_districts %>%
  filter(!(is.na(district)))


# Merger tilbage på CCC ---------------------------------------------------
# Udvælger relevante kolonner fra protest_districts

protest_districts_sel <- protest_districts %>% 
  select(id, bioguide_id, district)

# Merger tilbage på CCC
data <- merge(CCC, protest_districts_sel, by = "id", all = T)

# Gemmer data -------------------------------------------------------------

dput(data, "3_output/CCC_districts_assigned.txt")
dput(data, "/Users/carlgammeltoft/Lokale dokumenter/R_speciale/2_joining_data/1_input/CCC_districts_assigned.txt")

# prøver lige at loade igen for at tjekke om det kommer ud rigtigt
data_2 <- dget("/Users/carlgammeltoft/Lokale dokumenter/R_speciale/2_joining_data/1_input/CCC_districts_assigned.txt")

# Ekstra - plotter protester ----------------------------------------------

# Plot the protest points
ggplot(data = protest_districts) +
  geom_sf(aes(geometry = geometry), color = "blue", size = 1) +
  labs(title = "Map of Palestine Protests",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

