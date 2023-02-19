#
# Comparing NYC Covid-19 vaccine coverage by Modified ZIP Code Tabulation Areas (MODZCTAs)
# Coverage levels:  a. Completed primary series | b.  At least one dose | c. Bivalent dose
# All ages included
# As of: 02/19/2023
# Myrela Bauman
#

# https://www.nyc.gov/site/doh/covid/covid-19-data-vaccines.page

library(dplyr)
library(ggplot2)
library(sf)

# Load the NYC shape file
nyc_shapefile <- st_read("~/Desktop/NYC_shapefiles_zipcode/ZIP_CODE_nyc.shp")

#Reading NYC DOHMH data: covid vaccine coverage
vaccine_data <- read.csv("https://github.com/nychealth/covid-vaccine-data/raw/main/people/coverage-by-modzcta-allages.csv")

########
######## New Yorkers all ages who completed primary series by MODZCTA
########

# Keep relevant columns
vaccine_data1 <- vaccine_data %>% 
  select(MODZCTA, PERC_FULLY)

# Rename columns
colnames(vaccine_data1) <- c("modzcta", "proportion")

# Join data from shapefile and dataset
nyc_shapefile$ZIPCODE <- as.integer(nyc_shapefile$ZIPCODE)

nyc_map_vaccine_data1 <- nyc_shapefile %>%
  left_join(vaccine_data1, by = c("ZIPCODE" = "modzcta"))

# Capping at 99% because dataset includes multiple doses per person
nyc_map_vaccine_data_99_1 <- nyc_map_vaccine_data1 %>% 
  mutate(proportion = ifelse(proportion > 99, 99, proportion))


# Look up data for zips that are greyed out: 11434
#zip_11434 <- nyc_map_vaccine_data_99_1 %>%
#filter(ZIPCODE == 11434)

# filtering proportions that are missing
nyc_map_vaccine_data_99_1 <- nyc_map_vaccine_data_99_1 %>%
filter(!is.na(proportion))

# Plot the map

mynamestheme <- theme(
  plot.title = element_text(family = "Calibri", colour = "#AEBAC5", face = "bold", size = (18)),
  legend.text = element_text(face = "bold", colour = "#AEBAC5", family = "Calibri"),
  legend.title = element_text(colour = "#AEBAC5", face = "bold", family = "Calibri"))

map1 <- ggplot() +
  geom_sf(data = nyc_map_vaccine_data_99_1, aes(fill = proportion), color = "black") + 
  ggtitle("Percent of NYC residents who completed\nprimary series of COVID-19 vaccine (2023)") +
  scale_fill_gradientn(colors = c("#B0C4DE", "#000080"), name = "% vaccinated") +
  theme_void() + mynamestheme

print(map1 + mynamestheme + labs(
  title = "Percent of NYC residents who completed\nprimary series of COVID-19 vaccine (2023)"))

ggsave('map1.png', map1, width=8, height=8, dpi=200)
dev.off()

######## 
######## New Yorkers all ages who are partially vax by MODZCTA
######## 

# Keep relevant columns
vaccine_data2 <- vaccine_data %>% 
  select(MODZCTA, PERC_1PLUS)

# Rename columns
colnames(vaccine_data2) <- c("modzcta", "proportion")

# Join data from shapefile and dataset
nyc_shapefile$ZIPCODE <- as.integer(nyc_shapefile$ZIPCODE)

nyc_map_vaccine_data2 <- nyc_shapefile %>%
  left_join(vaccine_data2, by = c("ZIPCODE" = "modzcta"))

# Capping at 99% because dataset includes multiple doses per person
nyc_map_vaccine_data_99_2 <- nyc_map_vaccine_data2 %>% 
  mutate(proportion = ifelse(proportion > 99, 99, proportion))

# filtering proportions that are missing
nyc_map_vaccine_data_99_2 <- nyc_map_vaccine_data_99_2 %>%
  filter(!is.na(proportion))

# Plot the map
map2 <- ggplot() +
  geom_sf(data = nyc_map_vaccine_data_99_2, aes(fill = proportion), color = "black") + 
  ggtitle("Percent of NYC residents who received\nat least one dose of COVID-19 vaccine (2023)") +
  scale_fill_gradientn(colors = c("#B0C4DE", "#000080"), name = "% vaccinated") +
  theme_void() + mynamestheme

print(map2 + mynamestheme + labs(
  title = "Percent of NYC residents who received\nat least one dose of COVID-19 vaccine (2023)"))

ggsave('map2.png', map2, width=8, height=8, dpi=200)
dev.off()

########
######## New Yorkers all ages bivalent dose by MODZCTA
########

# Keep relevant columns
vaccine_data3 <- vaccine_data %>% 
  select(MODZCTA, PERC_BIVALENT_ADDITIONAL)

# Rename columns
colnames(vaccine_data3) <- c("modzcta", "proportion")

# Join data from shapefile and dataset
nyc_shapefile$ZIPCODE <- as.integer(nyc_shapefile$ZIPCODE)

nyc_map_vaccine_data3 <- nyc_shapefile %>%
  left_join(vaccine_data3, by = c("ZIPCODE" = "modzcta"))

# Capping at 99% because dataset includes multiple doses per person
nyc_map_vaccine_data_99_3 <- nyc_map_vaccine_data3 %>% 
  mutate(proportion = ifelse(proportion > 99, 99, proportion))

# filtering proportions that are missing
nyc_map_vaccine_data_99_3 <- nyc_map_vaccine_data_99_3 %>%
  filter(!is.na(proportion))

# Plot the map
map3 <- ggplot() +
  geom_sf(data = nyc_map_vaccine_data_99_3, aes(fill = proportion), color = "black") + 
  ggtitle("Percent of NYC residents who received\na bivalent dose of COVID-19 vaccine (2023)") +
  scale_fill_gradientn(colors = c("#B0C4DE", "#000080"), name = "% vaccinated") +
  theme_void() + mynamestheme

print(map3 + mynamestheme + labs(
  title = "Percent of NYC residents who received\na bivalent dose of COVID-19 vaccine (2023)"))

ggsave('map3.png', map3, width=8, height=8, dpi=200)
dev.off()









