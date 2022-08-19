# The purpose of this script is demonstrate some introductory tidyverse skills.

# Prior to running this script, you should start a new RStudio project. 

# Download the following from from Canvas (Pre-Term Workshop > R exercise):
#    # This script (example-script.R)
#    # FY2022_cambridge_asssessor.csv
#    # cambridge_parcels.geojson

# Save all three files to the project directory associated with the RStudio 
# project you just created. Open the RStudio project (not just the script).

# Load required packages
library(tidyverse)
library(sf)
library(here)

# Load parcel data from the csv file
parcel_data <- here("FY2022_cambridge_assessor.csv") %>%
  read_csv()

# load parcel boundaries
parcel_boundaries <- here("cambridge_parcels.geojson") %>%
  st_read()

# Join parcel data with parcel boundaries
parcel_boundaries_data <- parcel_boundaries %>%
  left_join(parcel_data)

# Reduce the number of columns by selecting the ones you want to keep
owners_values <- parcel_boundaries_data %>%
  select(gisid, landarea, buildingvalue, landvalue, owner_name)

# Filter to include only the properties owned by Harvard
Harvard_parcels <- owners_values %>%
  filter(owner_name == "PRESIDENT & FELLOWS OF HARVARD COLLEGE")

# Filter again to remove duplicates
Harvard_unique <- Harvard_parcels %>%
  filter(!duplicated(.))

# Calculate a new variable 
Harvard_pct_value <- Harvard_unique %>%
  mutate(pct_bldg = buildingvalue / (buildingvalue + landvalue))

# We can actually do all of the above in one step
# (this saves us having to keep all these different versions of the tibble)
final_data <- parcel_boundaries %>%
  left_join(parcel_data) %>%
  select(gisid, landarea, buildingvalue, landvalue, owner_name) %>%
  filter(owner_name == "PRESIDENT & FELLOWS OF HARVARD COLLEGE") %>%
  filter(!duplicated(.)) %>%
  mutate(pct_bldg = buildingvalue / (buildingvalue + landvalue))
  
# Use ggplot to create a scatterplot
scatter <- ggplot(final_data) +
  geom_point(aes(x = landarea, y = landvalue)) 

scatter

# I'm trying to keep this all super simple, but I can't help myself
# These data are begging to be log-transformed

scatter_log <- scatter +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log")

scatter_log

# Or a map
map <- ggplot(final_data) +
  geom_sf(aes(fill = landvalue)) +
  scale_fill_continuous(trans = "log")

map

# save your dataset as a csv
write_csv(final_data, "harvard_parcels.csv")

# or as a geojson
st_write(final_data, "harvard_parcels.geojson")

# And you can also save your plots
ggsave("scatter.pdf", plot = scatter_log)

ggsave("map.png", plot = map)
