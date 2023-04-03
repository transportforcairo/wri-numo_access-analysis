###################################################################################################
###    The purpose of this script is to process data received from micromobility providers. The ###
###    data is projected onto a different hexagon layer than the one we are using for our       ###
###    analysis. We need to do some initial spatial operations to match the two layers          ###
###################################################################################################

library(tidyverse)
library(sf)
library(lubridate)

# ----------------------------------- Read in the data ----------------------------------- #

# reproject spin data onto correct hexagon layer 

city <- "San Francisco"
# ----- Hexagon layer SENT TO micromobility provider. We will join it to the csv they sent back
hexagon_layer <- st_read(paste0("../data_raw/", city, "/sf_hex_grid_400.geojson"))

# ----- Data RECEIVED BY micromobility providers - Spin San Francisco 
city <- "San Francisco"

supply_layer <- read_csv(paste0("../data_raw/", city, "/GBFS/census_scooter_counts.csv"))

supply_layer_hex <- read_csv(paste0("../data_raw/", city, "/GBFS/hexgrid_scooter_counts.csv"))

# ----- Census data projected onto hexagons
census_layer <- st_read(paste0("../data/", city, "/level_i_ii/zones_w_micromobility_providers.geojson"))


# ------ 1. Match CENSUS spatial layer (census_layer) with MICROMOBILITY spatial layer (hexagon_layer)

# We want the data to be projected onto the census hexagons in the end. In this step, we create a column that 
# matches hexagons from the two layers together. We will use this column for a spatial join later

# --- filter hexagon layer to san francisco boundary
hexagon_layer <- hexagon_layer %>% st_filter(census_layer, .predicate = st_intersects)

# --- matching table. This matches the census zones to the zones sent to the micromobility provider
# joined <- hexagon_layer %>% 
#   st_join(census_layer %>% select(cell_id), join = st_intersects, largest = TRUE)  %>% 
#   st_drop_geometry()
# 
# write_csv(joined, paste0("../data_raw/", city, "/GBFS/joining_backup.csv"))
joined <- read_csv(paste0("../data_raw/", city, "/GBFS/joining_backup.csv"))

# --- add hexagon ids to micromobility data (replace ids from sf_hex_grid_400 with ids from zones_w_micromobility_providers)
supply_layer_hex <- supply_layer_hex %>% 
  left_join(joined, 
            by = c("hex_id" = "id")) %>% 
  rename(zone_id = cell_id) %>%
  select(-hex_id)


write_csv(supply_layer_hex, paste0("../data_raw/", city, "/GBFS/spin_scooter_counts_cleaned.csv"))



# ----------------------------------- Merge data from SPIN and BIRD ----------------------------------- #

data_bird <- vroom::vroom(paste0("../data_raw/", city, "/GBFS/SF_MDS_cleaned_hexgrid_scooter_counts.zip"))
data_spin <-  vroom::vroom(paste0("../data_raw/", city, "/GBFS/spin_scooter_counts_cleaned.csv"))

# ----- SPIN 

# --- create separate date and time columns
data_spin_edited <- data_spin %>% 
  mutate(date = lubridate::as_date(minute_local),
         time = hms::as_hms(minute_local),
         weekday = lubridate::wday(minute_local, label = TRUE))

# add group id
data_spin_edited <- data_spin_edited %>%
  group_by(date) %>% mutate(group_id = cur_group_id()) %>%
  ungroup()

# ----- BIRD

# --- create separate date and time columns
data_bird_edited <- data_bird %>% 
  mutate(date = lubridate::as_date(minute_local),
         time = hms::as_hms(minute_local),
         weekday = lubridate::wday(minute_local, label = TRUE)) 

# we want both datasets to start on the same day of the week. We will join by groups based on date, and 
# we want the dates to be aligned (Sunday = Sunday...)

data_bird_edited <- data_bird_edited %>% 
  filter(date >= lubridate::as_date("2021-05-23"))

# add group id
data_bird_edited <- data_bird_edited %>%
  group_by(date) %>% mutate(group_id = cur_group_id()) %>%
  ungroup()

# ------ COMBINE DATASETS

# we want the same number of days from both datasets. 
# Spin gave us 2 weeks of data so we will remove the excess data from Bird
data_bird_edited <- data_bird_edited %>% 
  filter(group_id <= max(data_spin_edited$group_id))

# merge
data_providers <- data_spin_edited %>% 
  bind_rows(data_bird_edited)

write_csv(data_providers, paste0("../data_raw/", city, "/GBFS/SF_scooter_counts_all_providers_merged.csv"))




# ------ 2. Clean / summarize data sent by micromobility providers

# --- create separate date and time columns
supply_layer_hex <- supply_layer_hex %>% 
  mutate(date = lubridate::as_date(minute_local),
         time = hms::as_hms(minute_local),
         weekday = lubridate::wday(minute_local, label = TRUE))

# --- remove weekend days
supply_layer_hex <- supply_layer_hex %>% 
  filter(!(weekday %in% c("Sat", "Sun")))

# For each date and hexagon combination, we have the number of scooters in the hexagon at each minute during the period
# from 7:30am - 9:30am (2 hours = 120 minutes = 120 data points for each hexagon + date combination)
# We want to group by date and hexagon, and count the number of minutes during which there were no available vehicles
# We use a threshold of 3 (not zero) to determine if there are functioning vehicles or not. Explanation for this is in the report

# --- get bike availability
supply_layer_hex <- supply_layer_hex %>% 
  group_by(date, hex_id) %>%
  summarise(min_available = sum(rentable_scooter_count >= 3), # number of minutes where a bike is available
            min_unavailable = sum(rentable_scooter_count < 3), # number of minutes where a bike is unavailable
            # availability as a fraction of the total time period
            frac_available = round(min_available / (min_available+ min_unavailable), 1)) %>%
  ungroup()

# --- average over all weekdays for the same hexagon (needs review. Poisson distribution?)
supply_layer_hex_avg <- supply_layer_hex %>% 
  group_by(hex_id) %>%
  summarise(frac_available = mean(frac_available)) %>% 
  ungroup()

# ------ 3. Join results onto census sf

# --- use matching table to add cell_id column to micromobility results
supply_layer_hex_avg <- joined %>%
  left_join(supply_layer_hex_avg, by = c("id" = "hex_id")) 

# --- we might have multiple hexagons join onto one cell_id. We need each cell_id to be available once only

supply_layer_hex_avg <- supply_layer_hex_avg %>% 
  # replace na values with 0 to indicate no availability
  mutate(frac_available = replace_na(frac_available, 0)) %>%
  group_by(cell_id) %>%
  # get max value per cell_id, not average (needs review)
  summarise(frac_available = round(max(frac_available, na.rm = TRUE), 1)) %>%
  ungroup()

# --- project from current layer to layer being used for analysis 
census_layer <- census_layer %>% left_join(supply_layer_hex_avg, by = "cell_id") %>%
  # replace na values with 0 to indicate no availability
  mutate(frac_available = replace_na(frac_available, 0))

plot(census_layer["frac_available"])



# ------ 4. Get availability based on averaging results of neighboring zones

# intersect geometry with itself
layer_matrix <- census_layer %>% st_intersects(.)
# convert result from list to dataframe
layer_matrix_df <- layer_matrix %>% as.data.frame()
# join onto census layer which has info on 'frac_available' for each zone 
layer_matrix_meta <- layer_matrix_df %>% 
  left_join(census_layer %>% select(cell_id, frac_available), by = c("col.id" = "cell_id")) 
# get availability for each zone by averaging availability at neighboring zones (or should we get the max?)
layer_matrix_meta <- layer_matrix_meta %>% 
  #st_drop_geometry() %>%
  group_by(row.id) %>%
  summarise(frac_available_neighb = round(mean(frac_available), 1))

# join results onto the census layer
census_layer <- census_layer %>% left_join(layer_matrix_meta, by = c("cell_id" = "row.id"))

# plot to check
plot(census_layer["frac_available"])
plot(census_layer["frac_available_neighb"])



# --- save
st_write(census_layer, paste0("../data/", city, "/level_i_ii/zones_w_micromobility_providers.geojson"), delete_dsn = TRUE)

