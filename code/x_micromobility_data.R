###################################################################################################
###    The purpose of this script is to process data received from micromobility providers. The ###
###    data is projected onto a different hexagon layer than the one we are using for our       ###
###    analysis. We need to do some initial spatial operations to match the two layers          ###
###################################################################################################

library(tidyverse)
library(sf)
library(lubridate)

# ----------------------------------- Read in the data ----------------------------------- #

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
joined <- hexagon_layer %>% 
  st_join(census %>% select(cell_id), join = st_intersects, largest = TRUE)  %>% 
  st_drop_geometry()

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

# --- average over all weekdays for the same hexagon
supply_layer_hex_avg <- supply_layer_hex %>% 
  group_by(hex_id) %>%
  summarise(frac_available = mean(frac_available)) %>% 
  ungroup()

# ------ 2. Join results onto census sf

# --- use matching table to add cell_id column to micromobility results
supply_layer_hex_avg <- joined %>%
  left_join(supply_layer_hex_avg, by = c("id" = "hex_id")) 

# --- we might have multiple hexagons join onto one cell_id. We need each cell_id to be available once only

supply_layer_hex_avg <- supply_layer_hex_avg %>% 
  # replace na values with 0 to indicate no availability
  mutate(frac_available = replace_na(frac_available, 0)) %>%
  group_by(cell_id) %>%
  # get max value per cell_id, not average
  summarise(frac_available = round(max(frac_available, na.rm = TRUE), 1)) %>%
  ungroup()

# --- project from current layer to layer being used for analysis 
census <- census %>% left_join(supply_layer_hex_avg, by = "cell_id") %>%
  # replace na values with 0 to indicate no availability
  mutate(frac_available = replace_na(frac_available, 0))

plot(census["frac_available"])

# --- save
st_write(census, paste0("../data/", city, "/level_i_ii/zones_w_micromobility_providers.geojson"), delete_dsn = TRUE)


