library(tidyverse)
library(sf)
library(lubridate)


# --------- 1. read in the data 
# city <- "Minneapolis"
# supply_layer_hex <- vroom::vroom(paste0("../data_raw/", city, "/GBFS/Minneapolis_MDS_cleaned_hexgrid_scooter_counts.zip"))

city <- "San Francisco"
supply_layer_hex <- vroom::vroom( paste0("../data_raw/", city, "/GBFS/SF_scooter_counts_all_providers_merged.csv"))

# ----- Census data projected onto hexagons
census_layer <- st_read(paste0("../data/", city, "/level_i_ii/zones_w_micromobility_providers.geojson"))

# # remove to re-add them 
# census_layer <- census_layer %>%
#   select(-c(frac_available, frac_available_neighb))

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
if(city == "Minneapolis"){
  supply_layer_hex <- supply_layer_hex %>% 
    group_by(date, zone_id) %>%
    summarise(min_available = sum(rentable_scooter_count >= 2), # number of minutes where a bike is available
              min_unavailable = sum(rentable_scooter_count < 2), # number of minutes where a bike is unavailable
              # availability as a fraction of the total time period
              frac_available = round(min_available / (min_available+ min_unavailable), 1)) %>%
    ungroup()
  
} else if(city == "San Francisco"){
  supply_layer_hex <- supply_layer_hex %>% 
    group_by(group_id, zone_id) %>%
    summarise(min_available = sum(rentable_scooter_count >= 2), # number of minutes where a bike is available
              min_unavailable = sum(rentable_scooter_count < 2), # number of minutes where a bike is unavailable
              # availability as a fraction of the total time period
              frac_available = round(min_available / (min_available+ min_unavailable), 1)) %>%
    ungroup()
}

# --- average over all weekdays for the same hexagon (needs review. Poisson distribution?)
supply_layer_hex_avg <- supply_layer_hex %>% 
  group_by(zone_id) %>%
  summarise(frac_available = mean(frac_available)) %>% 
  ungroup()


# # --- project from current layer to layer being used for analysis 
census_layer <- census_layer %>% left_join(supply_layer_hex_avg, by = c("cell_id" = "zone_id")) %>%
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



#TEST - DELETE
x <- census_layer %>% filter(frac_available > 0)
# plot to check
plot(x["frac_available"])
plot(x["frac_available_neighb"])



