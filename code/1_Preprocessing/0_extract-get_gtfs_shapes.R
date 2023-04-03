###################################################################################################
###    The purpose of this script is to get trip geometries from GTFS feeds and save the output ###
###    for visualization purposes of                                                            ###
###################################################################################################

library(sf)
library(tidyverse)
library(gtfstools)

#city <- "Mexico City"
#city <- "San Francisco"
city <- "Minneapolis"
#city <- "Cairo"

# ---------------------------------------- Read in the data ---------------------------------------- #

# ----- 1. define relative path of folder that contains gtfs feeds
feed_dir <- paste0("../data_raw/", city, "/GTFS")

# ----- 2. get relative paths of all gtfs feeds
feeds <- dir(feed_dir, ".zip$", full.names = TRUE)

# ----- 3. read in the feeds
gtfs_feeds <- map(feeds, gtfstools::read_gtfs)

#gtfs_feeds <- gtfstools::read_gtfs(feeds[1])
# ---------------------------------------- Convert shapes to sf ---------------------------------------- #

# Function to extract geometry from feeds and convert to one sf if multiple feeds
convert_feeds_to_spatial <- function(gtfs_feeds){
  
  # empty list to store results for each combination
  results <- vector(mode = "list", length = length(gtfs_feeds))
  
  # loop over feeds: convert to sf and add geometry
  for(i in 1:length(gtfs_feeds)){
    # --- select specific feed
    feed_i <- gtfs_feeds[[i]]
    # --- convert shapes to sf
    feed_i_sf <- convert_shapes_to_sf(feed_i)
    # --- add agency id and name
    feed_i_joined <- feed_i_sf %>% select(shape_id) %>%
      left_join(feed_i$trips %>% select(route_id, shape_id), by = "shape_id") %>%
      left_join(feed_i$routes %>% select(route_id, agency_id), by = "route_id") %>%
      left_join(feed_i$agency %>% select(agency_id, agency_name), by = "agency_id")
    
    # add converted feed to list of results
    results[[i]] <- feed_i_joined
    print(paste0("Done with feed ", i))
  }
  # combine all results into one sf feature
  results <- bind_rows(results)
  # keep unique geometries
  results <- results[!duplicated(results$shape_id), ]
  return(results)
}

# apply the function
gtfs_sf <- convert_feeds_to_spatial(gtfs_feeds = gtfs_feeds)

# plot results
plot(gtfs_sf["agency_id"])

# save layer
st_write(gtfs_sf, paste0("../data_raw/", city, "/level_i/transit_routes.geojson"), delete_dsn = TRUE)


