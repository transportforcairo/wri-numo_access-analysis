library(sf)
library(tidyverse)
library(tidytransit)

city <- "San Francisco"
city <- "Minneapolis"


# -------------------------- read in the gtfs data --------------------------- # 
# directory where GTFS feeds are located
gtfs_dir <- paste0("../data_raw/", city, "/GTFS/")
# select all feeds in a specific directory
feeds <- dir(gtfs_dir, ".zip$", full.names = TRUE)
# read in all the files using purrr::map   # safely used to prevent errors
# https://aosmith.rbind.io/2020/08/31/handling-errors/
gtfs_feeds <- map(feeds, purrr::safely(tidytransit::read_gtfs))
# extract feeds that didn't cause errors
gtfs_feeds <- gtfs_feeds %>%
  map("result") %>%
  # remove null results
  compact()
# -------------------------- combine stops from all feeds --------------------------- # 

# ---- turn stops to simple features

# a) extract stops
gtfs_stops <- map(gtfs_feeds, pluck("stops")) 
# b) convert stops dfs to sf features
gtfs_stops <- map(gtfs_stops, tidytransit::stops_as_sf)
# c) convert all feeds to one crs
#gtfs_stops <-- map(gtfs_stops, st_transform(3857)) # not working :(
for(i in 1:length(gtfs_stops)){
  gtfs_stops[[i]] <- gtfs_stops[[i]] %>% st_transform(3857)
}  
# d) combine into one sf feature
gtfs_stops <- bind_rows(gtfs_stops)

# -------------------------- get boundary of stops --------------------------- # 

# option 1: convex hull
gtfs_stops_boundary <- gtfs_stops %>% 
  st_union() %>%
  st_convex_hull()

# option 2: buffer around stops and cast to polygon
gtfs_stops_boundary2 <- gtfs_stops %>% 
  st_buffer(3000) %>%
  st_union() %>%
  st_cast("POLYGON")

# urban_boundary <- st_read(paste0("../data/", city, "/level_i_ii/zones_w_micromobility_providers.geojson")) %>%
#   st_transform(3857)
