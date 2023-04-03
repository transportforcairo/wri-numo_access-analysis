#library(osmextract)
library(sf)
library(tidyverse)

# define city 
#city <- "Mexico City"
#city <- "Minneapolis"
city  <- "San Francisco"
# ----------------------------------------- Download OSM road network data ----------------------------------------- #

# I use the bbbike API and save the exported data to the 'data/gtfs-osm' directory
# https://extract.bbbike.org/



# ----------------------------------------- Download elevation data (tif file) ----------------------------------------- #

# load in city geometry 
#city_geom <- st_read(paste0("../data/", city, "/level_i_ii/zones_w_micromobility_providers.geojson"))
city_geom <- st_read(paste0("../data_raw/", city, "/level_i/variable_hexgrid.geojson")) %>%
  st_transform(4326)

# remove any rows that aren't polygons
city_geom <- city_geom %>% filter(st_is(. , c("POLYGON", "MULTIPOLYGON")))

# remove empty geometry rows
city_geom <- city_geom[ ! st_is_empty(city_geom) , ]

# download elevation
city_elev <- elevatr::get_elev_raster(city_geom, z= 9, expand = 0.05)

# save
raster::writeRaster(city_elev, paste0("../data/", city, "/GTFS/city_elev.tif"), overwrite = TRUE)

# Download elevation data (tif file)

# add lts value for cycling based on available infrastructure + osm road type 
# https://github.com/ITDP/two_step_access/blob/main/prep_bike_osm.py

# Prepare points to query from 

# Edit r5 build config file 

# https://github.com/ipeaGIT/r5r/issues/187#issuecomment-887858477
library(r5r)
r5r_core$defaultBuildConfig()




