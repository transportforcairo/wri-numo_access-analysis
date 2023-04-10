###################################################################################################
###    The purpose of this script is to edit to download OSM road network data through R        ###
###    The data could also be downloaded manually through other tools such as the OSM HOT       ###
###    Export Tool
###################################################################################################

library(osmextract)
library(sf)

# --------------------------------- PREPARE THE INPUT DATA --------------------------------- #

# city <- "San Francisco"
# city <- "Minneapolis"
# layer_name <- "census_bg.geojson"

# # Mexico City
# city <- "Mexico City"
# layer_name <- "census_pop_jobs.geojson"

# Cairo
city <- "Cairo"
layer_name <- "gcr_shiyakhas_with_jobs.geojson"


# Read in the census layer
city_geom <- st_read(paste0("../data_raw/", city, "/level_i/", layer_name)) %>%
  # we need a metric crs to create the grid layer
  st_transform(4326) %>%
  st_make_valid()

# --------------------------------- DOWNLOAD THE PBF DATA --------------------------------- #

# bounding box 
bounding_box <- st_as_sfc(st_bbox(city_geom))
# path to store the pbf data
pbf_directory <- paste0("../data/", city, "/GTFS/")
# download the data 
osm_lines = oe_get(city_geom, stringsAsFactors = FALSE, quiet = TRUE,
                   provider = "geofabrik",
                   boundary = bounding_box,
                   boundary_type = "spat",
                   #download_only = TRUE,
                   download_directory = pbf_directory)
