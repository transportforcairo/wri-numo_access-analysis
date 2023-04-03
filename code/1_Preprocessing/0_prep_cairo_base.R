###############################################################################################
### This script is used to prepare the base layer (population + employment) for Cairo       ###
###############################################################################################

library(sf)
library(tidyverse)

# ------------------------------------- Load the layers ------------------------------------ #
city <- "Cairo"
# administrative boundaries with population and employment (polygon)
city_geom <- st_read(paste0("../data_raw/", city, "/level_i/gcr_shiyakhas_with_jobs.geojson")) 
# remove new cities
city_geom <- city_geom %>% filter(zoning_tfc %in% c("Inner", "Central"))
# save 
st_write(city_geom, paste0("../data_raw/", city, "/level_i/gcr_shiyakhas_with_jobs.geojson"), delete_dsn = TRUE)
