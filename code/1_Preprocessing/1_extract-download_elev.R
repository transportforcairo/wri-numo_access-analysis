
# define city 
#city <- "Mexico City"
#city <- "Minneapolis"
city  <- "San Francisco"

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