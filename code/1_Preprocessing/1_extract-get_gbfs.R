###################################################################################################
###    The purpose of this script is to download gbfs data on station locations (docked)        ###
###    and service geographies (dockless). The information is spatially joined onto the         ###
###    census data to determine if micromobility is available in each census block / hexagon.   ###
###################################################################################################

library(gbfs)
library(tidyverse)
library(sf)

# 1. ---------- Identify all available feeds
gbfs_feeds <- gbfs::get_gbfs_cities()



# 2. ---------- Identify all DOCKED micromobility providers in the city

get_stations_in_city = function(city, feed_list){
  
  # --- Identify all micromobility providers in the city
  gbfs_feeds_city <- feed_list %>% 
    filter(str_detect(Location, city))
  
  # empty list to store dataframe results
  results <- vector(mode = "list", length = nrow(gbfs_feeds_city))
  
  # --- Get data from each provider
  for(i in 1:nrow(gbfs_feeds_city)){
    # print name of feed
    print(paste0("extracting data for: ", gbfs_feeds_city$Name[i]))
    # url of gbfs feed 
    gbfs_url <- gbfs_feeds_city$`Auto-Discovery URL`[i]
    # get the urls of each sub-feed, such as: (station_information, system_information, free_bike_status etc)
    gbfs_all_feeds <- jsonlite::fromJSON(txt = gbfs_url) %>% 
      as.data.frame() %>% 
      jsonlite::flatten()
    # check if it is docked
    if ("station_information" %in% gbfs_all_feeds$data.en.feeds.name){
      # get the url of the station feeds
      url_station_info <- gbfs_all_feeds$data.en.feeds.url[gbfs_all_feeds$data.en.feeds.name == "station_information"]
      url_station_status <- gbfs_all_feeds$data.en.feeds.url[gbfs_all_feeds$data.en.feeds.name == "station_status"]
      
      # get the data from "station_information" AND "station_status"
      station_info <- jsonlite::fromJSON(txt = url_station_info)
      
      # some providers have the "station_information" feed, but keep it empty. We skip those to avoid errors
      if(length(station_info$data$stations) > 0){
        # convert json to df
        station_info <- station_info %>% 
          as.data.frame() %>% 
          jsonlite::flatten()
        
        # get station status
        station_status <- jsonlite::fromJSON(txt = url_station_status) %>% 
          as.data.frame() %>% 
          jsonlite::flatten() 
        
        # join two tables together
        stations <- station_info %>% 
          left_join(station_status, by = "data.stations.station_id") %>%
          # convert to sf
          st_as_sf(., coords = c("data.stations.lon", "data.stations.lat")) #%>%
        #st_set_crs(select a suitable crs)
        
        # add colun referencing the provider
        stations <- stations %>% 
          mutate(provider = gbfs_feeds_city$Name[i])
        # add station data to results
        results[[i]] <- stations
      }
    }
  }
  
  # bind all sfs in the list together
  results <- bind_rows(results)
  # select necessary columns 
  results <- results %>%
    select(data.stations.station_id,
           data.stations.capacity,
           data.stations.name,
           data.stations.station_id,
           #data.stations.station_status
           data.stations.num_ebikes_available,
           data.stations.num_bikes_available,
           data.stations.num_docks_available,
           provider)
  
  # edit column names: remove "data.stations." prefix
  names(results) <- sub('^data.stations.', '', names(results))
  
  # return the data 
  return(results)
}

# Minneapolis
docks_mn <- get_stations_in_city(city = "Minneapolis", 
                                 feed_list = gbfs_feeds)

# Save
st_write(docks_mn, "../data_raw/Minneapolis/GBFS/gbfs_stations.geojson", delete_dsn = TRUE)

# San Francisco
docks_sf <- get_stations_in_city(city = "San Francisco", 
                                 feed_list = gbfs_feeds)

st_write(docks_sf, "../data_raw/San Francisco/GBFS/gbfs_stations.geojson", delete_dsn = TRUE)


# 3. -------- Identify all DOCKLESS micromobility providers in the city

get_dockless_zones = function(city, feed_list){
  
  # --- Identify all micromobility providers in the city
  gbfs_feeds_city <- feed_list %>% 
    filter(str_detect(Location, city))
  
  # empty list to store dataframe results
  results <- vector(mode = "list", length = nrow(gbfs_feeds_city))
  
  # --- Get data from each provider
  for(i in 1:nrow(gbfs_feeds_city)){
    # print name of feed
    print(paste0("extracting data for: ", gbfs_feeds_city$Name[i]))
    # url of gbfs feed 
    gbfs_url <- gbfs_feeds_city$`Auto-Discovery URL`[i]
    # get the urls of each sub-feed, such as: (station_information, system_information, free_bike_status etc)
    gbfs_all_feeds <- jsonlite::fromJSON(txt = gbfs_url) %>% 
      as.data.frame() %>% 
      jsonlite::flatten()
    
    # check if it is dockless i.e. if the feed defines a geofencing zone
    if ("geofencing_zones" %in% gbfs_all_feeds$data.en.feeds.name){
      # get url of geofencing zones
      url_zones <- gbfs_all_feeds$data.en.feeds.url[gbfs_all_feeds$data.en.feeds.name == "geofencing_zones"]
      
      # download json for geofencing xzones
      geo_zones <- jsonlite::fromJSON(txt = url_zones) 
      # extract features
      geo_zones <- jsonlite::toJSON(geo_zones$data$geofencing_zones$features)
      # convert features to sf
      geo_zones <- geojsonsf::geojson_sf(geo_zones) 
      # add provider column
      geo_zones <- geo_zones %>% 
        mutate(provider = gbfs_feeds_city$Name[i])
      
      # parse "rules" json column to 2 columns 
      #(https://stackoverflow.com/questions/38858345/parse-error-trailing-garbage-while-trying-to-parse-json-column-in-data-frame)
      geo_zones_rules <- purrr::map(geo_zones$rules, jsonlite::fromJSON) %>%
        bind_rows()
      # bind it back to the zones sf
      geo_zones <- geo_zones %>%
        bind_cols(geo_zones_rules) %>% 
        select(-rules)
      
      # remove zones that are restricted
      geo_zones <- geo_zones %>%
        filter(ride_allowed == TRUE, ride_through_allowed == TRUE)
      
      # add zone data to results
      results[[i]] <- geo_zones
    }
  }
  # bind all sfs in the list together
  results <- bind_rows(results)
  
  # convert start and end columns to time values
  results$start <- format(as.POSIXct(lubridate::as_datetime(results$start)), format = "%H:%M:%S")
  results$end <- format(as.POSIXct(lubridate::as_datetime(results$end)), format = "%H:%M:%S")
  
  # return the data 
  return(results)
}

# Minneapolis
dockless_mn <- get_dockless_zones(city = "Minneapolis", 
                                  feed_list = gbfs_feeds)

# Save
st_write(dockless_mn, "../data_raw/Minneapolis/GBFS/gbfs_zones.geojson", delete_dsn = TRUE)


# San Francisco
dockless_sf <- get_dockless_zones(city = "San Francisco", 
                                  feed_list = gbfs_feeds)

st_write(dockless_sf, "../data_raw/San Francisco/GBFS/gbfs_zones.geojson", delete_dsn = TRUE)

