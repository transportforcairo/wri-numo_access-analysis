###################################################################################################
###    The purpose of this script is to calculate a travel time matrix for each different mode  ###
###    combination. r5r is used for the calculations                                            ###
###################################################################################################


# increase the memory available to Java. Needs to be done at the beginning of the script
options(java.parameters = "-Xmx20G")  # 3 gegabytes

library(r5r)
library(sf)
library(tidyverse)

# ------------------------------------- Define Variables ------------------------------------- #
# city
#city <- "San Francisco"
#city <- "Mexico City"
city <- "Minneapolis"
#city <- "Cairo"

# --- define paths 
# path to folder with gtfs, road network and elevation data
data_path <- paste0("../data_raw/", city, "/GTFS/")
# path where pbf files are stored
pbf_path <- paste0("../data_raw/", city, "/PBFs/") 

# are we running the analysis for congested (yes) or freeflow (no) speeds
#congested <- "yes"
congested <- "no"


# ------------------------------------- Load in the data ------------------------------------- #

# city geometry 
city_geom <- st_read(paste0("../data/", city, "/level_i_ii/zones_w_micromobility_providers.geojson"))

# prep data for inputting to r5r
# Function takes a base layer, gets the geometry centroid, and renames the id column that we pass to it into a standard name
prep_base_layer = function(layer, id_col){
  
  id_col = sym(id_col)
  
  layer = layer %>% select(!! id_col) %>% 
    st_centroid() %>%   # we route from zone centroids
    rename(id = !! id_col)
}

# apply the function
city_geom_r5 <- prep_base_layer(layer = city_geom, id_col = "cell_id")

# ------------------------------------- PBF data 
# We have different PBFs for each city (freeflow / congested). We need to make sure we using the correct one. 
  # PBFs for any city are stored in <City>/PBFs. 
  # We copy the correct one into the <City>/GTFS directory, and delete PBFs / .mapdb / .dat files from previous runs

pbf_files <- tribble(
  ~city, ~freeflow_pbf_file, ~congested_pbf_file, 
  # San Francisco
  "San Francisco", "SF_original3.osm.pbf" ,  "SF_real_speed3.osm.pbf", # docked_Bay.Wheels
  #"San Francisco", "dockless_Spin.San.Francisco",  TRUE,
  # Minneapolis
  "Minneapolis", "MN_original.osm.pbf",  NA,  # docked_Lyft.Scooters.Minneapolis
  # "Minneapolis", "dockless_Spin.Minneapolis",  TRUE,
  # Mexico City
  "Mexico City", "planet_mx.osm.pbf",  "mx_real1.osm.pbf",   # docked_ECOBICI
  # Cairo
  "Cairo", "cairo.osm.pbf",  "cairo_real1.osm.pbf"   # docked_ECOBICI
)

# --- identify pbf file names from table
freeflow_pbf <- pbf_files$freeflow_pbf_file[pbf_files$city == city]
congested_pbf <- pbf_files$congested_pbf_file[pbf_files$city == city]

# --- delete files in GTFS directory

# remove existing pbf file
# system(paste0("rm ", data_path, "*.pbf"))
# gsub to replace spaces with '\' https://stackoverflow.com/questions/38909016/r-how-to-replace-space-in-string-with-a-single-backslash-and-space
system(paste0("rm ", gsub(" ", "\\\ ", data_path, fixed = TRUE), "*.pbf"))


# remove other files created by r5 during graph building 
#system(paste0("rm ", data_path, "*.p"))
system(paste0("rm ", gsub(" ", "\\\ ", data_path, fixed = TRUE), "*.p"))
system(paste0("rm ", gsub(" ", "\\\ ", data_path, fixed = TRUE), "*.dat"))
system(paste0("rm ", gsub(" ", "\\\ ", data_path, fixed = TRUE), "*.map"))
system(paste0("rm ", gsub(" ", "\\\ ", data_path, fixed = TRUE), "*.mapdb"))

# --- copy pbf file we want into GTFS directory (directory where graph is built)
# check which file to move (depends on whether we want congestion or not)
if(congested == "yes"){
  # move the augmented pbf to the gtfs directory
  system(paste0("cp ", gsub(" ", "\\\ ", pbf_path, fixed = TRUE), congested_pbf, " ", 
                gsub(" ", "\\\ ", data_path, fixed = TRUE)))
} else {
  # mve the unedited pbf into the pbf directory
  system(paste0("cp ", gsub(" ", "\\\ ", pbf_path, fixed = TRUE), freeflow_pbf, " ", 
                gsub(" ", "\\\ ", data_path, fixed = TRUE)))
}

# ------------------------------------- Setup r5 ------------------------------------- #

# # need to edit pbf to avoid a turn-restrictions related bug in r5 
# # https://github.com/ipeaGIT/r5r/issues/141#issuecomment-767648579
# r5r_core <- setup_r5(data_path = data_path, 
#                      verbose = TRUE,
#                      use_elevation = TRUE,   # we need to download a tiff file and then turn this to TRUE
#                      overwrite = TRUE) # turn to true once we have elevation


# ------------------------------------- Determine GTFS Calendar Dates ------------------------------------- #

# (1) Merge GTFS feeds    # --- DO THIS IN A SEPERATE SCRIPT
# (2) Determine GTFS validity period from calendar. If we pass a date outside of the calendar window, the 
# GTFS will not be used in routing

# ------------------------------------- Define combinations ------------------------------------- #

# --- tibble with all mode combinations 

combinations <- tribble(
  ~combination, ~mode,  ~egress_mode, ~max_walk,
  # public transport
  1, c("WALK", "TRANSIT"),  "WALK", 500,
  # public transport + micromobility as a first mile option only
  2.1, c("WALK", "TRANSIT", "BICYCLE"),  "WALK",  500,
  # public transport + micromobility as a last mile option only
  2.2, c("WALK", "TRANSIT"), "BICYCLE", 500,
  # public transport + micromobility as a first and last mile option only
  2.3, c("WALK", "TRANSIT", "BICYCLE"),  "BICYCLE", 500,
  # cycling / micromobility only
  3, "BICYCLE",  "WALK", 500,
  # car
  4, c("WALK", "CAR"),  "WALK", 500,
  # walk only 
  5, "WALK",  "WALK", 5000
)


# ------------------------------------- Define Routing Parameters ------------------------------------- #

max_walk_dist <- 1000   # meters
max_trip_duration <- 75 # minutes

# change this later. Read the GTFS feeds and identify their calendar period

# Mexico City
# departure_datetime <- as.POSIXct("24-01-2022 07:30:00",
#                                  format = "%d-%m-%Y %H:%M:%S")
# San Francisco
# departure_datetime <- as.POSIXct("10-01-2022 07:30:00",
#                                  format = "%d-%m-%Y %H:%M:%S")

# Minneapolis
# departure_datetime <- as.POSIXct("24-01-2022 07:30:00",
#                                  format = "%d-%m-%Y %H:%M:%S")

# generic. Works for all cities after we updated the calendar.txt
departure_datetime <- as.POSIXct("24-01-2022 07:30:00",
                                 format = "%d-%m-%Y %H:%M:%S")

time_window <- 60 # in minutes (adding this value to departure datetime, gives you the end of the time window)
percentiles <- 75



# ------------------------------------- Define Routing Parameters ------------------------------------- #


# --- calculate a travel time matrix
# ttm_transit <- travel_time_matrix(r5r_core = r5r_core,
#                                   origins = city_geom_r5 ,
#                                   destinations = city_geom_r5,
#                                   time_window = time_window,
#                                   percentiles = percentiles,
#                                   mode = combinations$mode[1][[1]],
#                                   mode_egress = combinations$egress_mode[1][[1]],
#                                   departure_datetime = departure_datetime,
#                                   max_walk_dist = max_walk_dist,
#                                   max_trip_duration = max_trip_duration,
#                                   max_lts = 2,
#                                   # to suppress r5 output. Change to true if debugging
#                                   verbose = FALSE, 
#                                   # slow down function by ~20%
#                                   progress = TRUE)



# FUNCTION: Calculate travel time for all combinations and return the result as 1 dataframe

tt_matrix = function(data_path,
                     combinations, 
                     zone_layer,
                     elevation,
                     # bike speed for normal and electric vehicles
                     bike_speed_e,
                     bike_speed_c,
                     max_lts){
  
  # stop any running r5 instances
  # r5r::stop_r5()
  # setup r5
  print("Setting up r5...")
  r5r_core <- setup_r5(data_path = data_path, 
                       verbose = TRUE,
                       use_elevation = elevation,   # we need to download a tiff file and then turn this to TRUE
                       overwrite = TRUE) # turn to true once we have elevation
  
  
  # empty list to store results for each combination
  results <- vector(mode = "list", length = nrow(combinations))
 
  print("Calculating travel times...")
  # calculate travel time matrix for each combination
  for(i in 1:nrow(combinations)){
    #status updates
    print(paste0("CALCULATING TRAVEL TIME FOR combination: ", combinations$combination[i], " ....."))
    # calculate a travel time matrix
    ttm <- r5r::travel_time_matrix(r5r_core = r5r_core,
                                   origins = zone_layer ,
                                   destinations = zone_layer,
                                   time_window = time_window,
                                   percentiles = percentiles,
                                   mode = combinations$mode[i][[1]],
                                   mode_egress = combinations$egress_mode[i][[1]],
                                   departure_datetime = departure_datetime,
                                   max_walk_dist = combinations$max_walk[i][[1]], #max_walk_dist, 
                                   max_trip_duration = max_trip_duration,
                                   # if elevation is TRUE, we are looking at classic bikes, otherwise electric
                                   bike_speed = ifelse(elevation == TRUE, bike_speed_c, bike_speed_e),
                                   max_lts = max_lts,
                                   # number of threads
                                   #n_threads = 2,
                                   # to suppress r5 output. Change to true if debugging
                                   verbose = FALSE, 
                                   # slow down function by ~20%
                                   progress = TRUE)
    
    # add column with combination name / number
    # if elevation = TRUE, we are looking at classic bikes
    if (elevation == TRUE){
      ttm$combination <- stringr::str_c(combinations$combination[i], "_classic")} else{
        # if elevation = FALSE, we are looking at electric bikes
        ttm$combination <- stringr::str_c(combinations$combination[i], "_electric")
        
      }
      
    # add ttm to results list
    results[[i]] <- ttm
    #status updates
    print(paste0("COMPLETED combination: ", combinations$combination[i], " ....."))
    
  }
  # stop r5
  r5r::stop_r5()
  # combine list into 1 dataframe
  results <- bind_rows(results)
  # pivot wider to get 1 row per OD pair
  results <- results %>% pivot_wider(names_from = combination,
                                     values_from = travel_time)
  
  return(results)
  
}


# apply the function

# classic
tt_matrix_city_c <- tt_matrix(zone_layer = city_geom_r5,
                              combinations = combinations,
                              data_path = data_path,
                              elevation = TRUE,
                              # bike speed for normal and electric vehicles
                              bike_speed_e = 22.5,
                              bike_speed_c = 16.6,
                              max_lts = 2)

# temporary as I don't have the time to run the below function now:

#write_csv(tt_matrix_city_c, paste0("../data/", city, "/level_i_ii/freeflow_classic_temp.csv"))
#tt_matrix_city_c <- read_csv(paste0("../data/", city, "/level_i_ii/freeflow_classic_temp.csv"))
#write_csv(tt_matrix_city_c, paste0("../data/", city, "/level_i_ii/congested_classic_temp.csv"))
#tt_matrix_city_c <- read_csv(paste0("../data/", city, "/level_i_ii/congested_classic_temp.csv"))
# temporary - END

# electric
tt_matrix_city_e <- tt_matrix(zone_layer = city_geom_r5,
                            combinations = combinations,
                            data_path = data_path,
                            elevation = FALSE,
                            # bike speed for normal and electric vehicles
                            bike_speed_e = 22.5,
                            bike_speed_c = 16.6,
                            max_lts = 2)

tt_matrix_city <- tt_matrix_city_c %>% 
  full_join(tt_matrix_city_e, by = c("fromId", "toId"))

# save
#write_csv(tt_matrix_city, paste0("../data/", city, "/level_i_ii/travel_time_matrix_freeflow.csv"))
#write_csv(tt_matrix_city, paste0("../data/", city, "/level_i_ii/travel_time_matrix.csv"))





# --- test free flow vs congested speeds

# travel time matrix with freeflow car speeds
tt_matrix_city_free <- read_csv(paste0("../data/", city, "/level_i_ii/travel_time_matrix_freeflow.csv"))
# 
# travel time matrix using edited pbf (more realistic travel speeds)
tt_matrix_city <- read_csv(paste0("../data/", city, "/level_i_ii/travel_time_matrix.csv"))
# 
tt_matrix_city <- tt_matrix_city %>%
   mutate(across(fromId:toId, as.numeric))
# 
# congested joined with free
tt_matrix <- tt_matrix_city %>% left_join(tt_matrix_city_free, by = c("fromId", "toId"))
# #
# percentage change in speeds
tt_matrix <- tt_matrix %>% mutate(perc_change_1 = ((`1_classic.x` - `1_classic.y`) / `1_classic.y`) * 100,
                                  perc_change_2.1 = ((`2.1_classic.x` - `2.1_classic.y`) / `2.1_classic.y`) * 100,
                                  perc_change_2.2 = ((`2.2_classic.x` - `2.2_classic.y`) / `2.2_classic.y`) * 100,
                                  perc_change_2.3 = ((`2.3_classic.x` - `2.3_classic.y`) / `2.3_classic.y`) * 100,
                                  perc_change_3 = ((`3_classic.x` - `3_classic.y`) / `3_classic.y`) * 100,
                                  perc_change_4 = ((`4_classic.x` - `4_classic.y`) / `4_classic.y`) * 100,
                                  perc_change_5 = ((`5_classic.x` - `5_classic.y`) / `5_classic.y`) * 100
                                  )
# #
# plot
tt_matrix %>% select(fromId, toId, contains("perc_change")) %>%
  pivot_longer(cols = perc_change_1:perc_change_4) %>%
  ggplot(aes(x=value, fill=name)) +
  geom_histogram(alpha=0.7, position = 'identity', binwidth = 10) +
  facet_wrap(~ name) +
  #scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_minimal() +
  labs(fill="") + 
# axis limits
 scale_x_continuous(limits = c(NA, 200))




