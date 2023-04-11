###################################################################################################
###    The purpose of this script is to calculate a detailed itinerary for each different mode  ###
###    combination. Detailed itineraries are necessary to determine, for each OD pair, which    ###
###    modes are actually being used. The travel_time_matrix function used in                   ###
###    3.3_accessibility_analysis determines fastest travel times using permitted modes, but    ###
###    does not return information on which modes were used. We need this information, along    ### 
###    with probability of finding a shared bike (calculated using MDS data), to determine      ###
###    accessibility with supply constraints                                                    ###
###################################################################################################

### LIMITATION: 
# We cannot control bicycle being used as an access mode only in r5r. mode and mode_access
# are grouped together in the same parameter. 
# The result is that bicycles are sometimes used as a main mode, even if we put max_bike_dist, as 
# max_bike_dist only controls access / egress distances. This affects combination 2.1. One workaround 
# is to set shortest = FALSE to get multiple trip options for each OD pair, and then select the one 
# that matches our conditions (bike used as access only). This is infeasible for large datasets

# increase the memory available to Java. Needs to be done at the beginning of the script
options(java.parameters = "-Xmx22G")  # 3 gegabytes

library(r5r)
library(sf)
library(tidyverse)
library(data.table)

# ------------------------------------- Define Variables ------------------------------------- #
# city
city <- "San Francisco"
state <- "CA"

#city <- "Minneapolis"
#state <- "MN"

# --- define paths 
# path to folder with gtfs, road network and elevation data
data_path <- paste0("../data_raw/", city, "/GTFS/")
# path where pbf files are stored
pbf_path <- paste0("../data_raw/", city, "/PBFs/") 

# are we running the analysis for congested (yes) or freeflow (no) speeds
congested <- "yes"
#congested <- "no"


# ------------------------------------- Load in the data ------------------------------------- #

# city geometry 
city_geom <- st_read(paste0("../data/", city, "/level_i_ii/zones_w_micromobility_providers.geojson"))

# get boundary of SF city (we want to reduce scope for detailed_itineraries() to run)
city_boundary <- tigris::places(state) %>%
  tigris::filter_place(city) %>%
  st_transform(4326)

# filter to smaller scope
city_geom_improved <- city_geom %>% st_filter(city_boundary, .predicate = st_within)

# # origins that had improved accessibility due to mm (getting detailed itineraries for all origins would take days)
# city_geom_improved <- st_read(paste0("../data/", city, "/level_i_ii/accessibility_results.geojson"))
# city_geom_improved <- city_geom_improved %>% 
#   filter(jobs_tt_improve_dockless_pt_60 > 0, na.rm = TRUE)

# travel time data
#city_tt <- vroom::vroom(paste0("../data/", city, "/level_i_ii/travel_time_matrix_w_metadata.csv"))
city_tt <- read_csv(paste0("../data/", city, "/level_i_ii/travel_time_matrix_w_metadata.csv.gz"))


# select only the travel time columns (and supply constraint if applicable)
city_tt <- city_tt %>% select(fromId, toId, contains(c("tt_", "frac_", "_service"))) 

# # replace na travel time values with very big values
city_tt <- city_tt %>%
  mutate(across(contains("tt_"), ~replace_na(., 1000)))

# define job column(s)
job_cols <- "C000"

# Add destination jobs to travel time matrix #
city_tt <- city_tt %>% 
  left_join(city_geom %>% st_drop_geometry() %>% select(cell_id, all_of(job_cols)), by = c("toId" = "cell_id"))



# prep data for inputting to r5r
# Function takes a base layer, gets the geometry centroid, and renames the id column that we pass to it into a standard name
prep_base_layer = function(layer, id_col){
  
  id_col = sym(id_col)
  
  layer = layer %>% select(!! id_col) %>% 
    st_centroid() %>%   # we route from zone centroids
    rename(id = !! id_col)
}

# apply the function
city_geom_r5 <- prep_base_layer(layer = city_geom_improved, id_col = "cell_id")

# ------------------------------------- PBF data 
# We have different PBFs for each city (freeflow / congested). We need to make sure we using the correct one. 
# PBFs for any city are stored in <City>/PBFs. 
# We copy the correct one into the <City>/GTFS directory, and delete PBFs / .mapdb / .dat files from previous runs

pbf_files <- tribble(
  ~city, ~freeflow_pbf_file, ~congested_pbf_file, 
  # San Francisco
  "San Francisco", "SF_original3.osm.pbf" ,  "SF_real_speed3.osm.pbf",
  # Minneapolis
  "Minneapolis", "MN_original.osm.pbf",  NA
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


# ------------------------------------- Define mode combinations ------------------------------------- #

# --- tibble with all mode combinations 

# max bike distance is useful when micromobility is available as a first or last mile option only. 
# It prevents long trips by bike
combinations <- tribble(
  ~combination, ~mode,  ~egress_mode, ~max_walk, ~max_bike_dist,
  # # public transport
  # 1, c("WALK", "TRANSIT"),  "WALK", 500, 1000,
  # public transport + micromobility as a first mile option only
  2.1, c("WALK", "TRANSIT", "BICYCLE"),  "WALK",  500, 1000,
  # public transport + micromobility as a last mile option only
  2.2, c("WALK", "TRANSIT"), "BICYCLE", 500, 1000,
  # public transport + micromobility as a first and last mile option only (or as mode for entire trip)
  2.3, c("WALK", "TRANSIT", "BICYCLE"),  "BICYCLE", 500, 10000,
  # # cycling / micromobility only
  # 3, "BICYCLE",  "WALK", 500, 1000
  # # car
  # 4, c("WALK", "CAR"),  "WALK", 500, 1000,
  # # walk only 
  # 5, "WALK",  "WALK", 5000, 1000
)


# ------------------------------------- Define Routing Parameters ------------------------------------- #

max_walk_dist <- 1000   # meters
max_trip_duration <- 75 # minutes

# generic time. Works for all cities after we updated the calendar.txt
departure_datetime <- as.POSIXct("24-01-2022 07:30:00",
                                 format = "%d-%m-%Y %H:%M:%S")

time_window <- 60 # in minutes (adding this value to departure datetime, gives you the end of the time window)
percentiles <- 75



# --------------------------- Create OD matrix for r5r::detailed_itineraries() --------------------------- #

# --- detailed itineraries


#------- start: create od matrix  Rafa Perreira code

# function to turn sf point column to integer lat and lon columns
sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}

# get origins and destinations dfs
origins <- sfc_as_cols(city_geom_r5) %>% 
  st_drop_geometry() %>%
  rename(c(lon = x, lat = y)) #%>%
  #filter(id %in% c(1:20, 300:320))

destinations <- sfc_as_cols(city_geom_r5) %>% 
  st_drop_geometry() %>%
  rename(c(lon = x, lat = y)) #%>%
  #filter(id %in% c(1:20, 300:320))


# get all possible combinations between origins and destinations
# function
get_all_od_combinations <- function(origins, destinations){
  
  # all possible id combinations
  base <- expand.grid(origins$id, destinations$id)
  
  # rename df
  setDT(base)
  setnames(base, 'Var1', 'idorig')
  setnames(base, 'Var2', 'iddest')
  
  # bring spatial coordinates from origin and destination
  base[origins, on=c('idorig'='id'), c('lon_orig', 'lat_orig') := list(i.lon, i.lat)]
  base[destinations, on=c('iddest'='id'), c('lon_dest', 'lat_dest') := list(i.lon, i.lat)]
  
  return(base)
}

df <- get_all_od_combinations(origins, destinations)

# select/rename columns for r5r input
all_orig <- dplyr::select(df, c('id'=idorig, 'lon'=lon_orig,'lat'=lat_orig))
all_dest <- dplyr::select(df, c('id'=iddest, 'lon'=lon_dest,'lat'=lat_dest))

# we don't need this
rm(df)
#------- end: create od matrix  Rafa Perreira code

# code to calculate detailed itineraries for each OD pair using each different mode combination

det_itineraties = function(data_path,
                           combinations, 
                           origins,
                           destinations,
                           elevation,
                           bike_speed,
                           # bike speed for normal and electric vehicles
                           max_lts,
                           chunk){
  
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
    
    # number of iterations 
    iterations <- floor(nrow(origins)/chunk) + 1
    # list to store results in. Each list element will be a dataframe
    results_combination <- vector(mode = "list", length = iterations)
    
    for(j in 1:iterations){
      # determine the slice of the od pair buffer that we will operate on
      from_row <- 1 + ((j-1)*chunk) 
      to_row <- min(j*chunk, nrow(origins))
      
      # calculate a travel time matrix
      ttm <- r5r::detailed_itineraries(r5r_core = r5r_core,
                                       origins = origins[from_row:to_row,],
                                       destinations = destinations[from_row:to_row,],
                                       mode = combinations$mode[i][[1]],
                                       mode_egress = combinations$egress_mode[i][[1]],
                                       departure_datetime = departure_datetime,
                                       max_walk_dist = max_walk_dist,
                                       max_bike_dist = combinations$max_bike_dist[i][[1]],
                                       max_trip_duration = max_trip_duration,
                                       bike_speed = bike_speed,
                                       max_lts = max_lts,
                                       # all_to_all = TRUE,   # in development version only
                                       drop_geometry = TRUE,
                                       shortest_path = TRUE,
                                       verbose = FALSE,
                                       progress = TRUE)
      
      if (elevation == TRUE){
        ttm$combination <- stringr::str_c(combinations$combination[i], "_classic")} else{
          # if elevation = FALSE, we are looking at electric bikes
          ttm$combination <- stringr::str_c(combinations$combination[i], "_electric")
          
        }
      
      # # determine access and egress mode for each trip and keep only essential rows and columns
      ttm <- ttm %>%
        group_by(fromId, toId) %>%
        # arrange by trip leg
        arrange(segment) %>%
        # determine access and egress mode
        mutate(access = mode[row_number() == 1],
               egress = mode[row_number() == n()],
               distance = sum(distance)) %>%
        ungroup() %>%
        # keep only one row per trip (remove rows that correspond to the different segments)
        group_by(fromId, toId) %>%
        filter(row_number()==1) %>%
        ungroup() %>%
        # keep only necessary columns
        select(fromId, toId, total_duration, distance, access, egress, combination)
      
      
      
      # store result in list
      results_combination[[j]] <- ttm
      # Optional: save data for this step locally
      write_csv(ttm, paste0("../data/", city, "/level_i_ii/supply_constraints/partial_results/supply_constaints_", 
                            combinations$combination[i], "_", j))
      
      print(paste0("iterated over ", min(j*chunk, nrow(origins)), " of ", nrow(origins), " od pairs for combination ", 
                   combinations$combination[i], ".csv"))
    }
    
    # all calculations for a particular combination done:
    
    # 1. bind all dfs for that combination together
    results_combination <- bind_rows(results_combination)
    # 2. add data for that combination to the final results list
    results[[i]] <- results_combination
    # 3. OPTIONAL: save data for that combination locally
    write_csv(results_combination, paste0("../data/", city, "/level_i_ii/supply_constraints/supply_constaints_", combinations$combination[i], ".csv"))
    # status update 
    print(paste0("COMPLETED combination: ", combinations$combination[i], " ....."))
    
  }
  # stop r5
  r5r::stop_r5()
  # combine list into 1 dataframe
  results <- bind_rows(results)
  
  return(results)
}

# apply the function

det_itin_city <- det_itineraties(combinations = combinations,
                                 origins = all_orig,
                                 destinations = all_dest,
                                 data_path = data_path,
                                 elevation = TRUE,
                                 # bike speed for normal and electric vehicles
                                 bike_speed = 22.5, # 22.5 (electric), 16.6 (classic)
                                 max_lts = 2,
                                 chunk = 20000)

# ---------------- OPTIONAL: read in the data - if starting from a different session

# select all files in a specific directory
#files <- dir("Exit_by_Entry/", ".xlsx$", full.names = TRUE)
files <- dir(paste0("../data/", city, "/level_i_ii/supply_constraints/"), ".csv$", full.names = TRUE)
# read in all the files using purrr::map
csv_files <- map(files, read_csv)
# combine csvs into one df
det_itin_city <- bind_rows(csv_files)
# remove unnecessary data
rm(files, csv_files)
# ----------------


# --- add availability of dockless for origin and destination 
det_itin_city <- det_itin_city %>% 
  mutate(fromId = as.character(fromId), toId = as.character(toId)) %>%
  inner_join(city_tt %>% select(fromId, toId, dockless_service) %>%
               mutate(across(fromId:toId, as.character)),
             by = c("fromId", "toId"))

# determine which mode combination is being used. For each OD pair, we select 
# the fastest out of all FEASIBLE options (feasibility is based on dockless availability at O and D)

# pivot wider to get 1 row per OD pair
det_itin_city_wide <- det_itin_city %>% 
  mutate(fromId = as.character(fromId)) %>%
  select(-c(access, egress, distance)) %>% 
  pivot_wider(names_from = combination,
              values_from = total_duration)

# GET NAME OF COMBINATION CORRESPONDING TO SHORTEST TIME

# Step 1: Identify smallest column for scenario 2.3 (cycling available for all options). 

# https://community.rstudio.com/t/find-column-name-with-minimum-value/88117/2
det_itin_city_wide$best_comb <- names(det_itin_city_wide[,4:6])[apply(det_itin_city_wide[,4:6], 
                                                                      MARGIN = 1, FUN = which.min)]

# Step 2: Adjust answers for scenario 2.1, 2.2

det_itin_city_wide <- det_itin_city_wide %>% 
  mutate(best_comb = case_when(
    dockless_service == "first" ~ "2.1_classic",
    dockless_service == "last" ~ "2.2_classic",
    dockless_service == "both" ~ best_comb,
    dockless_service == "none" ~ "invalid"
  ))


# join access and egress modes onto the data (join based on scenario chosen)

det_itin_city_wide_t <- det_itin_city_wide %>% 
  inner_join(det_itin_city %>% select(fromId, toId, access, egress, combination),
             by = c("fromId", "toId", "best_comb" = "combination"))

# join access and egress modes onto the travel time matrix data
city_tt <- city_tt %>% left_join(det_itin_city_wide_t %>% 
                                   mutate(fromId = as.numeric(fromId), toId = as.numeric(toId)) %>%
                                   select(fromId, toId, access, egress),
                                 by = c("fromId", "toId"))

# ----------------------------------- Calculate si and sj for each OD pair ---------------------------------- #

# si and sj are only used if the access/egress mode is bicycle
city_tt <- city_tt %>%
  mutate(si = case_when(access == "BICYCLE" ~ frac_available_o,
                        TRUE ~ 1),
         sj = case_when(egress == "BICYCLE" ~ frac_available_d,
                        TRUE ~ 1))


# ------------------------------------ get dockless improvement --------------------------------------------- #

# a) identify destinations reachable by mm within tt threshold but not by pt
# b) multiply jobs at each of these destinations by sj
# c) sum jobs for each origin

# --- multiply by destination constraint (sj)

improvement_constrained <- city_tt %>%
  mutate(
    # wij = 1 if travel time is less than defined threshold, 0 otherwise
    wij_pt_60 = case_when(tt_pt <= 60 ~ 1,
                          TRUE ~ 0),
    wij_pt_45 = case_when(tt_pt <= 45 ~ 1,
                          TRUE ~ 0),
    wij_pt_30 = case_when(tt_pt <= 30 ~ 1,
                          TRUE ~ 0),
    wij_pt_15 = case_when(tt_pt <= 15 ~ 1,
                          TRUE ~ 0),
    wij_dockless_60 = case_when(tt_dockless_service <= 60 ~ 1,
                                TRUE ~ 0),
    wij_dockless_45 = case_when(tt_dockless_service <= 45 ~ 1,
                                TRUE ~ 0),
    wij_dockless_30 = case_when(tt_dockless_service <= 30 ~ 1,
                                TRUE ~ 0),
    wij_dockless_15 = case_when(tt_dockless_service <= 15 ~ 1,
                                TRUE ~ 0),
    # improvement caused by dockless service (jobs * sj * (wij_dockless - wij_pt))
    jobs_tt_improve_dockless_pt_60_const = pmax(wij_dockless_60 - wij_pt_60, 0) * C000 * sj,
    jobs_tt_improve_dockless_pt_45_const = pmax(wij_dockless_45 - wij_pt_45, 0) * C000 * sj,
    jobs_tt_improve_dockless_pt_30_const = pmax(wij_dockless_30 - wij_pt_30, 0) * C000 * sj,
    jobs_tt_improve_dockless_pt_15_const = pmax(wij_dockless_15 - wij_pt_15, 0) * C000 * sj)


# --- multiply by origin constraint (si)

improvement_constrained <- improvement_constrained %>%
  # sum the jobs for each travel time threshold
  mutate(
    # multiply jobs for each OD pair by origin constraint    (jobs * si)
    # si can be different for the same origin. It depends on access mode used to each destination
    across(contains("jobs_tt_improve_dockless_pt"), ~ .x * si)) %>%
  # get total values for each origin
  group_by(fromId) %>%
  # sum the jobs for each travel time threshold
  summarize(
    # sum the jobs for each origin and time threshold   
    across(contains("jobs_tt_improve_dockless_pt"), ~ round(sum(.x)))) %>%
  ungroup() 




# change from totals to % of total jobs
improvement_constrained <- improvement_constrained %>% 
  mutate(across(contains("jobs_tt_improve_dockless_pt"),
                ~.x / sum(city_geom[[job_cols]], na.rm = TRUE) * 100, 2)) %>%
  # round
  mutate(across(contains("tt_"), round, 2))


# ----- Add results to original accessibility calculations

access_results <-  st_read(paste0("../data/", city, "/level_i_ii/accessibility_results.geojson"))

# join constrined data
access_results  <- access_results  %>% 
  left_join(improvement_constrained, by = c("cell_id" = "fromId"))

# add frac available for vizualizations
access_results  <- access_results %>% 
  left_join(city_geom %>% 
              st_drop_geometry() %>% 
              select(cell_id, frac_available, frac_available_neighb), 
            by = "cell_id")

# save 
st_write(access_results, paste0("../data/", city, "/level_i_ii/accessibility_results_constrained.geojson"), 
         delete_dsn =TRUE)



# ------ summary statistics

# -- subset to the city of San Francisco
access_results_summary <- access_results %>% 
  st_filter(city_geom_improved, .predicate = st_within)

# -- calculate reduction in jobs accessible due to supply constraints
access_results_summary <- access_results_summary %>% 
  mutate(
    jobs_tt_reduction_const_60 = jobs_tt_improve_dockless_pt_60 - jobs_tt_improve_dockless_pt_60_const,
    jobs_tt_reduction_const_45 = jobs_tt_improve_dockless_pt_45 - jobs_tt_improve_dockless_pt_45_const,
    jobs_tt_reduction_const_30 = jobs_tt_improve_dockless_pt_30 - jobs_tt_improve_dockless_pt_30_const,
    jobs_tt_reduction_const_15 = jobs_tt_improve_dockless_pt_15 - jobs_tt_improve_dockless_pt_15_const)

# --- plot 

# mean value for plot
access_results_mean <- access_results_summary %>%
  # much faster when you remove the geometry
  st_drop_geometry() %>%
  group_by(agency_name) %>%
  summarize(mean_length = mean(trip_length_km)) 


# plot
ggplot(access_results_summary, aes(x = jobs_tt_reduction_const_60)) +
  geom_histogram(binwidth = 2, colour = "white", fill = "grey60") +
  #scale_fill_manual(values = cols) +
  #facet_grid(agency_name ~ .) +
  labs(title="Effect of Supply Constraints on Access to Jobs (60 minutes)",
       x = "Reduction in Jobs Accessible (% of Total Jobs)",
       y = "No. of Zones") + 
  # line representing mean
  geom_vline(data=access_results_summary, 
             aes(xintercept=mean(jobs_tt_reduction_const_60)),
             linetype = "dashed", size = 1, colour = "darkred") +
  theme_minimal()

# save
ggsave(paste0("../data/", city, "/Plots/supply_constraints_histogram_60.png"), width = 10, height = 6)







