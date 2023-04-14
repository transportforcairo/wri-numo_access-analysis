###################################################################################################
###    The purpose of this script is to use the travel time output from r5 to calculate travel  ###
###    times for each mode combination                                                          ###
###################################################################################################

library(tidyverse)
library(sf)
library(tidytable)


#city <- "San Francisco"
#city <- "Minneapolis"
city <- "Mexico City"
#city <- "Cairo"
# ------------------------------------- Read in the data ------------------------------------- #

# Do we have an od matrix using congested speeds: 1: yes, 0:no
od_real_speeds <- 1
# do we have docked service
docked <- 1
# do we have a dockless service 
dockless <- 0

# --- travel times

if(od_real_speeds == 1){
  # travel time matrix with freeflow car speeds
  tt_matrix_city_ff <- vroom::vroom(paste0("../data/", city, "/level_i_ii/travel_time_matrix_freeflow.csv"))
  
  # travel time matrix using edited pbf (more realistic travel speeds)
  tt_matrix_city <- vroom::vroom(paste0("../data/", city, "/level_i_ii/travel_time_matrix.csv"))
  
  
  # --- keep only car columns for freeflow df
  tt_matrix_city_ff <- tt_matrix_city_ff %>% 
    select(fromId, toId, `4_classic`) %>%
    rename_with(~paste0(., "_freeflow"), `4_classic`) 
  
  # merge the two dataframes
  tt_matrix_city <- tt_matrix_city %>% 
    left_join(tt_matrix_city_ff, by = c("fromId", "toId"))
  
  rm(tt_matrix_city_ff)
  
}else{
  # travel time matrix with freeflow car speeds
  tt_matrix_city <- vroom::vroom(paste0("../data/", city, "/level_i_ii/travel_time_matrix_freeflow.csv"))
}

# convert to lazy datatable to take advantage of datatable speed gains
tt_matrix_city <- as_tidytable(tt_matrix_city)


# --- city geometry 
city_geom <- st_read(paste0("../data/", city, "/level_i_ii/zones_w_micromobility_providers.geojson"))

# select the micromobility columns only 
if(city == "San Francisco"){
  city_geom <- city_geom %>% select(cell_id, pop_totalE, `C000`, #contains("dock"),
                                    docked_service, dockless_service,
                                    #supply constraint columns 
                                    frac_available, frac_available_neighb) %>% 
    rename(population = pop_totalE)
} else if(city == "Minneapolis"){
  city_geom <- city_geom %>% select(cell_id, pop_totalE, `C000`, #contains("dock"),
                                    docked_service, dockless_service) %>% 
    rename(population = pop_totalE)
  
} else if(city ==  "Mexico City"){
    city_geom <- city_geom %>% select(cell_id, pobtot, jobs, contains("dock")) %>% 
      rename(population = pobtot)
} else if(city ==  "Cairo"){
  city_geom <- city_geom %>% select(cell_id, pop_2018_c, jobs_total, contains("dock")) %>% 
    rename(population = pop_2018_c)
}

# add population density column - parking time will be informed by this value
city_pop_density  <- city_geom %>% 
  st_transform(3857) %>%
  mutate(pop_density = population / as.numeric(st_area(.)/1000000)) %>%   # people / km^2
  st_transform(4326) %>% 
  select(cell_id, pop_density) %>%
  st_drop_geometry()

city_pop_density <- as_tidytable(city_pop_density)

# ------------------------------------- Combine data into one layer ------------------------------------- #

# Join travel time results to zone layer 
tt_matrix_city <- tt_matrix_city %>% 
  # right join is just so we can preserve a nice column order
  inner_join.(city_geom %>% st_drop_geometry(), by = c("fromId" = "cell_id")) %>%
  # add suffix to micromobility (and supply constraint if applicable) columns to indicate that they refer to the origin zone
  rename_with.(.fn = ~ paste0(.x, "_o"), .cols = contains(c("dock", "frac_available"))) 

# free up space
gc()

# --- add population density column - parking time will be informed by this value

tt_matrix_city <- tt_matrix_city %>%
  # add pop density in destination zone
  left_join.(city_pop_density, by = c("toId" = "cell_id")) %>%
  # add suffix to pop_density column to indicate that it refers to the destination zone
  rename_with.(.fn = ~ paste0(.x, "_d"), .cols = "pop_density")

# --- create df with data on micromobility availability at destination zones & join it to the original data

# create the df
micro_dest <- city_geom %>% 
  st_drop_geometry() %>%
  # select the micromobility columns only (and supply constraint if applicable)
  select(cell_id, contains(c("dock", "frac_available"))) %>% 
  # add suffix to micromobility columns to indicate that they refer to the destination zone
  rename_with(.fn = ~ paste0(.x, "_d"), .cols = contains(c("dock", "frac_available")))  

# join onto original layer
tt_matrix_city <- tt_matrix_city %>% 
  left_join.(micro_dest, by = c("toId" = "cell_id")) 



# ------------------------------------- Determine micromobility availability per OD ------------------------------------- #


# Information on each provider, used to calculate travel times for the micromobility combination

mm_columns <- tribble(
  ~city, ~provider_name, ~electric, 
  # San Francisco
  "San Francisco", "docked_service" ,  FALSE, # docked_Bay.Wheels
  "San Francisco", "dockless_service",  TRUE, # dockless_Bird.San.Francisco
  #"San Francisco", "dockless_Spin.San.Francisco",  TRUE,
  # Minneapolis
  "Minneapolis", "docked_service",  TRUE,  # docked_Lyft.Scooters.Minneapolis
  "Minneapolis", "dockless_service",  TRUE,  # Nice.Ride.Minnesota
 # "Minneapolis", "dockless_Spin.Minneapolis",  TRUE,
  # Mexico City
  "Mexico City", "docked_service",  FALSE,   # docked_ECOBICI
  # Cairo
  "Cairo", "docked_service",  FALSE   # docked_ECOBICI
)

# Function that looks at availability of micromobility at origin and destination and summarizes the information in 
# one column PER PROVIDER:

#   none:  no micromobility at origin or destination
#   first: micromobility at origin only 
#   last:  micromobility at destination only 
#   both:  micromobility at both origin and destination

micromobility_options <- function(data, columns, chosen_city) {
  # i'm not sure when to use sym() and when to use ensym(): it is inspired by 
  # the rlang response here: https://www.reddit.com/r/rstats/comments/8jpykf/how_to_take_difference_of_paired_columns_in_dplyr/
  
  # keep only the data for the city we are analyzing 
  columns_city <- columns %>% filter(city == chosen_city)
  
  for(i in 1:nrow(columns_city)){
    # specify the provider we are looking at
    column = sym(columns_city$provider_name[i])
    # determine origin column name
    origin <- stringr::str_c(columns_city$provider_name[i], "_o")
    origin <- ensym(origin)
    # determine destination column name
    destination <- stringr::str_c(columns_city$provider_name[i], "_d")
    destination <- ensym(destination)
    
    # create a new column that identifies the availability of this provider in relation to the OD pair
    data <- data %>% mutate(!! column := case_when(
      !! origin == 0 & !! destination == 0 ~ "none",
      !! origin == 0 & !! destination >= 1 ~ "last",
      !! origin >= 1 & !! destination == 0 ~ "first",
      !! origin >= 1 & !! destination >= 1 ~ "both"))
  }
  
  return(data)
}

# run the function
tt_matrix_city <- micromobility_options(data = tt_matrix_city, 
                                        columns = mm_columns, 
                                        chosen_city = city)




# ------------------------------------- Get travel times for micromobility ------------------------------------- #

# This function looks at the availability of micromobility, and chooses the appropriate travel time. The travel times 
# are for the combination that includes micromobility
# --- LOGIC:

# We have the following combinations
# combination         mode                        egress_mode 

#   1          WALK;TRANSIT                       WALK            
#  2.1         WALK;TRANSIT;BICYCLE               WALK           
#  2.2         WALK;TRANSIT                       BICYCLE 
#  2.3         WALK;TRANSIT;BICYCLE               BICYCLE       
#  3           BICYCLE WALK                       WALK   

# IF MM exists at origin AND destination: choose the fastest travel time from all combinations
# IF: MM exists at origin only: choose the fastest travel time from 1 and 2.1
# IF: MM exists at destination only: choose the fastest travel time from 1 and 2.2
# IF: MM does not exist at origin or destination: choose combination 1 


# The function adds a travel time column FOR EACH PROVIDER
micromobility_tt <- function(data, columns, chosen_city) {
  # data: the dataframe we are applying the function to
  # columns: the columns with the micromobility provider names
  columns_city <- columns %>% filter(city == chosen_city)
  
  for(i in 1:nrow(columns_city)){
    # specify the provider we are looking at
    column = sym(columns_city$provider_name[i])
    new_column = str_c("tt_", columns_city$provider_name[i])
    new_column = sym(new_column)
    # column that specifies whether micromobility improved travel time for this od pair (compared to pt)
    improved = str_c(columns_city$provider_name[i], "_faster")
    improved = sym(improved)
    # create a new column that identifies the availability of this provider in relation to the OD pair
    # the travel time differs depending on whether the service is electric
    if(columns_city$electric[i] == TRUE){
      data <- data %>% 
        mutate(!! new_column := case_when(
          !! column == "none" ~ `1_electric`,
          !! column == "first" ~ pmin(`1_electric`, `2.1_electric`),
          !! column == "last" ~ pmin(`1_electric`, `2.2_electric`),
          !! column == "both" ~ pmin(`1_electric`, `2.1_electric`, `2.2_electric`, `2.3_electric`, `3_electric`)),
          # has micromobility improved travel time?
          !! improved := case_when(
            !! new_column < `1_electric` ~ "yes",
            !! new_column >= `1_electric` ~ "no")
          )
    } else{
      data <- data %>% 
        mutate(!! new_column := case_when(
          !! column == "none" ~ `1_classic`,
          !! column == "first" ~ pmin(`1_classic`, `2.1_classic`),
          !! column == "last" ~ pmin(`1_classic`, `2.2_classic`),
          !! column == "both" ~ pmin(`1_classic`, `2.1_classic`, `2.2_classic`, `2.3_classic`, `3_classic`)),
          # has micromobility improved travel time?
          !! improved := case_when(
            !! new_column < `1_classic` ~ "yes",
            !! new_column >= `1_classic` ~ "no")
          )
      
    }
  }
  
  return(data)
}


# apply the function
tt_matrix_city <- micromobility_tt(data = tt_matrix_city, 
                                   columns = mm_columns, 
                                   chosen_city = city)

# add a column showing the fastest time by mm (compare docked/dockless and choose the faster one)
if("tt_docked_service" %in% colnames(tt_matrix_city) && "tt_dockless_service" %in% colnames(tt_matrix_city)){
  tt_matrix_city <- tt_matrix_city %>%
    mutate(tt_docked_dockless_best = pmin(`tt_docked_service`, `tt_dockless_service`, na.rm = TRUE))
}


# ------------------------------------- Add Parking time for Car Travel (Based on Pop Density) ------------------------------------- #

# Two parking time functions

# 1. Parking time based on population density threshold 

# This function adds a parking time. We provide two static values (one for high density and one for low density areas)
# We calculate the nth percentile for population density, and assigns a parking time to each zone depending on whether 
# the population density of the zone is higher or lower than the calculating percentile

# Output: Input layer with an additional parking time value
parking_time_binary = function(layer, pop_column,
                               parking_time_low_density = 2, 
                               parking_time_high_density = 10,
                               # The nth percentile of population density variable e.g. 0.1, 0.3, 0.75 etc
                               parking_time_percentile = .75){   
  # to reference the column in the function
  pop_column <- sym(pop_column)
  # calculate the population density threshold
  density_threshold <- as.numeric(quantile(layer[[pop_column]], parking_time_percentile))
  # add parking time based on population density and calculated population density threshold
  layer <- layer %>% mutate(parking_time_car = case_when(
    !! pop_column <= density_threshold ~ parking_time_low_density,
    !! pop_column > density_threshold ~ parking_time_high_density
  ))
  return(layer)
}   

# 2. Parking time based on population density distribution

# We calculate which population density percentile each zone is in, and then use that to determine a parking time:
      # 1. set the lowest and highest parking time values
      # 2. calculate which population density percentile each zone is in
      # 3. parking time = (percentile * (highest - lowest)) + lowest

# Output: Input layer with an additional parking time value
parking_time_distribution = function(layer, 
                                     pop_column,
                                     parking_time_lowest = 2, 
                                     parking_time_highest = 10){   
  # to reference the column in the function
  pop_column <- sym(pop_column)
  
  # add parking time based on population density and calculated population density threshold
  layer <- layer %>% 
    mutate(
      # get cumulative distribution (i.e what percentile does each value fall in)
      cd = cume_dist(!! pop_column), 
      # calculate parking time by fitting values based on percentile and highest/lowest values
      parking_time = round((cd * (parking_time_highest - parking_time_lowest)) + parking_time_lowest)) %>%
    selct(-cd)
  
  return(layer)
}   

tt_matrix_city <- parking_time_binary(layer = tt_matrix_city, 
                         pop_column = "pop_density_d",
                         parking_time_low_density = 3,
                         parking_time_high_density = 10,
                         parking_time_percentile = .75)

# tt_matrix_city <- parking_time_distribution(layer = tt_matrix_city, 
#                                pop_column = "pop_density_d",
#                                parking_time_lowest = 3, 
#                                parking_time_highest = 10)

# ------------------------------------- Get travel time for all scenarios ------------------------------------- #

# uncomment freeflow columns if we have it 
tt_matrix_city <- tt_matrix_city %>% mutate(
  # car
  tt_car = pmax(`4_classic`, `4_electric`),
  tt_car_freeflow = `4_classic_freeflow`,
  # car with parking time
  tt_car_parking = pmax(`4_classic`, `4_electric`) + parking_time_car,
  tt_car_parking_freeflow = `4_classic_freeflow` + parking_time_car,
  # car with parking time + access and egress
  tt_car_parking_acc_egr = pmax(`4_classic`, `4_electric`) + parking_time_car + 2 + 2,
  tt_car_parking_acc_egr_freeflow = `4_classic_freeflow` + parking_time_car + 2 + 2,
  # public transport only 
  tt_pt = pmax(`1_classic`, `1_electric`),
  # public transport + classic bicycle (as access mode or main mode, not egress))
  tt_pt_bicycle_c = pmin(`2.1_classic`, `3_classic`),
  # public transport + electric bicycle (as access mode or main mode, not egress)
  tt_pt_bicycle_e = pmin(`2.1_electric`, `3_electric`),
  # classic bicycle
  tt_bicycle_c = `3_classic`,
  # electric bicycle
  tt_bicycle_e = `3_electric`) 

# how much has micromobility improved travel time relative to pt
if(dockless == 1){
  tt_matrix_city <- tt_matrix_city %>%
    mutate(
      # improvement in minutes
      tt_improve_docked_pt = pmax(tt_pt - tt_docked_service, 0), # docked
      tt_improve_dockless_pt = pmax(tt_pt - tt_dockless_service, 0), # dockless
      tt_improve_dock_dockless_pt = pmax(tt_pt - tt_docked_dockless_best, 0), # fastest of dockless - docked
      # improvement as % (travel time decrease)
      tt_improve_docked_pt_perc = (tt_improve_docked_pt / tt_pt) * 100, # docked
      tt_improve_dockless_pt_perc = (tt_improve_dockless_pt / tt_pt) * 100, # dockless
      tt_improve_dock_dockless_pt_perc = (tt_improve_dock_dockless_pt / tt_pt) * 100) # fastest of dockless - docked
  
} else if(dockless == 0){
  tt_matrix_city <- tt_matrix_city %>%
    mutate(
      # improvement in minutes
      tt_improve_docked_pt = pmax(tt_pt - tt_docked_service, 0), # docked
      # improvement as % (travel time decrease)
      tt_improve_docked_pt_perc = (tt_improve_docked_pt / tt_pt) * 100)
}

# remove unnecessary columns 

if(od_real_speeds == 1){
  tt_matrix_city <- tt_matrix_city %>% 
    select(!(ends_with("service_o"))) %>% select(!(ends_with("service_d"))) %>%
    select(!(ends_with("density_d"))) %>% 
    select(!(`1_classic`:`4_classic_freeflow`)) 
}else if(od_real_speeds == 0){
  tt_matrix_city <- tt_matrix_city %>% 
    select(!(ends_with("service_o"))) %>% select(!(ends_with("service_d"))) %>%
    select(!(ends_with("density_d"))) %>% 
    #select(!(`1_classic`:`4_classic_freeflow`))
    select(!(`1_classic`:`5_electric`)) 
  
}
                           
# ------------------------------- save 

# st_write(tt_matrix_city, paste0("../data/", city, "/level_i_ii/travel_time_matrix.geojson"), delete_dsn = TRUE)
write_csv(tt_matrix_city, paste0("../data/", city, "/level_i_ii/travel_time_matrix_w_metadata.csv"))


# ------------------------------- Desire lines of improved travel times 

# select necessary columns

if(dockless == 1){
  tt_matrix_od <- tt_matrix_city %>% 
    select(fromId, toId, docked_service, dockless_service, tt_docked_service, tt_dockless_service, tt_pt, 
           tt_improve_docked_pt, tt_improve_docked_pt_perc,  # docked improvement
           tt_improve_dockless_pt, tt_improve_dockless_pt_perc, # dockless improvement
           tt_improve_dock_dockless_pt, tt_improve_dock_dockless_pt_perc) # best improvement (considering both docked & dockless)
  
} else if(dockless == 0){
  tt_matrix_od <- tt_matrix_city %>% 
    select(fromId, toId, docked_service, tt_docked_service, tt_pt, 
           tt_improve_docked_pt, tt_improve_docked_pt_perc) # docked improvement
}

# prepare layer with zone centroids
city_geom_od <- city_geom %>% st_centroid()

# create desire lines
tt_matrix_od_desire = od::od_to_sf(tt_matrix_od, city_geom_od)
# add distance column
tt_matrix_od_desire <- tt_matrix_od_desire %>%
  mutate(distance_straight = st_length(.))

# filter because this is such a big dataset

if(dockless == 1){
  tt_matrix_od_desire <- tt_matrix_od_desire %>% 
    filter((tt_improve_docked_pt > 25 & tt_improve_docked_pt_perc > 30) |
             (tt_improve_dockless_pt > 25 & tt_improve_dockless_pt_perc > 30))
  
} else if(dockless == 0){
  tt_matrix_od_desire <- tt_matrix_od_desire %>% 
    filter(tt_improve_docked_pt > 25 | tt_improve_docked_pt_perc > 30)
}


# save
st_write(tt_matrix_od_desire, paste0("../data/", city, "/level_i_ii/tt_improvement_desire_lines.geojson"), delete_dsn = TRUE)


# plot

# reasonable size
tt_matrix_od_plot <- tt_matrix_od_desire %>% 
  arrange(desc(tt_improve_docked_pt)) %>%
  head(2500)

tm_shape(city_geom) +
  tm_borders(col = "grey80", 
             lwd = 1, 
             alpha = 0.5) +
  tm_shape(tt_matrix_od_plot) +
  tm_lines(title.col = "Travel time improvement (minutes)",
           lwd = "tt_improve_docked_pt",
           col= "tt_improve_docked_pt",
           palette = "YlOrRd",
           style = "pretty",
           scale = 1,
           alpha = 0.3,
           legend.col.is.portrait = FALSE,
           legend.lwd.show = FALSE) +   # remove lineweight legend) 
  tm_layout(fontfamily = 'Georgia',
            frame.lwd = NA,    # remove facet title frames
            legend.outside = TRUE,
            legend.outside.position = 'bottom',
            frame = FALSE) +
  tm_scale_bar(color.dark = "gray60") + 
  tm_compass(type = "arrow",
             size = 1,
             show.labels = 0,
             position = c("right","top"),
             color.dark = "gray60") -> p
p

