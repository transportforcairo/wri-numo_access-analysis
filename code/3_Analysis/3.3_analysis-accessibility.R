###################################################################################################
###    The purpose of this script is to calculate cumulative accessibility at the zone level    ###
###    based on the travel time calculations                                                    ###
###################################################################################################

library(tidyverse)
library(sf)
library(tmap)


# ----------------------------------------- Load in the data ----------------------------------------- #

# we use this table as a reference when running code for a specific city
city_metadata <- tribble(
  ~city, ~job_cols, ~dockless, 
  # San Francisco
  "San Francisco", c("C000"),  1, 
  # Minneapolis
  "Minneapolis", c("C000"),  1,  
  # Mexico City
  "Mexico City", "jobs",  0,  
  # Cairo
  "Cairo", "jobs_total",  0   
)

# do we have a dockless service 
dockless <- city_metadata$dockless[city_metadata$city == city]

# geometry data
city_geom <- st_read(paste0("../data/", city, "/level_i_ii/zones_w_micromobility_providers.geojson"))

# travel time data
city_tt <- vroom::vroom(paste0("../data/", city, "/level_i_ii/travel_time_matrix_w_metadata.csv"))

# select only the travel time columns (and supply constraint if applicable)
city_tt <- city_tt %>% select(fromId, toId, contains(c("tt_", "frac_", "_service"))) #%>%
  # remove travel time improvement columns
  #select(!(contains("improve")))

# # replace na travel time values with very big values
city_tt <- city_tt %>%
  mutate(across(contains("tt_"), ~replace_na(., 1000)))

# ----------------------------------------- Calculate Accessibility ----------------------------------------- #
# define job column(s)
job_cols <- city_metadata$job_cols[city_metadata$city == city]

# Add destination jobs to travel time matrix #
city_tt <- city_tt %>% 
  left_join(city_geom %>% st_drop_geometry() %>% select(cell_id, all_of(job_cols)), by = c("toId" = "cell_id"))

# accessibility


# ----- Function to calculate accessibility from travel time matrix

# https://stackoverflow.com/questions/50925734/dplyr-use-a-custom-function-in-summarize-after-group-by

# get_access = function(layer, job_col, cutoff_time = 60, add_suffix = TRUE){
#   
#   # Input: 
#     # layer with multiple travel time columns (1 per mode)
#     # Job column to use when calculating accessibility
#     # cutoff_time: travel time threshold for cumulative accessibility measure
#     # add_suffix: add _{cutoff_time} to accessibility columns
#   # Output:
#     # Jobs accessible for each mode combination
#   
#   #job_col <- sym(job_col)
#   
#   layer_access  <- layer  %>%
#     group_by(fromId) %>%
#     summarize(across(contains("tt_"),
#                      #summarize(across(`car`:`bicycle_electric`,  # columns to apply function on
#                      ~ sum({{job_col}}[.x <= cutoff_time], na.rm = TRUE), # the function to apply
#                      .names = "jobs_{.col}")) %>%  # names of new columns 
#     ungroup() 
#   # add a suffix to the accessibility columns
#   if(add_suffix == TRUE){
#     layer_access <- layer_access %>%
#       rename_with(.fn = ~ paste0(.x, "_", as.character(cutoff_time)), 
#                   .cols = contains("tt_"))
#     
#   }
#   
#   
#   return(layer_access)
#   
# }
# 
# # apply the function
# 
# # all jobs
# city_tt_access_60 <- get_access(layer = city_tt, 
#                                 job_col = `jobs`,  # C000  # put it in single brackets `` not double
#                                 cutoff_time = 60,
#                                 add_suffix = TRUE)


get_access_multiple = function(layer, job_col, cutoff_times = c(60), add_suffix = TRUE){
  
  # Input: 
    # layer with multiple travel time columns (1 per mode)
    # Job column to use when calculating accessibility
    # cutoff_times: (multiple) travel time threshold(s) for cumulative accessibility measure
  # add_suffix: add _{cutoff_time} to accessibility columns
  # Output:
    # Jobs accessible for each mode combination an travel time (one column for each cutoff time and mode combination)
  
  # list to store results of each cutoff time
  results <- vector(mode = "list", length = length(cutoff_times))
  # loop over all cutoff times
  for(i in 1:length(cutoff_times)){
    
    layer_access  <- layer  %>%
      group_by(fromId) %>%
      summarize(across(contains("tt_"),
                       ~ sum({{job_col}}[.x <= cutoff_times[i]], na.rm = TRUE), # the function to apply
                       .names = "jobs_{.col}")) %>%  # names of new columns 
      ungroup() 
    # add a suffix to the accessibility columns
    layer_access <- layer_access %>%
      rename_with(.fn = ~ paste0(.x, "_", as.character(cutoff_times[i])), 
                  .cols = contains("tt_"))
    # store in list
    results[[i]] <- layer_access
    
  }
  # turn list of dataframes into 1 long dataframe
  # https://stackoverflow.com/questions/67784159/column-bind-several-list-elements-based-on-id-variable
  results <-  purrr::reduce(results, full_join, by='fromId')
  
  
  return(results)
  
}

# all jobs
city_tt_access <- get_access_multiple(layer = city_tt,
                                      #job_col = city_metadata$job_cols[city_metadata$city == city],
                                      #job_col = `C000`,  # USA # put it in single brackets `` not double
                                      job_col = `jobs`,  # Mexico City 
                                      #job_col = `jobs_total`, # Cairo
                                      cutoff_times = c(15, 30, 45, 60),
                                      add_suffix = TRUE)

# # Number of jobs for workers with Educational Attainment: Some college or Associate degree
# city_tt_access <- get_access(layer = city_tt_jobs, job_col = `CD03`, cutoff_time = 60)


# LODES data columns https://lehd.ces.census.gov/data/lodes/LODES7/LODESTechDoc7.5.pdf
# CNS.. Number of jobs disaggregated by NAICS sector ..
# CD.. Number of jobs disaggregated by Educational Attainment of workers ..
# CE.. Number of jobs disaggregated by level of earning


# add columns showing how much the addition of mm improves accessibility
mm_access_improvement <- function(data, cutoff_times, dockless){
  # This function calculates the improvement in accessibility due to micromobility for each Origin
  # The benchmark is accessibility by pt
  # Input:
  # data: dataset with accessibility results for each origin and mode combination
  # cutoff_times: the different cut off times that we are calculating improvement for. Must match the values in data
  # dockless: 0/1 to indicate whether we have a dockless system
  
  for(i in 1:length(cutoff_times)){
    # micromobility + publli transport travel time column
    mm_docked_tt_column = str_c("jobs_tt_docked_service_", as.character(cutoff_times[i]))
    mm_docked_tt_column = sym(mm_docked_tt_column)
    # public transport travel time column
    pt_tt_column = str_c("jobs_tt_pt_", as.character(cutoff_times[i]))
    pt_tt_column = sym(pt_tt_column)
    
    improve_docked_column = str_c("jobs_tt_improve_docked_pt_", as.character(cutoff_times[i]))
    improve_docked_column = sym(improve_docked_column)
    
    if(dockless == 1){
      mm_dockless_tt_column = str_c("jobs_tt_dockless_service_", as.character(cutoff_times[i]))
      mm_dockless_tt_column = sym(mm_dockless_tt_column)
      
      improve_dockless_column = str_c("jobs_tt_improve_dockless_pt_", as.character(cutoff_times[i]))
      improve_dockless_column = sym(improve_dockless_column)
      
      data <- data %>% 
        mutate(!! improve_docked_column := pmax(!! mm_docked_tt_column - !! pt_tt_column, 0),
               !! improve_dockless_column := pmax(!! mm_dockless_tt_column - !! pt_tt_column, 0))
      
    } else{
      data <- data %>% 
        mutate(!! improve_docked_column := pmax(!! mm_docked_tt_column - !! pt_tt_column, 0))
    }
    
    print(paste0("added columns for cutoff time = ", cutoff_times[i]))
  }
  return(data)
}

city_tt_access <- mm_access_improvement(data = city_tt_access,
                                        cutoff_times = c(15, 30, 45, 60),
                                        dockless = dockless)



# join spatial layer onto accessibility calculations
city_tt_access <- city_geom %>% select(cell_id) %>%
  left_join(city_tt_access, by = c("cell_id" = "fromId")) 


# exploratory plots 
plot(city_tt_access["jobs_tt_car_freeflow_15"])
plot(city_tt_access["jobs_tt_car_freeflow_30"])
plot(city_tt_access["jobs_tt_pt_15"])
plot(city_tt_access["jobs_tt_pt_45"])
plot(city_tt_access["jobs_tt_improve_docked_pt_45"])
plot(city_tt_access["jobs_tt_improve_dockless_pt_60"])

# remove % columns (they are incorrect after accumulation). 
# TODO: keep column and use it as a sanity check for the results
city_tt_access <- city_tt_access %>% select(!contains("_perc_"))

# save 
st_write(city_tt_access, paste0("../data/", city, "/level_i_ii/accessibility_results.geojson"), 
         delete_dsn =TRUE)
# ----------------------------------------- Plot Results ----------------------------------------- #

# select necessary columns for plotting
city_tt_access_plot <- city_tt_access %>% 
  select(c(cell_id,
           contains(c("tt_pt", "bicycle", "dock", "jobs_tt_car_parking_acc_egr"))))


# long format for tmap facet plot
city_tt_access_long <- city_tt_access_plot %>% 
  #pivot_longer(cols = `jobs_car`:`jobs_bicycle_electric`) %>% 
  pivot_longer(cols = contains("jobs_tt_")) %>% 
  st_as_sf()

# tmap plots

# ----- comparing PT -   PT + micromobility

columns_p1 <- c("jobs_tt_pt_45", "jobs_tt_docked_service_45", "jobs_tt_improve_docked_pt_45")

city_tt_access_long_p1 <- city_tt_access_long %>%
  filter(name %in% columns_p1) %>%
  # divide by 1 million for plotting purposes
  mutate(
    value = value / 1000000,
    name = factor(name, levels=columns_p1))


tm_shape(city_tt_access_long_p1) +
  tm_fill(col = "value",
          style = "cont",
          palette = "-Spectral",
          title = "Number of Job Opportunities (millions)",
          legend.is.portrait = FALSE) +
  tm_facets(by="name",
            nrow = 1,
            free.coords=FALSE)  +  # so that the maps aren't different sizes 
  tm_layout(main.title = "Accessibility - Different Scenarios",        
            main.title.size = 1.2,
            main.title.color = "azure4",
            main.title.position = c("left", "top"),
            fontfamily = 'Georgia',
            legend.text.size = 0.5,
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            frame = FALSE,
            # panel.labels = c("Public Transport", "Public Transport + Micromobility", 
            #                  "Improvement due \nto Micromobility")) +
            panel.labels = LETTERS[1:3]) +
  tm_scale_bar(color.dark = "gray60") -> p1
p1


#tmap_save(tm = p1, filename = paste0("../data/", city,"/Plots/accessibility_pt_micrommobility.png"))


# Improvement caused by micromobility
columns_p2 <- c("jobs_tt_improve_docked_pt_45")

city_tt_access_long_p2 <- city_tt_access_long_p1 %>%
  filter(name %in% columns_p2) 



tm_shape(city_tt_access_long_p2) +
  tm_fill(col = "value",
          style = "cont",
          palette = "-Spectral",
          title = "Number of Job \nOpportunities (millions)",
          legend.is.portrait = FALSE) +
  # tm_facets(by="name",
  #           nrow = 1,
  #           free.coords=FALSE)  +  # so that the maps aren't different sizes
  tm_layout(main.title = "Accessibility Increase Due \nto Micromobility",        
            main.title.size = 1.2,
            main.title.color = "azure4",
            main.title.position = c("left", "top"),
            fontfamily = 'Georgia',
            #legend.text.size = 0.7,
            legend.position = c("left", "bottom"),
            # legend.outside = TRUE,
            frame = FALSE) +
  tm_scale_bar(color.dark = "gray60") -> p2
p2

#tmap_save(tm = p2, filename = paste0("../data/", city,"/Plots/accessibility_improvement_mm.png"))




# car only (freeflow vs congested)
city_tt_access_long_car <- city_tt_access_long %>% filter(stringr::str_detect(name, "car")) %>%
  # simplify names for plotting
  mutate(name = str_replace(name, pattern = "tt_car_parking_acc_egr_", replacement = ""))

tm_shape(city_tt_access_long_car) +
  tm_fill(col = "value",
          style = "cont",
          title = "Number of Job \nOpportunities (millions)",
          palette = "-Spectral") +
  tm_facets(by="name",
            nrow = 2,
            free.coords=FALSE)  +  # so that the maps aren't different sizes 
  tm_layout(main.title = "Accessibility By Car",        
            main.title.size = 1.2,
            main.title.color = "azure4",
            main.title.position = c("left", "top"),
            fontfamily = 'Georgia',
            legend.position = c("right", "bottom"),
            frame = FALSE) -> p3

p3

tmap_save(tm = p3, filename = paste0("../data/", city,"/Plots/accessibility_car_facet.png"))


# car only (effect of parking + access / egress)

city_tt_access_long_car2 <- city_tt_access %>% 
  select(c(cell_id, contains("jobs_tt_car"))) %>%
  select(!(contains("freeflow"))) %>%
  pivot_longer(cols = contains("jobs_tt_")) %>% 
  st_as_sf()
  
# keep one travel time only
city_tt_access_long_car2 <- city_tt_access_long_car2 %>%
  filter(stringr::str_detect(name, "30")) 

# plot  
  
 
tm_shape(city_tt_access_long_car2) +
  tm_fill(col = "value",
          style = "cont",
          title = "Number of Job \nOpportunities (millions)",
          palette = "-Spectral") +
  tm_facets(by="name",
            nrow = 1,
            free.coords=FALSE)  +  # so that the maps aren't different sizes 
  tm_layout(main.title = "Accessibility By Car",        
            main.title.size = 1.2,
            main.title.color = "azure4",
            main.title.position = c("left", "top"),
            fontfamily = 'Georgia',
            legend.position = c("right", "bottom"),
            frame = FALSE) -> p4

p4

tmap_save(tm = p4, filename = paste0("../data/", city,"/Plots/accessibility_car_parking_acc_egr.png"))

