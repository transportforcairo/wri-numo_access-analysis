###################################################################################################
###    The purpose of this script is to create a variable resolution hexagon grid layer. The    ###
###    Resolution chosen for each sub-area in a study area is based on the density of jobs /    ###
###    population                                                                               ###
###################################################################################################

library(sf)
library(tidyverse)

# --------------------------------- PRPARE THE INPUT DATA --------------------------------- #

# city <- "San Francisco"
city <- "Minneapolis"
layer_name <- "census_bg.geojson"

# # Mexico City
# city <- "Mexico City"
# layer_name <- "census_pop_jobs.geojson"

# Cairo
# city <- "Cairo"
# layer_name <- "gcr_shiyakhas_with_jobs.geojson"


# Read in the census layer
city_geom <- st_read(paste0("../data_raw/", city, "/level_i/", layer_name)) %>%
  # we need a metric crs to create the grid layer
  st_transform(3857) %>%
  st_make_valid()

# check that columns are numeric and convert if not
glimpse(city_geom)
if(city == "Mexico City"){
  city_geom <- city_geom %>% mutate(across(pobtot:pobfem, ~as.numeric(.)))
}
glimpse(city_geom)

# Define the hexagon diameters

grid_sizes = c(400, 700, 1000, 1300, 1600, 1900, 2200, 2500)


# --------------------------------- CREATE VARIABLE HEXGRID --------------------------------- #

# ----- 1. Create grids with different resolution

create_grids <- function(study_area, grid_sizes){
  
  # Function to create multiple hexgrids for a study area boundary 
  # Inputs
  #   study area: spatial layer of study area
  #   grid sizes: a vector whose length = the number of layers to create, and each value represents the grid diameter
  #               eg. grid_sizes <- c(400, 600, 800, 1000, 2000)
  # Output
  #   list of spatial layers. Each one is accessed through results[[i]]
  
  # list to store results in. Each list element will be a one column dataframe
  results <- vector(mode = "list", length = length(grid_sizes))
  # loop over the vector and create a grid
  for(i in 1:length(grid_sizes)){
    # unique identifier for grid. This is the name of the column id. It includes the grid diameter
    col_name <- sym(paste0("cell_id_", grid_sizes[i]))
    # --- create the grid
    study_area_grid <- st_make_grid(study_area, cellsize = grid_sizes[i], square = FALSE) %>% 
      st_as_sf() %>% st_make_valid() %>%
      # crop to study area boundaries 
      st_filter(study_area, .predicate = st_intersects) %>%
      # add unique id for each row
      mutate(cell_id = row_number()) %>%
      # rename the column id
      rename(!!col_name := cell_id)
    
    # add dataframe to list
    results[[i]] <- study_area_grid
    
  }
  return(results)
}

# apply the function
results <- create_grids(study_area = city_geom, 
                        grid_sizes = grid_sizes)

# ----- 2. Project population from census layer to hexagon grids

# old
# project_jobs = function(census_layer, hexagon_layers, existing_job_column){
#   
#   # Function to project population from census layer to hexagon layers. It also adds a threshold column 
#   # to determine whether to keep this hexagon, or replace it with smaller hexagons
#   
#   # Input:
#     # census_layer: original layer with population column
#     # hexagon_layers: a list where each item is a spatial hexagon layer
#     # existing_job_column: the name of the column with job information
# 
#   
#   existing_job_column <- sym(existing_job_column)
#   
#   # list to store results in. Each list element will be a one column dataframe
#   projected_results <- vector(mode = "list", length = length(hexagon_layers))
#   # iterate over all hexagon layers
#   for(i in 1:length(hexagon_layers)){
#     
#     # --- add an "area" column i order to get the ratio between the census polygon and the hexagon after intersecting
#     hexagon_layers[[i]] <- hexagon_layers[[i]] %>% 
#       mutate(area_km = as.numeric(st_area(.)/1000000))
#     
#     # --- project data from the census layer to the hexagon layer
#     # determine variable column to group_by
#     column <- colnames(hexagon_layers[[i]])[1] #cell_id_<resolution>
#     # job column name 
#     job_column <- stringr::str_c("jobs_", column)
#     # turn into symbols
#     column = sym(column)
#     job_column <- sym(job_column)
#     
#     # --- project data from the census layer to the hexagon layer
#     projected_layer <- hexagon_layers[[i]] %>% st_intersection(census_layer) %>% # get intersection area with each polygon
#       mutate(area_int = as.numeric(st_area(.)/1000000)) %>%    # get the area of each intersected polygon 
#       # calculate the ratio between the hexagon area of intersection and the area of the census block that it intersected
#       # multiply the ratio by the census block statistic to assign a value to the intersected area
#       mutate(!!job_column := !!existing_job_column * (area_int/area_km)) %>% 
#       # Some hexagons may be split between different census blocks, so we sum the results of the previous 
#       # step on the hexagon level
#       group_by(!!column) %>%
#       summarise(!!job_column := round(sum(!!job_column, na.rm = TRUE))) %>%  ungroup() 
#     
#     projected_results[[i]] <- projected_layer
#     
#     print(paste0("finished projecting layer ", i))
#     
#   }
#   return(projected_results)
# }

project_pop = function(census_layer, hexagon_layers, existing_pop_column){
  
  # Function to project population from census layer to hexagon layers. It also adds a threshold column 
  # to determine whether to keep this hexagon, or replace it with smaller hexagons
  
  # Input:
  # census_layer: original layer with population column
  # hexagon_layers: a list where each item is a spatial hexagon layer
  # existing_pop_column: the name of the column with population information
  
  existing_pop_column <- sym(existing_pop_column)
  
  # --- add an "area" column i order to get the ratio between the census polygon and the hexagon after intersecting
  census_layer <- census_layer %>% 
    mutate(area_km = as.numeric(st_area(.)/1000000))
  
  # list to store results in. Each list element will be a one column dataframe
  projected_results <- vector(mode = "list", length = length(hexagon_layers))
  # iterate over all hexagon layers
  for(i in 1:length(hexagon_layers)){
    # --- project data from the census layer to the hexagon layer
    # determine variable column to group_by
    column <- colnames(hexagon_layers[[i]])[1] #cell_id_<resolution>
    # job column name 
    pop_column <- stringr::str_c("pop_", column)
    # turn into symbols
    column = sym(column)
    pop_column <- sym(pop_column)
    
    # --- project data from the census layer to the hexagon layer
    projected_layer <- hexagon_layers[[i]] %>% st_intersection(census_layer) %>% # get intersection area with each polygon
      mutate(area_int = as.numeric(st_area(.)/1000000)) %>%    # get the area of each intersected polygon 
      # calculate the ratio between the hexagon area of intersection and the area of the census block that it intersected
      # multiply the ratio by the census block statistic to assign a value to the intersected area
      mutate(!!pop_column := !!existing_pop_column * (area_int/area_km)) %>% 
      # we don't need the geometry (it might be altered if census layer has bad polygon layers)
      st_drop_geometry() %>%
      # Some hexagons may be split between different census blocks, so we sum the results of the previous 
      # step on the hexagon level
      group_by(!!column) %>%
      summarise(!!pop_column := round(sum(!!pop_column, na.rm = TRUE))) %>%  ungroup() 
    
    # we join the results onto the original geometry
    projected_results[[i]] <- hexagon_layers[[i]] %>% 
      left_join(projected_layer, by = as.character(column))
    
    print(paste0("finished projecting layer ", i))
    
  }
  return(projected_results)
}

# apply the function
hexagons_with_pop <- project_pop(census_layer = city_geom, 
                                 hexagon_layers = results, 
                                 #existing_pop_column = "pop_2018_c") # Cairo  # "jobs_total
                                 #existing_pop_column = "pobtot") # Mexico City    # "jobs" #pobtot
                                 existing_pop_column = "pop_totalE") # US Cities  # "C000" #pop_totalE

# ----- 3. Spatial join hexagon layers with each other to determine overlapping hexagons

intersect_layers = function(layers){
  
  # This function gets the intersection between each layer and all the layers bigger than it. Each hexagon
  # is assigned to one hexagon in the intersected (bigger) layer. A column is added 
  
  # Input:
  #   layers: a list where each item is a spatial hexagon layer. 
  #           Items are arranged in ascending order of hexagon size (smaller first)
  
  i <- 1
  j <- i + 1
  # iterate over all layers
  while(i <= length(layers) - 1){
    # for each layer, iterate over all layers bigger than it, and do a spatial join to map hexagons from the smaller layer
    # onto the bigger layer
    while(j <= length(layers)){
      layers[[i]] <- layers[[i]] %>% st_join(layers[[j]], # bigger layer on the left
                                             join = st_intersects, 
                                             largest = TRUE) # largest so that each hexagon is joined onto one hexagon only
      
      print(paste0("finished ", j-i, " out of ", length(layers) - i, " intersections for layer ", i))
      
      j = j + 1
    }
    print(paste0("finished intersections for layer ", i))
    
    i = i + 1
    # bring j back down
    j = i + 1
    
    
  }
  
  return(layers)
}

# apply the function
intersected_layers <- intersect_layers(layers = hexagons_with_pop)

# ----- 4. Create a combined hexagon layer

# -- a. Determine which hexagon resolution to use for each area

determine_res <- function(hexagon_layer, density_threshold = 0.25){
  # Input
  # hexagon_layers: a list where each item is a spatial hexagon layer. 
  # density_threshold: the threshold at which we determine if a hexagon should be replaced by smaller resolution hexagons.
  #                    It is based on the cumulative distribution of population. If cd value of a column = 0.25, then 
  #                    the proportion of all values less than or equal to the column value = 0.25

    
  # determine the variable job column name
  column <- colnames(hexagon_layer)[1] #cell_id_<resolution>
  # job column name 
  pop_column <- stringr::str_c("pop_", column)
  pop_column <- sym(pop_column)
    
  # determine population density as a percentile. Ued to determine if we should keep the hexagon
  # or replace it with smaller resolution hexagons
  hexagon_layer_cd <- hexagon_layer  %>% 
    mutate(
      # Proportion of all values less than or equal to the column value
      cd = cume_dist(!!pop_column), 
      # we add a column to determine if this resolution is sufficient
      split_hexagon = case_when(cd < density_threshold ~ "no",
                                TRUE ~ "yes"))

  return(hexagon_layer_cd)
}




# # apply the function
# test <- determine_res(hexagon_layer = intersected_layers[[1]],
#                                       density_threshold = 0.75)


# -- b. Create the combined hexagon layer

merge_hexagons <- function(hexagon_layers, grid_sizes, density_threshold){
  
  # LOGIC: 
  # start with biggest hexagon
  # 1. RESULTS: Empty list of dataframes 
  # 2. start with biggest hexagon (LAYER 1)
  #     a) Store hexagons that will not be replaced with smaller hexagons (low population density)
  #     b) HEXAGONS_TO_SPLIT: IDs of hexagons from LAYER 1 that WILL be replaced with smaller hexagons
  # 3. Go down to smaller hexagon layer (LAYER 2)
  #     a) Filter hexagons with IDs corresponding to IDs in HEXAGONS TO REPLACE
  #     b) From this subset: 
  #         i) Store hexagons that will not be replaced with smaller hexagons (low population density)
  #         ii) HEXAGONS_TO_SPLIT: IDs of hexagons from LAYER 2 that WILL be replaced with smaller hexagons
  
  # INPUT
  # hexagon_layers: a list where each item is a spatial hexagon layer. 
  # grid sizes: a list of grid sizes. The same input used for the "create_grids" function
  
  
  
  # list to store results in. Each list element will be a one column dataframe
  results <- vector(mode = "list", length = length(hexagon_layers))
  
  for(i in length(hexagon_layers):1){
    
    
    # for the largest diameter layer, we don't compare it with any bigger layers
    if(i == length(hexagon_layers)){
      # apply the function that gets that adds a percentile column 
      hexagon_layers[[i]] <- determine_res(hexagon_layer = hexagon_layers[[i]],
                                           density_threshold = density_threshold)
      
      hexagons_to_keep <- hexagon_layers[[i]] %>% filter(split_hexagon == "no") %>%
        mutate(hex_diameter = grid_sizes[i]) %>% select(hex_diameter)
      #hexagons_to_split <- hexagon_layers[[i]] %>% filter(split_hexagon == "yes")
      results[[i]] <- hexagons_to_keep
      
    } else{
      # id column of the layer bigger than the current one
      id_column_name <-  str_c("cell_id_", grid_sizes[i+1])
      id_column_name <-  sym(id_column_name)
      
      # look at layer bigger than this one and determine the IDs of the hexagons that should be split
      # a) get hexagons of the bigger layer
      hexagons_to_split <- hexagon_layers[[i+1]] %>% filter(split_hexagon == "yes")
      # b) look at current layer, and only use the hexagons that intersect with the hexagons that need to be 
      # split in the larger layer
      relevant_hexagons <- hexagon_layers[[i]] %>%
        filter(!!id_column_name %in% hexagons_to_split[[id_column_name]])
      
      # apply the function that gets that adds a percentile column 
      hexagon_layers[[i]] <- determine_res(hexagon_layer = relevant_hexagons,
                                           density_threshold = density_threshold)
      # determine which hexagons don't need to be split further. If condition used because we won't split further 
      # if we are at the smallest resolution, regardless of whether we should or not
      if(i > 1){
        # determine which hexagons don't need to be split further, and store them in the results list
        hexagons_to_keep <- hexagon_layers[[i]] %>% filter(split_hexagon == "no") %>%
          mutate(hex_diameter = grid_sizes[i]) %>% select(hex_diameter)
        
        results[[i]] <- hexagons_to_keep
        
      } else{
        hexagons_to_keep <- hexagon_layers[[i]] %>%
          mutate(hex_diameter = grid_sizes[i]) %>% select(hex_diameter)
        
        results[[i]] <- hexagons_to_keep
      }
      
    }
  }
  return(results)
}

# apply the function
combined_hexagon_layer <- merge_hexagons(hexagon_layers = intersected_layers, 
                                         grid_sizes = grid_sizes,
                                         density_threshold = 0.3)

x <- bind_rows(combined_hexagon_layer)
plot(x["hex_diameter"])


# ----- 6. Remove Overlapping Goemetries

remove_overlaps = function(combined_layer){
  # function that removes overlapping geometries between hexagon layers of different resolutions. 
  # It starts from the biggest diameter and works its way down
  
  for(i in length(combined_layer):2){
    # prepare the layer we are trimming
    layer_to_trim <- combined_layer[[i]] %>% st_make_valid()
    # we combine all smaller layers and union them so as not to get the cross product of both layers
    # https://gis.stackexchange.com/questions/414722/st-difference-resulting-in-multiple-overlapping-polygons-and-no-difference
    layers_to_trim_by <- bind_rows(combined_layer[1:i-1]) %>% st_union() %>% st_make_valid()
    # replace the layer with the cropped layer
    combined_layer[[i]] <- st_difference(layer_to_trim, layers_to_trim_by)
    
  }
  return(combined_layer)
  
}

# apply the function
trimmed_hexagon_layer  <- remove_overlaps(combined_hexagon_layer)

x2 <- bind_rows(trimmed_hexagon_layer)
plot(x2["hex_diameter"])

# check
#plot(st_geometry(combined_hexagon_layer[[3]]))
#plot(st_geometry(trimmed_hexagon_layer[[3]]), add = TRUE, col = "green")

# convert to one layer
trimmed_hexagon_layer <- bind_rows(trimmed_hexagon_layer)

# remove any rows that aren't polygons
trimmed_hexagon_layer <- trimmed_hexagon_layer  %>% filter(st_is(. , c("POLYGON", "MULTIPOLYGON"))) %>%
  mutate(cell_id = row_number())

# removing overlaps creates some strange multipolygons. 
# Step 1. cast to Polygon: this creates multiple rows for each polygon in a multipolygon
# Step 2: take the biggest polygon for each cell id
# step 3: remove very small areas

# step 1
trimmed_hexagon_layer <- trimmed_hexagon_layer %>% st_cast(., "POLYGON")
# step 2
trimmed_hexagon_layer <- trimmed_hexagon_layer %>% 
  mutate(area =  as.numeric(st_area(.)/1000000)) %>% 
  group_by(cell_id) %>% slice(which.max(area)) %>% 
  # step 3: # remove very small areas -> use area of smallest hexagon as reference
  filter(area > 1e-8) %>%
  ungroup() %>% select(-area) %>%
  # edit cell_id again
  mutate(cell_id = row_number())


#plot(st_geometry(trimmed_hexagon_layer))
#plot(trimmed_hexagon_layer["hex_diameter"])
trimmed_hexagon_layer <- st_make_valid(trimmed_hexagon_layer)


# ----- 7. Fill Gaps

# Combining hexagons of different diameters leads to holes in the variable hexgrid layer. We fill these gaps with 
# hexagons of the smallest diameter

fill_gaps <- function(layer, fixed_res_layer, resolution){
  #Input:
  # layer: variable hexgrid layer with gaps
  # fixed_res_layer: layer used to plug gaps in "layer" (Using st_difference()),. It is one of the layers in the results list
  # resolution: the hexagon_diameter of "fixed_res_layer". It needs to correspond to the diameter of 'fixed_layer_res"
  #             I use the grid_sizes vector to get the diameter
  # turn the layer into one geometry
  # use st_snap to grid to avoid this problem: https://gis.stackexchange.com/questions/50399/fixing-non-noded-intersection-problem-using-postgis
  layer_union <- layer %>% lwgeom::st_snap_to_grid(1) %>%
    st_union()
  # get the difference between the variable hexgrid layer and the fixed res layer
  layer_diff <- st_difference(fixed_res_layer, layer_union)
  # add cell_id column and remove small polygons
  layer_diff <- layer_diff %>% mutate(hex_diameter = resolution,
                                      area = as.numeric(st_area(.))) %>%
    filter(area >= 0.7*max(area)) %>%
    select(hex_diameter)
  # add to original layer and reindex the cell_id column
  layer_plugged <- layer %>%
    bind_rows(layer_diff) %>%
    mutate(cell_id = row_number())
  
  layer_plugged <- st_make_valid(layer_plugged)
  
  return(layer_plugged)
  
}

# apply the function
trimmed_hexagon_layer_filled <- fill_gaps(layer = trimmed_hexagon_layer, 
                                          fixed_res_layer = results[[2]], 
                                          resolution = grid_sizes[2])

mapview::mapviewOptions(fgb = FALSE)
mapview::mapview(trimmed_hexagon_layer_filled, zcol = "hex_diameter") + mapview::mapview(trimmed_hexagon_layer, zcol = "hex_diameter")

# save result
st_write(trimmed_hexagon_layer_filled, paste0("../data_raw/", city, "/level_i/variable_hexgrid.geojson"), delete_dsn = TRUE)








# x <- city_geom %>% 
#   mutate(pop_2018_c = case_when(is.na(pop_2018_c) ~ 1, 
#                                 pop_2018_c == 0 ~ 1,
#                                 TRUE ~ pop_2018_c))
# 
# 
# xx <- city_geom %>% filter(is.na(pop_2018_c))


