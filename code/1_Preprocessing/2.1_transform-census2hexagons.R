###################################################################################################
###    The purpose of this script is to project data from one spatial layer to another. We      ###
###    create a hexagon layer with the necessary diamter, and then we project the data from the ###
###    census layers onto the hexagon layer                                                     ###
###################################################################################################
library(sf)
library(tidyverse)


# # USA
# city <- "San Francisco"
# city <- "Minneapolis"
# layer_name <- "census_bg.geojson"
# hex_layer_name  <- "variable_hexgrid.geojson"

# Mexico City
# city <- "Mexico City"
# layer_name <- "census_pop_jobs.geojson"
# hex_layer_name  <- "variable_hexgrid.geojson"

# Cairo
city <- "Cairo"
layer_name <- "gcr_shiyakhas_with_jobs.geojson"
hex_layer_name  <- "variable_hexgrid.geojson"

# geometry data
city_geom <- st_read(paste0("../data_raw/", city, "/level_i/", layer_name)) %>%
  # we need a metric crs to create the grid layer
  st_transform(3857)

# check that columns are numeric and convert if not
glimpse(city_geom)
if(city == "Mexico City"){
  city_geom <- city_geom %>% mutate(across(pobtot:pobfem, ~as.numeric(.)))
}
glimpse(city_geom)

# --------------------------------- Hexagon Grid Layer ----------------------------- #

# ------- OPTION A: Create a Hexagon Grid Layer 


# # # 1. create grid
# #
# # --- select grid size (diameter in meters)
# grid_size <- 400
# #
# # # --- create the grid
# city_geom_grid <- st_make_grid(city_geom, cellsize = grid_size, square = FALSE) %>%
#   st_as_sf() %>% st_make_valid() %>%
#   # crop to study area boundaries
#   st_filter(city_geom, .predicate = st_intersects) %>%
#   # add unique id for each row
#   mutate(cell_id = row_number())
# # 
# # plot to check
# plot(st_geometry(city_geom_grid))
# mapview::mapviewOptions(fgb = FALSE)
# mapview::mapview(city_geom_grid) + mapview::mapview(city_geom)


# ------- OPTION B: Read in Variable Resolution Grid Layer 
city_geom_grid <- st_read(paste0("../data_raw/", city, "/level_i/", hex_layer_name)) %>%
  st_transform(3857)
# convert hex_diameter column to character, so that it's not affected when getting ratios below
city_geom_grid <- city_geom_grid %>% mutate(hex_diameter = as.character(hex_diameter))
# we will need this matching table to join the hex diameter column back at the end
matching_table <- city_geom_grid %>% st_drop_geometry()

# we will need to edit the function below if we use OPTION B

# --------------------------------- Project columns from census polygons to hexagons ----------------------------- #

# --- add an "area" column i order to get the ratio between the census polygon and the hexagon after intersecting
city_geom <- city_geom %>% 
  mutate(area_km = as.numeric(st_area(.)/1000000)) %>%
  st_make_valid()


# --- project data from the census layer to the hexagon layer
city_geom_grid_proj <- city_geom_grid %>% st_intersection(city_geom) %>% # get intersection area with each polygon
  st_make_valid() %>%
  mutate(area_int = as.numeric(st_area(.)/1000000)) %>%    # get the area of each intersected polygon 
  # calculate the ratio between the hexagon area of intersection and the area of the census block that it intersected
  # multiply the ratio by the census block statistic to assign a value to the intersected area
  group_by(cell_id) %>%
  mutate(across(where(is.numeric), ~ round(.x * (area_int/area_km)))) %>% ungroup() %>%
  # the geometry column will be altered by the summarize operation. We drop it and join it back later
  st_drop_geometry() %>%
  # Some hexagons may be split between different census blocks, so we sum the results of the previous 
  # step on the hexagon level
  group_by(cell_id) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%  ungroup() %>%
  # we no longer need these columns
  select(-c("area_km", "area_int"))

# re-join geometry 
city_geom_grid <- city_geom_grid %>% 
  inner_join(city_geom_grid_proj, by = "cell_id")

# # remove empty geometries 
# city_geom_grid <- city_geom_grid[!st_is_empty(city_geom_grid), drop=FALSE]

# # remove geometries that aren't polygons
# city_geom_grid <- city_geom_grid %>%
#   mutate(geom_type = st_geometry_type(.)) %>%
#   filter(geom_type == "POLYGON") %>% select(-geom_type)


# transform crs
city_geom_grid <- city_geom_grid %>% st_transform(4326) %>%
  # remove empty geometries
  filter(!st_is_empty(.)) %>%
  st_make_valid() 

# validation may turn some polygons into multipolygons. We undo this:

# Step 1. cast to Polygon: this creates multiple rows for each polygon in a multipolygon
# Step 2: take the biggest polygon for each cell id
# step 3: remove very small areas

# city_geom_grid <- city_geom_grid %>%
#   st_cast(., "POLYGON") %>%
#   mutate(area =  as.numeric(st_area(.)/1000000)) %>% 
#   group_by(cell_id) %>% slice(which.max(area)) %>% 
#   # step 3: # remove very small areas -> use area of smallest hexagon as reference
#   ungroup() %>% select(-area) %>%
#   st_make_valid() 

# # check that intersection steps worked
# # original
# a <- city_geom %>% st_drop_geometry() %>% summarise(across(is.numeric,  ~ sum(.x, na.rm = TRUE)))
# # processed
# b <- city_geom_grid %>% st_drop_geometry() %>% summarise(across(is.numeric,  ~ sum(.x, na.rm = TRUE)))
# # comparison
# c <- a %>% bind_rows(b)

city_geom_grid <- city_geom_grid %>% select(-hex_diameter) %>%
  left_join(matching_table, by = "cell_id") %>%
  mutate(hex_diameter = as.numeric(hex_diameter))

# remove invalid geometries
city_geom_grid <- city_geom_grid %>% filter(st_is_valid(.) == TRUE)
# ------------------- save

# variable resolution
st_write(city_geom_grid, paste0("../data_raw/", city, "/level_i/census_hex.geojson"), delete_dsn = TRUE)
# if fixed resolution
#st_write(city_geom_grid, paste0("../data_raw/", city, "/level_i/census_hex_fixed.geojson"), delete_dsn = TRUE)

# Plot
library(tmap)

# hex diamater
tm_shape(city_geom_grid) +
  tm_polygons(col = 'hex_diameter', 
              style = "cat",
              palette = "-Blues",
              title = "Hexagon Diameter (m)",
              border.alpha = 0.2,
              legend.is.portrait = FALSE) +
  tm_layout(main.title = "Variable Hexgrid",
            main.title.size = 1.2,
            main.title.color = "azure4",
            main.title.position = c("left", "top"),
            fontfamily = 'Georgia',
            frame = FALSE,
            legend.position = c("left","bottom")) -> p1
p1

tmap_save(tm = p1, filename = paste0("../data/", city,"/Plots/variable_hexgrid.png"))


# job distribution
tm_shape(city_geom_grid) +
  tm_fill(col = 'pobtot', 
          style = "fisher",
          palette = "-Greens",
          title = "Number of Job",
          border.alpha = 0.2,
          legend.is.portrait = FALSE) +
  tm_layout(main.title = "Job Distribution", 
            main.title.size = 1.2,
            main.title.color = "azure4",
            main.title.position = c("left", "top"),
            fontfamily = 'Georgia',
            frame = FALSE,
            legend.position = c("left","bottom")) -> p2
p2



