###################################################################################################
###    The purpose of this script is to identify whether each zone (census block / hexagon) is  ###
###    served by a shared micromobility provider or not. For each zone, we add a binary column  ###
###    for each provider to identify whether it works in the zone or not.                       ###
###################################################################################################

library(tidyverse)
library(sf)

# ------------------------------------ Load the data ------------------------------------ #

#city = "Minneapolis"
#city = "San Francisco"
#city = "Mexico City"
city = "Cairo"


# variables that differ depending on the city
city_variables <- tribble(
  ~city,  ~provider_docked, ~provider_dockless, 
  "San Francisco", "docked_Bay Wheels" ,  "dockless_Spin San Francisco", 
  "Minneapolis", "docked_Nice Ride Minnesota",  "dockless_Spin Minneapolis", 
  "Mexico City", "docked_ECOBICI",  NA,
  "Cairo", "docked_Donkey_Republic",  NA
)

# --- docks

# Minneapolis / San Francisco
if (city %in% c("San Francisco", "Minneapolis")){
  docks <- st_read(paste0("../data_raw/", city, "/GBFS/gbfs_stations.geojson")) %>%
    st_make_valid()
  
  dockless <- st_read(paste0("../data_raw/" , city, "/GBFS/gbfs_zones.geojson")) %>%
    st_make_valid()
  
} else if (city == "Mexico City"){
  docks <- st_read(paste0("../data_raw/", city, "/GBFS/gbfs_stations.geojson")) %>%
    mutate(provider = "ECOBICI") %>%
    st_make_valid()
  
} else if (city == "Cairo"){
  docks <- st_read(paste0("../data_raw/", city, "/GBFS/bikeshare.geojson")) %>%
    mutate(provider = "Donkey_Republic") %>%
    st_make_valid()
}



# --- census data 

census <- st_read(paste0("../data_raw/", city, "/level_i/census_hex.geojson"))

# US blocks
# census_b_sf <- st_read("../data_raw/San Francisco/level_i/census_san_francisco_block.geojson")  # San Francisco

# -----------------------  Functions to match identify if a zone has docked / dockless service ----------------------- #

# ----- Function to determine which DOCKED providers operate in which zones 
match_docked_to_zones = function(city_layer,           # census / hexagons
                                 docked_stations,      # point layer with stations and associated provider
                                 id_col){              # id column in the zone layer. Used for joining
  # --- create a column with a unique ID for each zone
  city_layer <- city_layer %>% 
    mutate(zone_id = row_number())
  # --- select necessary columns from each layer
  city_layer_temp <- city_layer %>% 
    select(zone_id) 
  
  docked_stations <- docked_stations %>% 
    select(provider)
  # --- make sure both layers have the same crs
  docked_stations <- docked_stations %>%
    st_transform(st_crs(city_layer)) %>%
    # add a column that will be used in the pivot wider operation
    mutate(provider_binary = 1)
  
  # --- spatial join to assign stations to city_layer zones
  joined_layer <- st_join(city_layer_temp, 
                          docked_stations, 
                          left = FALSE,  # we want an inner join not a left join
                          join = st_intersects) 
  
  # --- remove duplicate rows
  # the same provider might have multiple stations in the same zone. This will create multiple rows, whereas 
  # we only want 1 row to identify the availability of a docked station in the zone
  joined_layer <- joined_layer %>% 
    # remove the geometry so that we can run the distinct function 
    st_drop_geometry() %>%
    distinct(zone_id, provider, .keep_all = TRUE) 

  # --- pivot wider to get one row per zone, and all providers as columns with binary 0/1 values to indicate availability
  joined_layer <- joined_layer %>% 
    pivot_wider(names_from = provider, 
                values_from = provider_binary,
                values_fill = 0,
                names_prefix = "docked_")
  
  # --- join results onto original layer 
  city_layer <- city_layer %>%
    left_join(joined_layer, by = "zone_id")

  # --- replace na values with 0 for provider columns
  city_layer <- city_layer%>%
    mutate_at(vars(matches("docked_")), ~replace_na(., 0))
  
  # transform to desired crs
  city_layer <- city_layer %>% st_transform(4326)

  return(city_layer)
}


# census_bg_mn_docked <- match_docked_to_zones(city_layer = census_bg_mn, 
#                                              docked_stations = docks_mn,
#                                              id_col = GEOID)



# ----- Function to determine which DOCKLESS systems operate in each zone

match_dockless_to_zones = function(city_layer,               # census / hexagons
                                   dockless_zones,           # polygon layer with dockless zones and associated providers
                                   id_col){                  # id column in the zone layer. Used for joining
  # --- create a column with a unique ID for each zone
  city_layer <- city_layer %>% 
    mutate(zone_id = row_number())
  # --- select necessary columns from each layer
  city_layer_temp <- city_layer %>% 
    select(zone_id) 
  
  dockless_zones <- dockless_zones %>% 
    st_make_valid() %>%
    select(provider)
  # --- make sure both layers have the same crs
  dockless_zones <- dockless_zones %>%
    st_transform(st_crs(city_layer)) %>%
    # add a column that will be used in the pivot wider operation
    mutate(provider_binary = 1)
  
  # --- spatial join to assign stations to city_layer zones
  joined_layer <- st_join(city_layer_temp, 
                          dockless_zones,
                          left = FALSE,  # we want an inner join not a left join
                          join = st_intersects) 
  
  # --- remove duplicate rows
  # the same provider might have multiple stations in the same zone. This will create multiple rows, whereas 
  # we only want 1 row to identify the availability of a docked station in the zone
  joined_layer <- joined_layer %>% 
    # remove the geometry so that we can run the distinct function 
    st_drop_geometry() %>%
    distinct(zone_id, provider, .keep_all = TRUE) 
  
  # --- pivot wider to get one row per zone, and all providers as columns with binary 0/1 values to indicate availability
  joined_layer <- joined_layer %>% 
    pivot_wider(names_from = provider, 
                values_from = provider_binary,
                values_fill = 0,
                names_prefix = "dockless_")
  
  # --- join results onto original layer 
  city_layer <- city_layer %>%
    left_join(joined_layer, by = "zone_id")
  
  # --- replace na values with 0 for provider columns
  city_layer <- city_layer%>%
    mutate_at(vars(matches("dockless_")), ~replace_na(., 0))
  
  # transform to desired crs
  city_layer <- city_layer %>% st_transform(4326)
  
  return(city_layer)
}



# ----- Combined Function: Return both DOCKED and DOCKLESS service providers in each zone

match_providers_to_zones = function(city_layer, 
                                    docked_stations,
                                    dockless_zones,
                                    id_col){
  
  # --- create a column with a unique ID for each zone
  city_layer <- city_layer %>% 
    mutate(zone_id = row_number())
  
  # ---------- DOCKED
  
  if(!is.na(docked_stations)){
    # --- select necessary columns from each layer
    city_layer_temp <- city_layer %>% 
      select(zone_id) 
    
    docked_stations <- docked_stations %>% 
      select(provider)
    # --- make sure both layers have the same crs
    docked_stations <- docked_stations %>%
      st_transform(st_crs(city_layer)) %>%
      # add a column that will be used in the pivot wider operation
      mutate(provider_binary = 1)
    
    # keep only geometry that is inside the sudy area
    docked_stations <- docked_stations %>%
      st_filter(city_layer, .predicate = st_intersects)
    
    # --- spatial join to assign stations to city_layer zones
    joined_layer <- st_join(city_layer_temp, 
                            docked_stations, 
                            left = FALSE,  # we want an inner join not a left join
                            join = st_intersects) 
    
    # --- remove duplicate rows
    # the same provider might have multiple stations in the same zone. This will create multiple rows, whereas 
    # we only want 1 row to identify the availability of a docked station in the zone
    joined_layer <- joined_layer %>% 
      # remove the geometry so that we can run the distinct function 
      st_drop_geometry() %>%
      distinct(zone_id, provider, .keep_all = TRUE) 
    
    # --- pivot wider to get one row per zone, and all providers as columns with binary 0/1 values to indicate availability
    joined_layer <- joined_layer %>% 
      pivot_wider(names_from = provider, 
                  values_from = provider_binary,
                  values_fill = 0,
                  names_prefix = "docked_")
    
    # --- join results onto original layer 
    city_layer <- city_layer %>%
      left_join(joined_layer, by = "zone_id")
    
    # --- replace na values with 0 for provider columns
    city_layer <- city_layer %>%
      mutate_at(vars(matches("docked_")), ~replace_na(., 0))
    
  }
  
  
  
  # ---------- DOCKLESS
  
  if(!is.na(dockless_zones)){
    
    # --- select necessary columns from each layer
    city_layer_temp <- city_layer %>% 
      select(zone_id) 
    
    dockless_zones <- dockless_zones %>% 
      st_make_valid() %>%
      select(provider)
    # --- make sure both layers have the same crs
    dockless_zones <- dockless_zones %>%
      st_transform(st_crs(city_layer)) %>%
      # add a column that will be used in the pivot wider operation
      mutate(provider_binary = 1)
    
    # keep only geometry that is inside the sudy area
    dockless_zones <- dockless_zones %>%
      st_filter(city_layer, .predicate = st_intersects)
    
    # --- spatial join to assign stations to city_layer zones
    joined_layer <- st_join(city_layer_temp, 
                            dockless_zones,
                            left = FALSE,  # we want an inner join not a left join
                            join = st_intersects) 
    
    # --- remove duplicate rows
    # the same provider might have multiple stations in the same zone. This will create multiple rows, whereas 
    # we only want 1 row to identify the availability of a docked station in the zone
    joined_layer <- joined_layer %>% 
      # remove the geometry so that we can run the distinct function 
      st_drop_geometry() %>%
      distinct(zone_id, provider, .keep_all = TRUE) 
    
    # --- pivot wider to get one row per zone, and all providers as columns with binary 0/1 values to indicate availability
    joined_layer <- joined_layer %>% 
      pivot_wider(names_from = provider, 
                  values_from = provider_binary,
                  values_fill = 0,
                  names_prefix = "dockless_")
    
    # --- join results onto original layer 
    city_layer <- city_layer %>%
      left_join(joined_layer, by = "zone_id")
    
    # --- replace na values with 0 for provider columns
    city_layer <- city_layer%>%
      mutate_at(vars(matches("dockless_")), ~replace_na(., 0))
    
  }
  
  
  # transform to desired crs
  city_layer <- city_layer %>% st_transform(4326)
  
  return(city_layer)
}


# ----- Run function 

# San Francisco / Minneapolis
if (city %in% c("San Francisco", "Minneapolis")){
  # San Francisco / Minneapolis
  census_matched <- match_providers_to_zones(city_layer = census,
                                             docked_stations = docks,
                                             dockless_zones = dockless,
                                             id_col = GEOID)
} else if(city %in% c("Mexico City", "Cairo")){
  # Mexico City/Cairo
  census_matched <- match_providers_to_zones(city_layer = census,
                                             docked_stations = docks,
                                             dockless_zones = NA,
                                             id_col = GEOID)
  
}


# -----------------------  Availability of docked/dockless in neighbouring zones ----------------------- #

# Since users can walk to neighboring zones, it makes sense to factor in these zones when determining whether 
# micromobility exists at a zone or not. We use 

# Spatial Weights


# Create a sparse matrix of neighbors using st_intersects https://github.com/r-spatial/sf/issues/234

availability_neighbors = function(layer, docked_col, dockless_col){
  
  # the id column must match the row_number because the output of st_intersects(.) is row.id and col.id
  layer <- layer %>% mutate(cell_id = row_number())
  
  # prepare column names for calling inside the function
  #docked_col <- sym(docked_col)
  docked_col <- ifelse(is.na(docked_col), docked_col, sym(docked_col))
  dockless_col <- ifelse(is.na(dockless_col), dockless_col, sym(dockless_col))

  # intersect geometry with itself
  layer_matrix <- layer %>% st_intersects(.)
  # convert result from list to dataframe
  layer_matrix_df <- layer_matrix %>% as.data.frame()
  # join onto original layer which has info on docked / dockless service availability at each zone 
  layer_matrix_meta <- layer_matrix_df %>% 
    left_join(layer, by = c("col.id" = "cell_id")) 

  
  # get number of stations (docked) / zones (dockless) that are in the zone + all its neighboring zones
  layer_matrix_meta <- layer_matrix_meta %>% 
    group_by(row.id) %>% 
    summarise(docked_service = sum(!! docked_col, na.rm = TRUE),
              dockless_service = sum(!! dockless_col, na.rm = TRUE)) %>%
    ungroup()
  
  layer_matrix_meta <- layer %>% left_join(layer_matrix_meta, by = c("cell_id" = "row.id"))
  
  return(layer_matrix_meta)
}

# --- apply the function

if (city == "San Francisco"){
  # San Francisco
  census_matched_neighbors <- availability_neighbors(layer = census_matched,
                                                     docked_col = "docked_Bay Wheels",
                                                     dockless_col = "dockless_Spin San Francisco")
  
} else if(city == "Minneapolis"){
  # Minneapolis
  census_matched_neighbors <- availability_neighbors(layer = census_matched,
                                                     docked_col = "docked_Nice Ride Minnesota",
                                                     dockless_col = "dockless_Spin Minneapolis")
  
} else if(city == "Mexico City"){
  # Mexico city
  census_matched_neighbors <- availability_neighbors(layer = census_matched,
                                                     docked_col = "docked_ECOBICI",
                                                     dockless_col = NA)
  
} else if(city == "Cairo"){
  # Cairo
  census_matched_neighbors <- availability_neighbors(layer = census_matched,
                                                     docked_col = "docked_Donkey_Republic",
                                                     dockless_col = NA)
}



plot(census_matched_neighbors["docked_service"])
plot(census_matched_neighbors["dockless_service"])

mapview::mapviewOptions(fgb = FALSE)
mapview::mapview(census_matched_neighbors, zcol = "docked_service") + mapview::mapview(docks)
mapview::mapview(census_matched_neighbors, zcol = "dockless_service") + mapview::mapview(dockless)

# --- save
st_write(census_matched_neighbors, paste0("../data/", city, "/level_i_ii/zones_w_micromobility_providers.geojson"), delete_dsn = TRUE)

#census_matched_neighbors <- st_read(paste0("../data/", city, "/level_i_ii/zones_w_micromobility_providers.geojson"))

# ---------------------------------   Plots --------------------------------- #
library(tmap)

# Docked
tm_shape(census_matched_neighbors) +
  tm_polygons(#col = 'docked_Bay Wheels',  # San francisco
              col ='docked_Nice Ride Minnesota', # Mineapolis
              #col ='docked_ECOBICI', # Mexico City
              style = "cat",
              palette = "-Pastel2",
              title = "Availability of \nDocked Services",
              border.alpha = 0.3, 
              legend.is.portrait = FALSE) +
  tm_layout(main.title = "Docking Stations",
            main.title.size = 1.2,
            main.title.color = "azure4",
            main.title.position = c("left", "top"),
            fontfamily = 'Georgia',
            frame = FALSE,
            legend.position = c("left","bottom")) -> p1
p1

tmap_save(tm = p1, filename = paste0("../data/", city,"/Plots/docked_availability.png"))


# Dockless
tm_shape(census_matched_neighbors) +
  tm_polygons(#col = 'dockless_Spin San Francisco',
              col = 'dockless_Spin Minneapolis',
              style = "cat",
              palette = "-Pastel2",
              title = "Availability of \nDockless Service",
              border.alpha = 0.3, 
              legend.is.portrait = FALSE) +
  tm_layout(main.title = "Dockless Zones",
            main.title.size = 1.2,
            main.title.color = "azure4",
            main.title.position = c("left", "top"),
            fontfamily = 'Georgia',
            frame = FALSE,
            legend.position = c("left","bottom")) -> p2
p2

tmap_save(tm = p2, filename = paste0("../data/", city,"/Plots/dockless_availability.png"))

# join plots together
p3 <- tmap_arrange(p1, p2, nrow = 1)
p3

tmap_save(tm = p3, filename = paste0("../data/", city,"/Plots/docked_dockless_availability.png"))


# census job distribution

# layer with census data mapped onto one resultion hexagon (useful for density)
fixed_res_census <- st_read(paste0("../data_raw/", city, "/level_i/census_hex_fixed.geojson"))

# columns we need for plotting
# USA
old_names <- c("pop_totalE", "C000")  # USA
old_names <- c("pobtot", "jobs")  # Mexico City
new_names <- c("population", "jobs")


census_long <- fixed_res_census %>% 
  select(cell_id, all_of(old_names)) %>%
  rename_with(~ new_names, all_of(old_names)) %>% # rename the columns using the new_names list
  pivot_longer(cols = new_names)


tm_shape(census_long) +
  tm_fill(col = 'value',
              style = "fisher",
              palette = "BuPu",
              title = "",
              #border.alpha = 0.05, 
              #legend.is.portrait = FALSE
          ) +
  tm_facets(by="name",
            nrow = 1,
            free.coords=FALSE)  +
  tm_layout(main.title = "Spatial Distribution of Empolyment and Population",
            main.title.size = 1.2,
            main.title.color = "azure4",
            main.title.position = c("left", "top"),
            fontfamily = 'Georgia',
            frame = FALSE,
            legend.position = c("left","bottom")) -> p

p

tmap_save(tm = p, filename = paste0("../data/", city,"/Plots/pop_emp_facet.png"))



