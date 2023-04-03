###############################################################################################
### This script is used to prepare the base layer (population + employment) for mexico city ###
###############################################################################################

library(sf)
library(tidyverse)

# ------------------------------------- Load the layers ------------------------------------ #
city <- "Mexico City"
# administrative boundaries with population (polygon)
city_pop <- st_read(paste0("../data_raw/", city, "/level_i/population.geojson")) %>% st_transform(4326)
# job locations (point)
city_jobs <- st_read(paste0("../data_raw/", city, "/level_i/employment_median_personas.shp")) %>% st_transform(4326)

# check that crs is equal
st_crs(city_pop) == st_crs(city_jobs)


# ------------------------------------- Assign jobs to polygons ------------------------------------ #

# spatial join to determine which zone each point fall in

# city_pop_jobs <- city_jobs %>%
#   st_join(city_pop, join = st_intersects, largest = TRUE) 

# within operation without largest argument is orders of magnitude faster
city_pop_jobs <- city_jobs %>%
  st_join(city_pop, join = st_within)  

# get number of jobs per zone
city_pop_jobs_summed <- city_pop_jobs %>% 
  st_drop_geometry() %>%
  group_by(cve_ageb) %>%
  summarize(jobs = sum(per_median))

# join dataframe back onto population polygon layer
city_pop_jobs_sf <- city_pop %>% 
  left_join(city_pop_jobs_summed, by = "cve_ageb") %>%
  # replace na values (zones that didn't have any jobs)
  mutate(jobs = replace_na(jobs, 0))

plot(city_pop_jobs_sf["jobs"])


# ---- save
st_write(city_pop_jobs_sf ,paste0("../data_raw/", city, "/level_i/census_pop_jobs.geojson"), delete_dsn = TRUE)


