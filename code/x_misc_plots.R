library(sf)
library(tidyverse)
library(tmap)
# population data from 
library(tidycensus)
# employment data from lehd lodes
library(lehdr)
# city boundaries
library(tigris)

city <- "Minneapolis"
state <- "MN"
county <- c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington")


# ---------------------- Define the different boundaries 

# --- boundary of the specified counties 
counties_boundary <- get_acs(state = state, 
                               county = county, 
                               geography = "block group",
                               variables = c(pop_total = "B01001_001"),
                               output = "wide",
                               year = 2019,
                               geometry = TRUE) %>% 
  # remove the margin of error (moe) columns
  select(-ends_with("M")) %>%
  st_transform(3857)

# --- urban boundary of metropolitan area
metro_boundary <- tigris::urban_areas() %>%
  # filter to the city ussing stringr
  filter(str_detect(NAME10, city)) %>%
  st_transform(3857)

# --- boundary of the city 
city_boundary <- tigris::places(state) %>%  
  tigris::filter_place(city) %>%
  st_transform(3857)




# -------------------------- Micromobility 

# --- docks
docks <- st_read(paste0("../data_raw/", city, "/GBFS/gbfs_stations.geojson")) %>%
  st_make_valid() %>%
  st_transform(3857)

# --- dockless

# # Minneapolis / San Francisco
dockless <- st_read(paste0("../data_raw/" , city, "/GBFS/gbfs_zones.geojson")) %>% 
  st_make_valid() %>%
  st_transform(3857)



# ------------------------ Plots

# urban boundaries
tm_shape(counties_boundary) + 
  tm_polygons() +
tm_shape(metro_boundary) +
  tm_fill(col = "darkred") +
tm_shape(city_boundary) +
  tm_fill(col = "steelblue2") +
tm_shape(docks) +
  tm_dots(col = "green") +
tm_shape(dockless) +
  tm_borders(col  = "yellow") +
tm_layout(main.title = "Minneapolis - Boundary Definitions",
          main.title.size = 1.2,
          main.title.color = "azure4",
          main.title.position = c("left", "top"),
          fontfamily = 'Georgia',
          frame = FALSE) +
  # add legend for the existing cycling infrastructure
tm_add_legend('fill', col= c("grey", "darkred", "steelblue2", "green", "yellow"),
                labels = c('Metro Area Counties ','Urban Boundary','City Boundary', 'Docking Stations', 'Dockless Zone'),
                title="")  -> p1

tmap_save(tm = p1, filename = paste0("../data/", city,"/Plots/geographic_scope.png"))


library(dplyr)
library(readr)
library(ggplot2)
library(readxl)
library(lubridate)


# supply constraints station dummy data 
ex_station <- readxl::read_excel("../data_raw/Other/D1_IR_1_example_supply_constraints.xlsx")

# convert date-time to time using base function `format`
ex_station <- ex_station %>% 
  mutate(Time = format(Time, format = "%H:%M"))

# convert Time to hms class for plotting
ex_station <- ex_station %>% mutate(time = lubridate::hm(Time))

# plot

cutoff <- data.frame( x = c(-Inf, Inf), y = 3, cutoff = factor(3))

ggplot(ex_station, aes(x=time, y= Bikes)) +
  geom_col(aes(fill = Bikes)) + 
  scale_fill_gradient(low = "red3", high = "seagreen3", na.value = NA) +
  # geom_hline(yintercept=3, linetype="dashed", color = "red") +
  geom_line(aes( x, y, linetype = cutoff), cutoff) +
  scale_x_time() +
  annotate("text", x = "07:35:00", y = 3, label = "Cutoff") +
  labs(title = "Bike Availability at Station", 
       x = "") +
  # xlab("") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))

ggsave("../data_raw/Other/example_Station.png", width = 5, height = 3.5)

ggsave(paste0(data, "Plots/agency_plots/sample_trips_agency.png"), width = 5, height = 3.5)

