library(tidyverse)
library(sf)
# population data from 
library(tidycensus)
# employment data from lehd lodes
library(lehdr)
# city boundaries
library(tigris)

# NOTE: "The concept of race is separate from the concept of Hispanic origin. 
#        Percentages for the various race categories add to 100 percent, and should not be 
#        combined with the percent Hispanic."


# -------------------------------  Define Variables ------------------------------- #
# # San Francisco
city <- "San Francisco"
state <- "CA"
#county <- "San Francisco"
county <- c("San Francisco", "San Mateo", "Alameda", "Contra Costa", "Santa Clara") # removed Marin

# # Minneapolis
# city <- "Minneapolis"
# state <- "MN"
# #county <- "Hennepin County"
# county <- c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington")

# ----------------------------------- GEOGRAPHIC BOUNDARY ----------------------------------- #

# data obtained from the acs covers a much bigger gegraphy than what we are interested in. 
# We are interested in the city layer. We obtain the city boundary, and later on we clip the census data to that boundary
# if the tigris api stops working, we can get the data this way https://gis.stackexchange.com/questions/10231/seeking-shapefile-of-city-boundaries-in-us

# city_boundary <- tigris::places(state) %>%  
#   tigris::filter_place(city) %>%
#   st_transform(3857)

# --- urban boundary
city_boundary <- tigris::urban_areas() %>%
  # filter to the city ussing stringr
  filter(str_detect(NAME10, city)) %>%
  st_transform(3857)

# add San Jose
if(city == "San Francisco"){
  city_boundary2 <- tigris::urban_areas() %>%
    # filter to the city ussing stringr
    filter(str_detect(NAME10, "San Jose, CA")) %>%
    st_transform(3857)
  
  city_boundary <- bind_rows(city_boundary, city_boundary2)
  
}


# create a buffer. We will include jobs outside the city boundary to reduce boundary effects
#city_boundary_buffered <- city_boundary %>% st_buffer(2000)

# ----------------------------------- BLOCK GROUP LEVEL DATA ----------------------------------- #

# -------------------------- American Community Survey (ACS)  

# ----- check what the available columns are
#asc_cols <- load_variables(year = 2019, dataset = "acs5", cache = FALSE)

# ----- select the variables we want, and choose how we want to name them 
vars_acs <- c(
  # Population
  pop_total = "B01001_001", 
  pop_white = "B02001_002",
  pop_black = "B02001_003",
  pop_american_indian = "B02001_004",
  pop_asian = "B02001_005",
  pop_hawaiian = "B02001_006",
  pop_other_race = "B02001_007",
  pop_other_two_races = "B02001_008",
  #jobs
  # mean income per capita 2019 (past 12 months total)
  income_per_capita = "B19301_001", # mean income per capita 2019 (past 12 months total)
  income_per_capita_white = "B19301A_001",
  income_per_capita_black = "B19301B_001",
  income_per_capita_american_indian = "B19301C_001",
  income_per_capita_asian = "B19301D_001",
  income_per_capita_hawaiian = "B19301E_001",
  income_per_capita_other_race = "B19301F_001",
  income_per_capita_other_two_races = "B19301G_001",
  # household income (past 12 months total)
  `income_0-9999` = "B19001_002",
  `income_10000-14999` = "B19001_003",
  `income_15000-29999` = "B19001_004",
  `income_20000-24999` = 'B19001_005',
  `income_25000-29999` = "B19001_006",
  `income_30000-34999` = "B19001_007",
  `income_35000-39999` = "B19001_008",
  `income_40000-44999` = "B19001_009",
  `income_44000-49999` = "B19001_010",
  `income_50000-59999` = "B19001_011",
  `income_60000-74999` = "B19001_012",
  `income_75000-99999` = "B19001_013",
  `income_100000-124999` = "B19001_014",
  `income_125000-149999` = "B19001_015",
  `income_150000-199999` = "B19001_016",
  `income_above-200000` = "B19001_017")



# ----- download the data at block group level

# one county
city_bg_demographic <- get_acs(state = state, 
                               county = county, 
                               geography = "block group",
                               variables = vars_acs,
                               output = "wide",
                               year = 2019,
                               geometry = TRUE) %>% 
  # remove the margin of error (moe) columns
  select(-ends_with("M")) %>%
  st_transform(3857)


# -------------------------- Employment data (LEHD - LODES) - https://lehd.ces.census.gov/data/lodes/LODES7/LODESTechDoc7.5.pdf

city_bg_employment <- lehdr::grab_lodes(state = tolower(state), 
                                        year = 2019, 
                                        agg_geo = "bg",
                                        lodes_type = "wac",
                                        segment = "S000") 


# -------------------------- Join the data
city_bg <- city_bg_demographic %>% 
  inner_join(city_bg_employment, by = c("GEOID" = "w_bg"))

# -------------------------- Clip to city/urban boundary
# city_bg_clipped <- city_bg %>% st_filter(city_boundary, #city_boundary_buffered,
#                                          .predicate = st_intersects)

# st_filter() keeps the entire polygon if it intersects, we want to crop it so we use st_intersection()
city_bg_clipped <- city_bg %>% st_intersection(st_union(city_boundary))
# save 
st_write(city_bg_clipped, paste0("../data_raw/", city, "/level_i/census_bg.geojson"), delete_dsn = TRUE)


# ----------------------------------- CENSUS BLOCK LEVEL DATA ----------------------------------- #

# -------------------- Demographic Data 

# ----- check what the available columns are
#census_cols <- load_variables(year = 2020, dataset = "pl", cache = FALSE) 


# ----- select the variables we want, and choose how we want to name them 
vars_census <- c(pop_total = "P1_001N", # Population
                 pop_white = "P1_003N",
                 pop_black = "P1_004N",
                 pop_american_indian = "P1_005N",
                 pop_asian = "P1_006N",
                 pop_hawaiian = "P1_007N",
                 pop_other = "P1_008N",
                 pop_two_or_more_races = "P1_009N")

# ----- download the data

city_block_demographic <- get_decennial(state = state,
                                        county = county,
                                        geography = "block",
                                        variables = vars_census,
                                        output = "wide",
                                        year = 2020,
                                        geometry = TRUE) %>% 
  st_transform(3857)



# -------------------------- Employment data (LEHD - LODES) 

city_block_employment <- lehdr::grab_lodes(state = tolower(state), 
                                           year = 2019, 
                                           agg_geo = "block",
                                           lodes_type = "wac",
                                           segment = "S000")


# -------------------------- Join the data
city_block <- city_block_demographic %>% 
  left_join(city_block_employment, by = c("GEOID" = "w_geocode"))

# -------------------------- Clip to city boundary
city_block_clipped <- city_block %>% st_filter(city_boundary_buffered, 
                                            .predicate = st_intersects)


# save 
st_write(city_block_clipped, paste0("../data_raw/", city, "/level_i/census_block.geojson"), delete_dsn = TRUE)

