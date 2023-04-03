###################################################################################################
###    The purpose of this script is to edit the gtfs calendar.txt . We have many gtfs feeds    ###
###    for a city. We get these feeds from different sources and they may be valid for          ###
###    different periods (start and end dates). We edit the start and end dates so that we can  ###
###    make sure that all the fields are valid for our chosen analysis date                     ###   
###    NOTE: we edit the dates only                                                             ###
###################################################################################################

library(tidyverse)
library(gtfstools)
library(lubridate)

# city <- "San Francisco"
# city <- "Minneapolis"
# city <- "Mexico City"
city <- "Cairo"

# ----- 1. define relative path of folder that contains gtfs feeds
feed_dir <- paste0("../data_raw/", city, "/GTFS")

# ----- 2. get relative paths of all gtfs feeds
feeds <- dir(feed_dir, ".zip$", full.names = TRUE)

# ----- 3. define start and end dates
start_date <- lubridate::as_date("2021-12-01") # December 2021
end_date <- lubridate::as_date("2022-03-01")   # March 2022

# ----- 4. Edit feeds
for (i in 1:length(feeds)){
  # read in the gtfs feed
  gtfs_feed <- gtfstools::read_gtfs(path = feeds[i])
  # edit start and end dates in calendar.txt
  if ('calendar' %in% names(gtfs_feed)){
    gtfs_feed$calendar$start_date <- start_date
    gtfs_feed$calendar$end_date <- end_date
    # status
    print(paste0("Edited feed: ", i))
    # save the feed, overwriting the existing feed
    gtfstools::write_gtfs(gtfs = gtfs_feed, 
                          path = feeds[i])
    # status
    print(paste0("Saved feed: ", i))
    
  }
}

