###################################################################################################
###    The purpose of this script is to validate gtfs feeds that will be used for routing. It   ###
###    used the command line tool "gtfstidy" (https://github.com/patrickbr/gtfstidy), written   ###
###    in GO, to loop over all feeds in a specified directory and validate them. If the gtfs    ###
###    feed is invalid, it is validated in place (i.e. it is replaced with a valid feed that    ###
###    has the same name                                                                        ### 
###################################################################################################

city <- "San Francisco"
# ----- 1. define relative path of folder that contains gtfs feeds
feed_dir <- paste0("../data_raw/", city, "/GTFS")

# ----- 2. get relative paths of all gtfs feeds
feeds <- dir(feed_dir, ".zip$", full.names = TRUE)

# ----- 3.  define GO path. this needs to be done every time (unless I can figure out a way to set it permanently)
go_path_1 <- "export GOPATH=/Users/$USER/go"
go_path_2 <- "export PATH=$GOPATH/bin:$PATH"

# if running in vm
#go_path_1 <- "export GOPATH=$HOME/Projects/Go"
#go_path_2 <- "export PATH=$GOROOT/bin:$GOPATH/bin:$PATH"

# ----- 4. convert relative gtfs feed path to absolute path. (to pass onto the terminal)
gtfs_feed_path <- normalizePath(feeds[1])

# ----- 5. validate the feed using gtfstidy

# -- (a) gtfstidy command to validate a gtfs feed in place (gtfstidy -D, <old feed name> , -o ,<new feed name>)
gtfstidy_command <- paste0("gtfstidy ", "-D '", gtfs_feed_path, "' -o '", gtfs_feed_path, "'")

# -- (b) pass the concatenated command onto the terminal. 

# All commands need to be passed in one system call https://stackoverflow.com/questions/5745886/r-and-system-calls

system(paste0(go_path_1, " ; ", go_path_2, " ; ", gtfstidy_command))



# -------------------------------------- Wrap logic in function -------------------------------------- #

# This function takes the RELATIVE PATH of the folder containing the GTFS feeds, and validates each feed in place

validate_feeds <- function(rel_folder_dir){
  
  # ----- 2. get relative paths of all gtfs feeds
  feeds <- dir(rel_folder_dir, ".zip$", full.names = TRUE)
  
  # check if the path has gtfs feeds 
  if(length(feeds) == 0){
    print("There are no GTFS feeds in this directory! Make sure that the directory is correct and that each feed is in one .zip file")
  } else{
    # ----- 3.  define GO path. this needs to be done every time (unless I can figure out a way to set it permanently)
    go_path_1 <- "export GOPATH=/Users/$USER/go"
    go_path_2 <- "export PATH=$GOPATH/bin:$PATH"
    # if running in vm
    #go_path_1 <- "export GOPATH=$HOME/Projects/Go"
    #go_path_2 <- "export PATH=$GOROOT/bin:$GOPATH/bin:$PATH"
    
    for(i in 1:length(feeds)){
      print(paste0("Validating feed ", i))
      # ----- 4. convert relative gtfs feed path to absolute path. (to pass onto the terminal)
      gtfs_feed_path <- normalizePath(feeds[i])
      
      # ----- 5. validate the feed using gtfstidy
      
      # --- (a) gtfstidy command to validate a gtfs feed in place (gtfstidy -D, <old feed name> , -o ,<new feed name>)
      gtfstidy_command <- paste0("gtfstidy ", "-D '", gtfs_feed_path, "' -o '", gtfs_feed_path, "'")
      
      # --- (b) pass the concatenated command onto the terminal. 
      # All commands need to be passed in one system call https://stackoverflow.com/questions/5745886/r-and-system-calls
      system(paste0(go_path_1, " ; ",
                    go_path_2, " ; ",
                    gtfstidy_command))
      
      print(paste0("Feed ", i, " done"))
      
    }
  }
}


# ----------------------- Run the function

validate_feeds(rel_folder_dir = paste0("../data_raw/", city, "/GTFS"))







