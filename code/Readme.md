# Code Documentation

## Instructions on applying the method to a new city

1. clone the repo to your local machine to use the same file structure
2. download the following raw data for each city you wish to apply the accessibility framework to (scripts to download and process data from online sources can be found in 1_Preprocessing)
    - **OSM Road Network**: 
        - Sourcing the data: There are many ways to download osm data. The [HOT OSM Export Tool](https://export.hotosm.org/en/) provides a user-friendly GUI. Alternatively, you could use a package like [osmextract](https://docs.ropensci.org/osmextract/index.html) if you prefer a reproducible pipeline. 
        - File path This should be added to data_raw/<City_Name>/PBFs
    - **GTFS feeds** for public transit
        - File path: data_raw/<City_Name>/GTFS
        - Validating the feeds: Use [0_edit_gtfs_calendar.R](https://github.com/transportforcairo/wri-numo_access-analysis/blob/main/code/1_Preprocessing/0_edit_gtfs_calendar.R): When using a multimodal routing engine (such as opentripplanner or r5) you need to specify the date and time of your query (check the [departure_datetime argument](https://ipeagit.github.io/r5r/reference/travel_time_matrix.html) in r5r). This should match the dates in the calendar.txt of the GTFS feeds. If you are dealing with multiple feeds, they could have different start and end dates, especially if they are not published on a regular basis. This script allows you to load all the gtfs feeds in your directory and edit the start and end dates for calendar.txt to match a date that you specify. The feeds are then all valid for the same days
    - **GBFS** for bike-sharing/micromobility: 
        - Sourcing the data: We use the [1_extract_get_gbfs](https://github.com/transportforcairo/wri-numo_access-analysis/blob/main/code/1_Preprocessing/1_extract-get_gbfs.R) script to download gbfs data from the [MobilityData gbfs repository](https://github.com/MobilityData/gbfs/blob/master/systems.csv). If you follow the link, you will see a csv file. If you have micromobility data that is not in this list, then you will have to add it manually to the repository
        - File path: data_raw/<City_Name>/GBFS 
    - **Census data**
        - Description: You need a spatial layer for your chosen city. The layer should have administrative boundaries, as well as a column for population and a column for employment
        - Sourcing the data: The script [1_extract-us_census_data.R](https://github.com/transportforcairo/wri-numo_access-analysis/blob/main/code/1_Preprocessing/1_extract-us_census_data.R) can be used to download census data for any US city. It relies on the [tidycensus](https://github.com/walkerke/tidycensusle), [lehdr](https://github.com/jamgreen/lehdr), and [tigris](https://github.com/walkerke/tigris) r packages. 
        - File path: data_raw/<City_Name>/level_i
    - **Elevation data**
        - Description: This data is needed for bicycle/micromobility routing. The routing engine we use (r5r) can consider elevation if provided with a .tif file
        - Sourcing the data: The script [1_extract-download_elev.R](https://github.com/transportforcairo/wri-numo_access-analysis/blob/main/code/1_Preprocessing/2.2_extract-download_elev.R) uses the [elevatr](https://github.com/jhollist/elevatr) package to download elevation data for any area you specify
    - **Speed data** (OPTIONAL)
        - Description: Data that has actual speeds on road segments. It should be a csv with the OSM way ID and the speed
        - Sourcing the data: We used the Uber Movement (open) and Mapbox (proprietary) data for our analysis. The analysis can be run without this data; the routing engine will use freeflow speeds based on speed limits.
3. Run the transformation scripts in 1_Preprocessing. It is best to run them in the order they are listed below:
    - Optional:
        - [2.0_variablehexgrid.R](https://github.com/transportforcairo/wri-numo_access-analysis/blob/main/code/1_Preprocessing/2.0_variable_hexgrid.R): Create a variable resolution hexagon grid for use in creating a travel time matrix. Refer to the *Prepocessing* section of the technical appendix on why this can be useful
    - Necessary
        - [2.1_transform-census2hexagons.R](https://github.com/transportforcairo/wri-numo_access-analysis/blob/main/code/1_Preprocessing/2.1_transform-census2hexagons.R): Project data from a census administrative boundary layer to the hexagon layer which is used to create the OD matrix. A hexagon layer is prefered as administrative boundaries can vary greatly in size
        - [2.3_transform-gbfs2zones.R](https://github.com/transportforcairo/wri-numo_access-analysis/blob/main/code/1_Preprocessing/2.3_transform-gbfs2zones.R): Project the gbfs data (docked: station locations, dockless: service geography) onto the hexagon layer
        - [2.4b_transform-validate_gtfs_feeds.R](https://github.com/transportforcairo/wri-numo_access-analysis/blob/main/code/1_Preprocessing/2.4b_transform-validate_gtfs_feeds.R): Use the command line tool ["gtfstidy"](https://github.com/patrickbr/gtfstidy) to validate all GTFS feeds you have sourced
4. Run the [Maxspeed_Setter_wfunctions](https://github.com/transportforcairo/wri-numo_access-analysis/blob/main/code/2_pbf_augmenter/Maxspeed_setter_wfunctions.py) script in 2_pbf_augmenter. Refer to the chapter on *Modelling Realistic Car Travel Times* in the technical appendix for an explanation of the logic.
    - The output should be added to data_raw/<City_Name>/PBFs
5. Run the scripts in 3_Analysis to compute travel time and accessibility and supply contraints
    - Necessary
        - [3.1_analysis-travel_time_r5.R](https://github.com/transportforcairo/wri-numo_access-analysis/blob/main/code/3_Analysis/3.1_analysis-travel_time_r5.R): Calculate travel time matrices for different (access, egress, main) mode combinations 
        - [3.2_analysis-travel_time_scenarios.R](https://github.com/transportforcairo/wri-numo_access-analysis/blob/main/code/3_Analysis/3.2_analysis-travel_time_scenarios.R): Use the travel time matrices output from above to calculate travel times for the mode combinations in the study. Refer to the chapter on *Modelling Intermodal Travel Times* in the Appendix to understand the logic
        - [3.3_accessibility_analysis.R](https://github.com/transportforcairo/wri-numo_access-analysis/blob/main/code/3_Analysis/3.3_analysis-accessibility.R): Use the output of 3.2 to calculate a cumulative opportunity measure of accessibility at different travel time thresholds (15, 30, 45, and 60 minutes). Refer to the chapter on *Multimodal Accessibility Analysis* in the technical appendix for an explanation of the method
    - Optional (not dependant on 3.1, 3.2, 3.3)
        - [3.3b-analysis-accessibility_supply_constraints.R](https://github.com/transportforcairo/wri-numo_access-analysis/blob/main/code/3_Analysis/3.3b-analysis-accessibility_supply_constraints.R): This script calculates detailed itineraries for each OD pair. The output is then used to calculate accessibility given micromobility supply constraints, as described in the chapter *Supply Constraints of Shared Micromobility Systems* in the technical appendix. The calculations rely on proprietary MDS data (which we obtained from micromobility providers for specific cities for our study). The data is not available for public use, but we keep the script in the repository as inspiration for anyone wishing to do the same analysis with MDS data
6. Repurpose the Equity anlaysis iPython Notebooks for SF or Minneapolis for your city. The analysis can be redone for any US city. It only requires the accessibility results from 3.3


## Script parameters

There are many parameters in the scripts. Some should not be edited, while some you need to edit. Below is a description of all parameters that need editing and how you should edit them:

- [0_edit_gtfs_calendar.R](https://github.com/transportforcairo/wri-numo_access-analysis/blob/main/code/1_Preprocessing/0_edit_gtfs_calendar.R)
    - city: Your chosen city
    - start_date / end_date: the dates you want the feed to be valid for
- [1_extract_get_gbfs](https://github.com/transportforcairo/wri-numo_access-analysis/blob/main/code/1_Preprocessing/1_extract-get_gbfs.R)
    - city: Your chosen city
- [1_extract-us_census_data.R](https://github.com/transportforcairo/wri-numo_access-analysis/blob/main/code/1_Preprocessing/1_extract-us_census_data.R)
    - city: your chosen city 
    - state: initials of the state the city is in
    - county: the counties you want to include
- [2.0_variable_hexgrid.R](https://github.com/transportforcairo/wri-numo_access-analysis/blob/main/code/1_Preprocessing/2.0_variable_hexgrid.R)
    - city: your chosen city
    - layer_name: the name of the administrative layer with the variable that you will base the hexagon resolution on (population, jobs etc)
    - grid_sizes: a list of hexagon diameters that will be used to create the layer
    - existing_pop_column: the name of the column in the "city_geom" feature that includes the population variable (you can use another variable)
    - density_threshold: check the documentation in the script

- [2.1_transform-census2hexagons.R](https://github.com/transportforcairo/wri-numo_access-analysis/blob/main/code/1_Preprocessing/2.1_transform-census2hexagons.R)
    - city: your chosen city
    - layer_name: the administrative layer with the census data
    - hex_layer_name: the hexagon layer created from 2.0_variable_hexgrid.R
    - variable_hexgrid: TRUE if we want to use the variable hexgrid created in 2.0, FALSE otherwise. We recommend this is set to TRUE
    - grid_size: If we will create a fixed resolution hexgrid, what diameter should the hexagons have (in meters)

We have two options: 
    - OPTION A: create a fixed resolution hexgrid and use that 
    - OPTION B: use the variable resolution hexgrid we created
- [2.3_transform-gbfs2zones.R](https://github.com/transportforcairo/wri-numo_access-analysis/blob/main/code/1_Preprocessing/2.3_transform-gbfs2zones.R)
    - city: chosen city
    - docked_provider_name/dockless_provider_name: edit these variables based on the name of your provider. This isn't necessary if you got the gbfs data through the 1_extract_get_gbfs.R script(the provider column will exist if so)
    - *census_matched* function: replace argument for docks/dockless should be NULL if the layer does not exist (city doesn't have the service)
    - *census_matched_neighbors* function: docked_col / dockless_col arguments should be the name of the provider you want to use for the anlysis. If there are more than one docked services provider in a city, choose one (same appllies for dockless). The name should match exactly with the name of the provider in the "provider" column of the docks/dockless layer
- [2.4b_transform-validate_gtfs_feeds.R](https://github.com/transportforcairo/wri-numo_access-analysis/blob/main/code/1_Preprocessing/2.4b_transform-validate_gtfs_feeds.R)
    - city: chosen city
- [Maxspeed_Setter_wfunctions](https://github.com/transportforcairo/wri-numo_access-analysis/blob/main/code/2_pbf_augmenter/Maxspeed_setter_wfunctions.py). Change the following input variables in the script: 
     - road_path: the path to the .xml OSM road network 
     - newwayID_seed: for naming, can be left at 1000
     - output_file_name: a string to name the output .xml file
     - avgspeed: this is the real speed from Uber/Mapbox as csv
     - cityunits: string 'mph' or 'kph'
     - uncomment the colnames and speedsDFpeak variables for the appropriaate data source (Uber or Mapbox) on lines 40,41, and 44
- [3.1_analysis-travel_time_r5.R](https://github.com/transportforcairo/wri-numo_access-analysis/blob/main/code/3_Analysis/3.1_analysis-travel_time_r5.R): This script could be run twice, once with an unedited pbf file, and once with a pbf file produced by *Maxspeed_setter_wfunctions* (if you have road segment speed data)
    - city: chosen city
    - congested: "yes" if running analysis using an osm.pbf with congested speeds, "no" if using an unedited .pbf file
    - pbf_files tribble: create a row with you city name, name of freeflow pbf file, name of congested pbf file (use the existing table as a reference, and remove the example rows if you want to)
    - departure_datetime: edit this so it matches the validity period of your gtfs feeds / the day you want to run the analysis (0_edit_gtfs_calendar.R can help with this)
    - time_window / percentiles / max_walk_distance: check r5r documentation and edit if you want
- [3.2_analysis-travel_time_scenarios.R](https://github.com/transportforcairo/wri-numo_access-analysis/blob/main/code/3_Analysis/3.2_analysis-travel_time_scenarios.R)
    - city: chosen city
    - od_real_speeds: 1 if we have an od matrix based on congested speeds, 0 if we don't
    - docked: 1 if we have a docked service, 0 if we don't 
    - dockless: 1 if we have a dockless service, 0 if we don't 
- [3.3_accessibility_analysis.R](https://github.com/transportforcairo/wri-numo_access-analysis/blob/main/code/3_Analysis/3.3_analysis-accessibility.R)
    - city: chosen city
    - city metadata tribble: use the existing table as a reference. The jobs column should match the name of the jobs column in the city_geom layer
    - get_access_multiple function: 
        - cutoff_times: can take a list of numbers. If you want accessibility at 60, 45, and 30 minutes, use c(30, 45, 60)
        - job_col: the name of the job column in single brackets
- [3.3b-analysis-accessibility_supply_constraints.R](https://github.com/transportforcairo/wri-numo_access-analysis/blob/main/code/3_Analysis/3.3b-analysis-accessibility_supply_constraints.R): See the description for this script above
- [Equity_Minneapolis.ipynb](https://github.com/transportforcairo/wri-numo_access-analysis/blob/main/code/4_Equity/Equity_Minneapolis.ipynb) and [Equity_SF.ipynb](https://github.com/transportforcairo/wri-numo_access-analysis/blob/main/code/4_Equity/Equity_SF.ipynb)
