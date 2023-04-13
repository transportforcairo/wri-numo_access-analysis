# Code 

## Instructions on applying the method to a new city

1. download the repo to your local machine to use the same file structure
2. download the following raw data for each city you wish to apply the accessibility framework to (scripts to download and process data from online sources can be found in 1_Preprocessing)
    - OSM Road Network
    - GTFS and GBFS for transit and bike-sharing/micromobility
    - Census data
    - Hexagon Grids
    - Elevation data
3. Run the transformation scripts in 1_Preprocessing
4. Download real speed data for all roads in your study area (we used Uber Movement or Mapbox)
5. Run the Maxspeed_Setter_wfunctions script in 2_pbf_augmenter
6. Run the scripts in 3_Analysis to compute travel time and accessibility and supply contraints
7. Repurpose the iPython Notebooks for SF or Minneapolis for your city 
