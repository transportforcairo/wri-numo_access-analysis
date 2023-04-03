

############################################################################################
#																						   #
#                    The purpose of this script is to apply real speeds from uber or 	   #
#   							mapbox to the osm road network in .osm format			   #
#																						   #
############################################################################################

####### libraries
import xml.etree.ElementTree as ET
import pandas as pd
import gc
from timeit import default_timer as timer
import numpy as np
from copynodefunctions import *

######################################## INPUTS ######################################## 
road_path = r'D:\_TfC\_melegy\MyScratch\osmosis-0.48.3\sf.osm'

output_file_name = 'trial_2.osm'

### 1 #### Choose one of the following cities:

### Mexico:
# avgspeed = '/Users/askalila/Documents/GitHub/32_wri_numo/4_Tool_Development/2_transform/avg_speeds/mexico_output.csv'
# cityunits = 'kph' # string 'mph' or 'kph'

### Cairo:
# ## check points to copy in line 13 of next block [1660,16600,83000,182000, 270000, 360000]
# avgspeed = '/Users/askalila/Documents/GitHub/32_wri_numo/4_Tool_Development/2_transform/avg_speeds/cairo_output.csv'
# cityunits = 'kph' # string 'mph' or 'kph'

### San Francisco
avgspeed = r'D:\_TfC\Repos\_Projects\32_wri_numo\4_Tool_Development\2_transform\avg_speeds\sf_output.csv'
cityunits = 'mph' # string 'mph' or 'kph'
### 2 #### Choose one of the following speed sources

### for mapbox
# colnames=['osm_start_node_id','osm_end_node_id', 'speed_kph_mean'] 
# speedsDFpeak = pd.read_csv(avgspeed, names=colnames, header=0)

##$ for uber
speedsDFpeak = pd.read_csv(avgspeed)

######################################## END INPUTS ##################################### 


print('Reading inputs...')

# Setting up Element Tree to read in the osm file
tree = ET.parse(road_path)
root = tree.getroot()

number_ofways = len(root.findall('./way'))
print('Total number of ways in the osm:', number_ofways)

# creating checpoints for the progress printer
checkpoints = []
for j in [0.01,0.05, 0.1, 0.5, 0.75, 0.9, 0.99]:
    checkpoints.append(round(number_ofways*j))

## Run the functions on all the ways in the osm xml tree
print('Starting maxspeed augmenting...')
start = timer()
copynodesObj = SpeedSetter()
for i,originalway in enumerate(root.findall("./way")):
    wayID = originalway.attrib['id']
    root = copynodesObj.CheckNodesForSpeeds(wayID, originalway, cityunits, speedsDFpeak, root)
    
    
    if i in checkpoints: 
        bookmark = timer()
        progress = int(i/(number_ofways-1)*100)
        print(f'{progress}% completed in {np.round((bookmark - start)/60)} minutes...')

        
    # delete original ways with ref nodes 1 or fewer
    if len(originalway.findall("./nd")) < 2: root.remove(originalway)
        
end = timer()
print(f'100% completed in {np.round((end- start)/60)} minutes!')

# output xml to .osm file
tree.write(output_file_name, encoding="utf-8",xml_declaration=True)

