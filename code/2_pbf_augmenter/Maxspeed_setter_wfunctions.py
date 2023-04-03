

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

######################################## INPUTS ######################################## 
road_path = '/Users/askalila/Documents/TfC Offline Work/numo/otp/Clutter/Final networks/original/Census_bg_April20.osm'
newwayID_seed = 1000 
output_file_name = 'census_bg_april20.osm'

### 1 #### Choose one of the following cities:

### Mexico:
# avgspeed = '/Users/askalila/Documents/GitHub/32_wri_numo/4_Tool_Development/2_transform/avg_speeds/mexico_output.csv'
# cityunits = 'kph' # string 'mph' or 'kph'

### Cairo:
# ## check points to copy in line 13 of next block [1660,16600,83000,182000, 270000, 360000]
# avgspeed = '/Users/askalila/Documents/GitHub/32_wri_numo/4_Tool_Development/2_transform/avg_speeds/cairo_output.csv'
# cityunits = 'kph' # string 'mph' or 'kph'

### San Francisco
avgspeed = '/Users/askalila/Documents/GitHub/32_wri_numo/4_Tool_Development/2_transform/avg_speeds/sf_output.csv'
cityunits = 'mph' # string 'mph' or 'kph'

### 2 #### Choose one of the following speed sources

### for mapbox
# colnames=['osm_start_node_id','osm_end_node_id', 'speed_kph_mean'] 
# speedsDFpeak = pd.read_csv(avgspeed, names=colnames, header=0)

##$ for uber
speedsDFpeak = pd.read_csv(avgspeed)

######################################## END INPUTS ##################################### 


######### FUNCTIONS

def CopyNodes (originalway, nodesequence, realspeed, units, newwayID, verbose = 'no'):
    """
    Purpose: this function creates a new way with the nodes that have speed and adds attribute to the old and new ways to mark them as synthetic
    
    input:
        originalway: original way element object
        nodesequence: list of node IDs to be copied as strings in list, in the correct direction
        realspeed: mean speed as string or 'no' to skip changing maxspeed tag
        units: string of 'mph' or'kph'
        newwayID: string with a new way ID number
        
    no outputs
    """

    newway = ET.Element('way', attrib = dict(originalway.items())) #creates new way with same attributes

    # add nodes as nd children
    
    for i,anode in enumerate(nodesequence):    
        NodeRef = ET.Element('nd', attrib = {'ref':anode})
        newway.append(NodeRef)
        

    # adds attributes and tags to new way
    newway.set('id', newwayID) #replace orig way id with new id
    newway.set('synthetic','yes')
    newway.extend(originalway.findall("./tag")) #copies over all the exisitng tags

    if realspeed != 'no': 
    
        # add the maxspeed tag from real speed or replace current maxspeed with real speed
        if newway.find('tag/[@k="maxspeed"]') is None:
            speedtag = ET.Element('tag', attrib = {'k':'maxspeed','v':realspeed})
            newway.append(speedtag)   # adds a tag element under way
        else:
            newway.find('tag/[@k="maxspeed"]').attrib['v'] = realspeed

    
    global root
    root.append(newway) # adds new way to the xml
    if verbose == 'yes': print('adding new way with ID',newwayID)
    
    
def CheckNodesForSpeeds (wayID, originalway, units, verbose = 'no'):
    """
    This function will check the Speeds Dataframe (global variable) for speeds between node pairs in the way.
    
    Global variables that need to be present:
        root
        speedsDFpeak
    inputs: 
        wayID: original way element's ID as string
        nodelist: the way's list of node reference ids as strings
        units: 'mph' or 'kph' string
        newwayID_seed: starting number for new way's to iterate over by 1
    
    function Calls another function to do:
        - creates new way with nodes and speed and copies all metadata and non-speed tags
        - removes the copied nodes from the original way
        - adds attribute to newway "synthetic = yes"
        
        - adds attribute to origin way "Checked_for_speed = yes"
        
    no outputs
    """
    global newwayID_seed

    #create nodelist of node id strings and similar list of floats
    nodelist = []
    nodelistfloat = []
    for node in originalway.iter('nd'):
        
        ref = node.attrib['ref']
        nodelist.append(ref)
        nodelistfloat.append(int(ref))

    # get reverse of nodelist
    reverseNodelistfloat = list(reversed(nodelistfloat))
    reverseNodelist = list(reversed(nodelist))

    # get only speeds with origin AND Destination nodes in the original way
    relevantSpeeds = speedsDFpeak.loc[(speedsDFpeak.osm_start_node_id.isin(nodelistfloat)) & (speedsDFpeak.osm_end_node_id.isin(nodelistfloat))]
    try: 
        relevantSpeedsShort = relevantSpeeds[['osm_start_node_id','osm_end_node_id','speed_mph_mean']]
    except:
        relevantSpeedsShort = relevantSpeeds[['osm_start_node_id','osm_end_node_id','speed_kph_mean']]

        
    nodepairdict = dict()
    
    

    # create new ways from the relevant speeds
    
    for nodes in relevantSpeedsShort.iterrows():
        pair = list(nodes[1])
        startnode, endnode, realspeed_float = pair[0],pair[1], pair[2]
        realspeed = str(realspeed_float)

        # check if its the direction of node sequence or reverse
        if nodelistfloat.index(endnode) > nodelistfloat.index(startnode): #correct sequence     
            startIndex, endIndex = nodelistfloat.index(startnode), nodelistfloat.index(endnode)
            CopyNodes(originalway, nodelist[startIndex:endIndex+1],realspeed,units,str(newwayID_seed))
            newwayID_seed += 1
            nodepairdict[startIndex] = endIndex

        elif nodelistfloat.index(endnode) < nodelistfloat.index(startnode): # reverse
            # check if the reverse nodepair belongs to another way (from DF in Uber, from xml tree in mapbox), 
            # if yes then skip this block
#             try: # check if the speed DF has a different way ID for the reverse node direction (n/a to mapbox speed)
#                 samewayID = wayID == str(list(speedsDFpeak.loc[(speedsDFpeak.osm_start_node_id == startnode) & (speedsDFpeak.osm_end_node_id == endnode)]['osm_way_id'])[0])
#             except: # for mapbox, check if the root has only way with the start and end node pair
                
# #                 This did not work becuase it is very slow. Skipping it will not have adverse effects on the result
# #                 samewayID = len(root.findall("./way/nd/[@ref ='"+ str(round(startnode)) +"'].." and "./way/nd/[@ref ='"+ str(round(endnode)) +"']..")) == 1
#                 samewayID = True
              
            samewayID = True
            if samewayID:
                startIndex, endIndex = nodelistfloat.index(startnode), nodelistfloat.index(endnode)
                revstartIndex, revendIndex = reverseNodelistfloat.index(startnode), reverseNodelistfloat.index(endnode)
                CopyNodes(originalway, reverseNodelist[revstartIndex:revendIndex+1],realspeed,units,str(newwayID_seed))
                newwayID_seed += 1

        else: raise Exception('start node or end node not in way nodelist')


    ## remove nodes from original way

    startPos = 0
    endPos = len(nodelist)-1

    data_items = nodepairdict.items()
    data_list = list(data_items)
    nodepairdf = pd.DataFrame(data_list,columns=['start','end'])

    # first, delete nodes from segments at the start of the way

    while len(nodepairdf[nodepairdf.start == startPos]) > 0: #loop over this until it all start segments are removed from original way

        start = startPos
        end = nodepairdf[nodepairdf.start == startPos].values[0][1]

        for i in range(start, end): # delete nodes in orig way from startPos to end-1
            nodeID = nodelist[i]
            try: originalway.remove(originalway.find("./nd/[@ref='"+nodeID+"']"))
            except: pass
            if verbose == 'yes': print('removed node index',i)

        nodepairdf = nodepairdf.drop(nodepairdf[nodepairdf.start == startPos].index[0])  # delete this row from the df
        startPos = end


    # second, delete nodes from segments at the end of the way
    while len(nodepairdf[nodepairdf.end == endPos]) > 0:  
        end = endPos
        start = nodepairdf[nodepairdf.end == endPos].values[0][0]

        for i in range(start+1, end+1): # delete nodes in orig way from startPos +1 to endPos
            nodeID = nodelist[i]
            try: originalway.remove(originalway.find("./nd/[@ref='"+nodeID+"']"))
            except: pass
            if verbose == 'yes': print('removed node index',i)

        nodepairdf = nodepairdf.drop(nodepairdf[nodepairdf.end == endPos].index[0]) # delete this row from the df
        endPos = start



    copypairs =[]
    sortedNodepairdf = nodepairdf.sort_values(by=['start']).values
    
    # third, the remaining rows should only be nodes of middle segments. delete the middle nodes

    for i, pair in enumerate(sortedNodepairdf): 

        if pair[1] < pair[0]: continue # ignore reverse pairs
        if len(copypairs) == 0: 
            copypairs.append((startPos,pair[0]))
            lastEnd = pair[1]
        if i == len(sortedNodepairdf)-1: copypairs.append((pair[1],endPos))
        if pair[0] > lastEnd: copypairs.append((lastEnd,pair[0]))
        if i == len(sortedNodepairdf)-1 and pair[1] < endPos: copypairs.append((pair[1],endPos))

        for i in range(pair[0]+1, pair[1]): # delete nodes in orig way from startPos +1 to endPos -1
            nodeID = nodelist[i]
            try: originalway.remove(originalway.find("./nd/[@ref='"+nodeID+"']"))
            except: pass
            if verbose == 'yes': print('removed node index',i)

        lastEnd = pair[1]

    if verbose == 'yes': print('copy pairs:',copypairs)
    # create copynodes with no realspeed for all pairs in copypairs 
    for pair in copypairs:
        nodesequence = []
        for i in range(pair[0],pair[1]+1): nodesequence.append(nodelist[i])
        CopyNodes(originalway, nodesequence,'no',units, str(newwayID_seed))
        newwayID_seed += 1

######################################## END FUNCTIONS ##################################### 

print('Reading inputs...')

# Setting up Element Tree to read in the osm file
tree = ET.parse(road_path)
root = tree.getroot()

number_ofways = len(root.findall('./way'))
print('Total number of ways in the osm:', number_ofways)

# creating checKpoints for the progress printer
checkpoints = []
for j in [0.01,0.05, 0.1, 0.5, 0.75, 0.9, 0.99]:
    checkpoints.append(round(number_ofways*j))

## Run the functions on all the ways in the osm xml tree
print('Starting maxspeed setting...')
start = timer()
for i,originalway in enumerate(root.findall("./way")):
    wayID = originalway.attrib['id']
    CheckNodesForSpeeds(wayID, originalway, cityunits)
    
    
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

print('Done!')
