import xml.etree.ElementTree as ET
import pandas as pd
import gc

from sqlalchemy import column

road_path = r'D:\_TfC\_melegy\MyScratch\osmosis-0.48.3\sf.osm'
speed_path = r'D:\_TfC\temp\NUMO\sf_hod_16.csv'

o_nd_col = 'osm_start_node_id'
d_nd_col = 'osm_end_node_id'
speed_col = 'speed_mph_mean'

newwayID = 11000

speeds_df = pd.read_csv(speed_path, names=[o_nd_col, d_nd_col, speed_col])

tree = ET.parse(road_path)
root = tree.getroot()

def CopyNodesSpeed (originalway, nodesequence, realspeed, units, newwayID):
    """
    Purpose: this function creates a new way with the nodes that have speed and adds attribute to the old and new ways to mark them as synthetic
    
    input:
        original way element object
        list of node IDs with real speed as strings in list, in the correct direction
        mean speed as string
        units: string of 'mph' or'kph'
        
    no outputs
    """

    newway = ET.Element('way', attrib = dict(originalway.items())) #creates new way with same attributes

    # add nodes as nd children
    
    for i,anode in enumerate(nodesequence):    
        NodeRef = ET.Element('nd', attrib = {'ref':anode})
        newway.append(NodeRef)
        

    # adds attributes and tags to new way
    newway.set('id', str(newwayID)) #replace orig way id with new id
    newway.set('synthetic','yes')
    newway.extend(originalway.findall("./tag")) #copies over all the exisitng tags


     # add the maxspeed tag from real speed or replace current maxspeed with real speed
    if newway.find('tag/[@k="maxspeed"]') is None:
        speedtag = ET.Element('tag', attrib = {'k':'maxspeed','v':realspeed})
        newway.append(speedtag)   # adds a tag element under way
    else:
        newway.find('tag/[@k="maxspeed"]').attrib['v'] = realspeed


    root.append(newway) # adds new way to the xml
    
    newwayID += 1

def CheckNodesForSpeeds (wayElement, nodelist, untis):
    """
    This function will check the Speeds Dataframe (global variable) for speeds between node pairs in the way.
    
    Global variables that need to be present:
        root
        speeds_df
    inputs: 
        wayElement: original way element object
        nodelist: the way's list of node reference ids as strings
    
    function Calls another function to do:
        - creates new way with nodes and speed and copies all metadata and non-speed tags
        - removes the copied nodes from the original way
        - adds attribute to newway "synthetic = yes"
        
        - adds attribute to origin way "Checked_for_speed = yes"
        
    no outputs
    """
    print('smth')

for way in root.findall('./way'):

    #create nodelist of node id strings and similar list of floats
    nodelist = []
    nodelistfloat = []
    for node in way.iter('nd'):
        ref = node.attrib['ref']
        nodelist.append(ref)
        nodelistfloat.append(int(ref))

    # get reverse of nodelist
    reverseNodelistfloat = list(reversed(nodelistfloat))
    reverseNodelist = list(reversed(nodelist))

    # get only speeds with origin AND Destination nodes in the original way
    relevantSpeeds = speeds_df.loc[(speeds_df.osm_start_node_id.isin(nodelistfloat)) & (speeds_df.osm_end_node_id.isin(nodelistfloat))]

    nodepairtuples = []
    # create new ways from the relevant speeds
    for nodes in relevantSpeeds.iterrows():
        pair = list(nodes[1])
        startnode, endnode, realspeed_float = pair[0],pair[1], pair[2]
        realspeed = str(realspeed_float)

        # check if its the direction of node sequence or reverse
        if nodelistfloat.index(endnode) > nodelistfloat.index(startnode): #correct sequence     
            startIndex, endIndex = nodelistfloat.index(startnode), nodelistfloat.index(endnode)
            CopyNodesSpeed(way, nodelistfloat[startIndex:endIndex+1],realspeed,'mph',newwayID)
            nodepairtuples.append((startIndex, endIndex))
            
        elif nodelistfloat.index(endnode) < nodelistfloat.index(startnode): # reverse
            startIndex, endIndex = reverseNodelistfloat.index(startnode), reverseNodelistfloat.index(endnode)
            CopyNodesSpeed(way, reverseNodelist[startIndex:endIndex+1],realspeed,'mph',newwayID)
            
        else: raise Exception('start node or end node not in way nodelist')

# elem = root.find("way[@id='29222667']")

"""
<?xml version='1.0' encoding='UTF-8'?>
<osm version="0.6" generator="osmconvert 0.8.8">
"""

print('hi')