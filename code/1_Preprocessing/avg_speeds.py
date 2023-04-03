# This script averages speeds from Mapbox OR Uber data

import pandas as pd

speeds_path = r'D:\_TfC\Repos\_Projects\32_wri_numo\data_raw\Cairo\travel_speeds\1221211-Africa-Cairo.csv'

def avg_mapbox(speeds_path):
    speeds_cols = [0,1, 381, 382, 383, 384, 385, 386, 387, 388, 389, 390, 391, 392, 393]

    speeds = pd.read_csv(speeds_path, usecols=speeds_cols, header=None)

    speeds['speed'] = speeds[speeds_cols[2:]].mean(axis=1)

    speeds = speeds[[0,1,'speed']]

    speeds.to_csv('cairo_output.csv', index=False)

def avg_uber(speeds_path):
    speeds_cols = ['year','quarter','hour_of_day','osm_way_id','osm_start_node_id','osm_end_node_id','speed_mph_mean']
    
    speeds = pd.read_csv(speeds_path, usecols=speeds_cols)

    speeds = speeds[speeds['hour_of_day'].isin([8, 9])]

    speeds = speeds.groupby(['year','quarter','hour_of_day','osm_way_id','osm_start_node_id','osm_end_node_id'])['speed_mph_mean'].mean().reset_index()

    speeds = speeds.drop(['year','quarter','hour_of_day'], axis = 1)

    speeds.to_csv('sf_output.csv', index=False)

avg_uber(r'D:\_TfC\temp\NUMO\movement-speeds-quarterly-by-hod-san-francisco-2020-Q1.csv')

print('hello')

