import pandas as pd
import numpy as np

colony_lat = 21.5752667
colony_long = -158.2733528

### Since the there is no uniform tag data interchangable parameters are listed below: ###
df_insert = pd.read_csv('original-datasets/updated 2013-2014 chick coordinates - Sheet1.csv')
latitude = 'latitude'
longitude = 'longitude'
identifier = 'Tag'

def correct_step_size(df, lat, lon):
    df["prev_lat"] = df.groupby(identifier)[lat].shift(1)
    df["prev_long"] = df.groupby(identifier)[lon].shift(1)

    df["prev_lat"] = df["prev_lat"].fillna(colony_lat)
    df["prev_long"] = df["prev_long"].fillna(colony_long)

    df['colony_lat'] = colony_lat
    df['colony_long'] = colony_long

    lat1 = np.radians(df["prev_lat"])
    long1 = np.radians(df["prev_long"])
    lat2 = np.radians(df[lat])
    long2 = np.radians(df[lon])

    dlat = abs(lat1 - lat2)
    dlong = abs(long1 - long2)


    R = 6371 #earth radius

    df['correct_step_distance'] = (2*R) * np.arcsin(np.sqrt(
        ((np.sin(dlat / 2)) ** 2)
        + (np.cos(lat1)*np.cos(lat2))
        * ((np.sin(dlong / 2)) ** 2)
    ))

    df.to_csv("chickdata.csv", index=False)
    df.to_excel("chickdata.xlsx", index=False)

correct_step_size(df_insert, latitude, longitude)
