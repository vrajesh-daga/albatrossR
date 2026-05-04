import pandas as pd
import numpy as np

df = pd.read_csv('Combined chick data.xlsx - Sheet1.csv')

colony_lat = 21.5752667
colony_long = -158.2733528

df["prev_lat"] = df.groupby("BirdID")["compensatedlat"].shift(1)
df["prev_long"] = df.groupby("BirdID")["long"].shift(1)

df["prev_lat"] = df["prev_lat"].fillna(colony_lat)
df["prev_long"] = df["prev_long"].fillna(colony_long)

df['colony_lat'] = colony_lat
df['colony_long'] = colony_long

lat1 = np.radians(df["prev_lat"])
long1 = np.radians(df["prev_long"])
lat2 = np.radians(df["compensatedlat"])
long2 = np.radians(df["long"])

dlat = abs(lat1 - lat2)
dlong = abs(long1 - long2)


R = 6371 #earth radius

df['correct_step_distance'] = (2*R) * np.arcsin(np.sqrt(
    ((np.sin(dlat / 2)) ** 2)
    + (np.cos(lat1)*np.cos(lat2))
    * ((np.sin(dlong / 2)) ** 2)
))

df.to_csv("all_chick_data_with_metrics.csv", index=False)
df.to_excel("all_chick_data_with_metrics_final.xlsx", index=False)
