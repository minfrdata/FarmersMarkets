import pandas as pd
import numpy as np
data=pd.read_csv("Farmer'sMarket.csv")
weekday_list=[]
for i in data["season1time"]:
    try:
        i_list=i.split(";")
        for j in i_list:
            weekday_list.append(j[:3])
    except:
        next

unique, counts = np.unique(weekday_list, return_counts=True)   
data=pd.DataFrame([counts],columns=['', 'Fri', 'Mon', 'Sat', 'Sun', 'Thu', 'Tue', 'Wed', 'sun'])
Week_Freq=data[['Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun']].T
Week_Freq.columns=["Freq"]


#plot
import plotly.plotly as py
import plotly.graph_objs as go
trace2 = go.Scatter(
    x = Week_Freq.index,
    y = Week_Freq.Freq,
    name = 'High 2007',
    line = dict(color = ('rgb(205, 12, 24)'),width = 4,dash = 'dash'))

layout = dict(title = "No. of Farmers markets in each Weekday",
              xaxis = dict(title = 'Weekday'),
              yaxis = dict(title = 'No of Farmers markets in each Weekday'),
              )

fig = dict(data=[trace2], layout=layout)
py.iplot(fig, filename='styled-line')