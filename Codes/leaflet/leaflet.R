setwd("/Users/minxiaocn/Desktop/Georgetown/ANLY503 Visualization/exam")
farm<-read.csv("Farmer'sMarket.csv")
head(farm)
library(leaflet)
library(sp)
library(rgdal)
library(maps)
library(dplyr)

library(noncensus)



data("counties")
#data("states")
cty=counties



#farm state name
# clean farm data: remove dc and some islands
farm<-read.csv("Farmer'sMarket.csv")
sapply(farm,class)
farm$state<-as.character(farm$state)

farm<-farm[(farm[,"state"] %in% c(state.name)),]

n=dim(farm)[1]
farm["state_abb"]=0

for (i in c(1:n))
  {
  farm[i,"state_abb"]= state.abb[which(state.name ==farm[i,"state"])]
  }

#combine farm and counties data
cty$state_abb<-cty$state

farm2=merge(farm,cty,by=c("state_abb"))

# pivot table to calculate the farm
amount_state <- group_by(farm,state_abb) %>%
                summarise(n_farms=length(fmid),x_mean=mean(x,na.rm=T),y_mean=mean(y,na.rm=T)) 


cty$no_farms<-0
cty<-merge(cty,amount_state,by=c("state_abb"))
colnames(cty)
state_<-unique(cty[c("state_abb", "state_fips")])
data("states")
#merge state and amount_state

state_farm<-merge(state_,amount_state,by=c("state_abb"))
colnames(state_farm)<-c("state","STATEFP","n_farms", "x","y" )
state_farm<-merge(state_farm,states,by=c("state"))


#import us county data
us.map <- readOGR(dsn = "/Users/minxiaocn/Desktop/Georgetown/ANLY503 Visualization/exam/Code And Data for Leaflet_R Maps Example/.", layer = "cb_2016_us_county_20m", stringsAsFactors = FALSE)
head(us.map)
# Remove Alaska(2), Hawaii(15), Puerto Rico (72), Guam (66), Virgin Islands (78), American Samoa (60)
#  Mariana Islands (69), Micronesia (64), Marshall Islands (68), Palau (70), Minor Islands (74)
us.map <- us.map[!us.map$STATEFP %in% c("02", "15", "72", "66", "78", "60", "69",
                                        "64", "68", "70", "74"),]
#head(us.map)

# Make sure other outling islands are removed.
us.map <- us.map[!us.map$STATEFP %in% c("81", "84", "86", "87", "89", "71", "76",
                                        "95", "79"),]
#merge farm map to us map
farm_map<-merge(us.map,state_farm,by=c("STATEFP"))



##Make pop up for the land use sites
# Format popup data for leaflet map.
popup_dat <- paste0("<strong>State: </strong>", 
                    farm_map$state, 
                    "<br><strong>No of farmers: </strong>", 
                    farm_map$n_farms)

# Format popup data for leaflet map.
popup_LU2<- paste0("<strong>State: </strong>", 
                   state_farm$state, 
               "<br><strong>Population: </strong>", 
                    state_farm$population)

popup_LU<- paste0("<strong>County: </strong>", 
                  farm$market_name, 
                  "<br><strong>Website link</strong>", 
                  farm$website)


pal <- colorQuantile("YlOrRd", NULL, n = 9)
gmap <- leaflet(data = farm_map) %>%
  # Base groups
  addTiles() %>%
  setView(lng = -106, lat = 40, zoom = 4) %>% 
  addPolygons(fillColor = ~pal(n_farms), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1,
              popup = popup_dat,
              group="No. of farmer markets by states") %>% 
  # Overlay groups
  addMarkers(data=farm,lat=~y, lng=~x, popup=popup_LU, group = "Farmer's market details") %>% 
  addMarkers(data=state_farm,lat=~y, lng=~x, popup=popup_LU2, group = "Population in each state") %>% 
  
  # Layers control
  addLayersControl(
    baseGroups = c("No. of farmer markets by states"),
    overlayGroups = c("Farmer's market details","Population in each state"),
    options = layersControlOptions(collapsed = FALSE)
  )

gmap
saveWidget(gmap, 'leaflet.html', selfcontained = TRUE)




