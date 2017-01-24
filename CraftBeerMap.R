library(leaflet,rgdal)

setwd("/Users/David_Jordan/Google Drive/Special Projects/CLTCraftBeer/CLTCraftBeer/")

#Reading in Craft Beer Data
Breweries <- read.csv("Beer Data.csv")

#Getting the Frequency of Year Opened
#Brew_Freq <- table(Breweries$Year.Opened)
#Brew_Freq

#Creating Icons for Map Markers
beer_bottle <- makeIcon("empty-beer-bottle.png","empty-beer-bottle.png",36,36)

#Plotting Beer Map Using Leaflet
leaflet() %>%
  addProviderTiles("OpenStreetMap") %>%
  addMarkers(data=Breweries,
             icon=beer_bottle, 
             popup=paste("<b>","Brewery: ","</b>",paste("<a href=",Breweries$Brewery.Website,">",Breweries$Brewery,"</a>"), "<br>", "<b>","Neighborhood: ","</b>", Breweries$Neighborhood,"<br>","<b>","Avg. Yelp Score: ","</b>",Breweries$Average.Yelp.Rating),
             clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F)
             )

    
