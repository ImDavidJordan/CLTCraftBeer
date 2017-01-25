library(leaflet,rgdal,data.table)
library(shiny)



setwd("/Users/David_Jordan/Google Drive/Special Projects/CLTCraftBeer/CLTCraftBeer/")

#Reading in Craft Beer Data
Breweries <- read.csv("Beer Data.csv")

#Reading in Beer List Data
Beer_List <- read.csv("Charlotte Craft Beer List.csv")

#Generating a dataframe of beer styles by brewery
Beer_List2 <-data.table(Beer_List)
Beer_List3 <- Beer_List2[,list(Style=list(unique(Style))),by=Brewery]

l<-list()
for (i in 1:length(Beer_List3$Brewery)){
  Styles<-toString(unlist(Beer_List3$Style[i],recursive = TRUE, use.names = TRUE))
  l[i]<-Styles
}
Beer_List3$Styles <- l #Adds Styles to Beer List Dataset

#Adding Beer Styles to Breweries Dataset
Breweries_New <- merge(Breweries, Beer_List3, by="Brewery")

#Getting the Frequency of Year Opened
#Brew_Freq <- table(Breweries$Year.Opened)
#Brew_Freq

#Creating Icons for Map Markers
beer_bottle <- makeIcon("empty-beer-bottle.png","empty-beer-bottle.png",36,36)

ui <- navbarPage("Charlotte Craft Breweries Map", id="nav",
                  tabPanel("Interactive Map",
                           wellPanel(selectInput("Styles", 
                                                 "Beer Styles",
                                                 choices=unique(Beer_List$Style),
                                                 multiple=FALSE)),
                           leafletOutput("CharlotteCraftMap")
                          
                 ),
                 tabPanel("Data Viewer",
                          wellPanel(h3("Beer Statistics"), plotlyOutput("BreweryOpen")))
                 )



server <- function(input, output, session) {

  output$CharlotteCraftMap <- renderLeaflet({
  #Plotting Beer Map Using Leaflet
  leaflet(Breweries_New) %>%
      addProviderTiles("OpenStreetMap")%>%
      addMarkers(icon=beer_bottle, 
                 popup=paste(
                   "<b>",
                   "Brewery: ",
                   "</b>",
                   paste("<a href=",Breweries_New$Brewery.Website,
                         ">",
                         Breweries_New$Brewery,
                         "</a>"), 
                   "<br>", 
                   "<b>",
                   "Neighborhood: ",
                   "</b>", 
                   Breweries_New$Neighborhood,
                   "<br>",
                   "<b>",
                   "Avg. Yelp Score: ",
                   "</b>",
                   Breweries_New$Average.Yelp.Rating,
                   "<br>",
                   "<b>",
                   "Styles: ",
                   "</b>",
                   Breweries_New$Styles
                 )
      )
  })
  
  filteredData <- reactive({
    Filtered_Breweries <- subset(Breweries_New, grepl(input$Styles,Breweries_New$Styles))
  })
  observe({
    leafletProxy("CharlotteCraftMap",data=filteredData()) %>%
      clearMarkers() %>%
      addMarkers(icon=beer_bottle, 
                 popup=paste(
                   "<b>",
                   "Brewery: ",
                   "</b>",
                   paste("<a href=",filteredData()[,7],
                         ">",
                         filteredData()[,1],
                         "</a>"), 
                   "<br>", 
                   "<b>",
                   "Neighborhood: ",
                   "</b>", 
                   filteredData()[,5],
                   "<br>",
                   "<b>",
                   "Avg. Yelp Score: ",
                   "</b>",
                   filteredData()[,6],
                   "<br>",
                   "<b>",
                   "Styles: ",
                   "</b>",
                   filteredData()[,9]
                 )
      )
  })
  
  #Creating plots from beer data
  output$BreweryOpen<-renderPlotly({
    Brewery_Openings<- count(Breweries_New$Year.Opened)
    plot_ly(x = ~Brewery_Openings$x, y = ~Brewery_Openings$freq, mode="lines")
    })
}
    
shinyApp(ui, server)