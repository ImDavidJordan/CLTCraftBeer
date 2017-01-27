library(leaflet,rgdal)
library(shiny)
library(rsconnect)
library(data.table)
library(plotly)
library(leaflet)
library(plyr)
rsconnect::setAccountInfo(name='imdavidjordan', token='AE67CB673B29060AC44C11D499F5F92E', secret='FdmHdvZeFZqsivaudPLAmPmgnI8KgqE5P2Z4eicT')





#Reading in Craft Beer Data
Breweries <- read.csv("Data Folder/Beer Data.csv")

#Reading in Beer List Data
Beer_List <- read.csv("Data Folder/Charlotte Craft Beer List.csv")

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
beer_bottle <- makeIcon("bottle.png","bottle.png",36,36)

ui <- navbarPage("Charlotte Craft Breweries Map", id="nav",
                  tabPanel("Interactive Map",
                           wellPanel(selectInput("Styles", 
                                                 "Beer Styles",
                                                 choices=unique(Beer_List$Style),
                                                 multiple=FALSE)),
                           leafletOutput("CharlotteCraftMap")
                          
                 ),
                 tabPanel("Statistics",
                          wellPanel(h3("Brewery Statistics"), 
                                    plotlyOutput("BreweryOpen")
                                    ),
                          wellPanel(h3("Beer Statistics"))
                          ),
                 tabPanel("Brewery Data Viewer", dataTableOutput("table")
                          ),
                 tabPanel("Beer Data Viewer", dataTableOutput("Beers"))
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
    plot_ly(x = ~Brewery_Openings$x, y = ~Brewery_Openings$freq, mode="lines") %>%
      layout(xaxis=list(title="Year"),
             yaxis=list(title="Brewery Openings")
      )
    })
  Breweries_New_D <- data.table(subset(Breweries_New,select = c(1:7,9)))
  output$table <- renderDataTable(Breweries_New_D)
  output$Beers <- renderDataTable(Beer_List2)
}
    
shinyApp(ui, server)