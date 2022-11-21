library(shiny)
library(dplyr)
library(leaflet)
library(DT)
library(shinyWidgets)
library(tidyverse)


# Import Data and clean it

crime_data <- read.csv("CRIME_INCIDENT_REPORTS_2022.csv", stringsAsFactors = FALSE )
crime_data <- data.frame(crime_data)
crime_data <- crime_data[crime_data$MONTH == '11',]
crime_data$Lat <-  as.numeric(crime_data$Lat)
crime_data$Long <-  as.numeric(crime_data$Long)
crime_data=filter(crime_data, Lat != 0)
crime_data=filter(crime_data, DISTRICT != '')
crime_data$SHOOTING <- ifelse(crime_data$SHOOTING == '1', 'Shooting','No shooting')

#new column for the popup label

crime_data <- mutate(crime_data, cntnt=paste0('<strong>INCIDENT_NUMBER: </strong>',INCIDENT_NUMBER,
                                              '<br><strong>OFFENSE_CODE:</strong> ', OFFENSE_CODE,
                                              '<br><strong>OFFENSE_DESCRIPTION:</strong> ', OFFENSE_DESCRIPTION,
                                              '<br><strong>DISTRICT:</strong> ',DISTRICT,
                                              '<br><strong>REPORTING_AREA:</strong> ',REPORTING_AREA,
                                              '<br><strong>SHOOTING:</strong> ',SHOOTING,
                                              '<br><strong>OCCURRED_ON_DATE:</strong> ',OCCURRED_ON_DATE,
                                              '<br><strong>STREET:</strong> ',STREET)) 

pal <- colorFactor(pal = c("#1b9e77", "#d95f02"), domain = c('Shooting','No shooting'))
ui <- fluidPage(
  sidebarLayout(mainPanel(navbarPage("CRIME_INCIDENT_REPORTS_2022", id="main",
                                     tabPanel("Map", leafletOutput("bbmap", height=1000)),
                                     tabPanel("Data", DT::dataTableOutput("data")))),
    sidebarPanel(top = 50, right = 10,
                                pickerInput("DISTRICT", label = "Select a district:",
                                            choices = list("All districts", 
                                                           "A1","B2","D4","E5","D14","B3","C11","A7","C6","E18",
                                                           "E13","A15","External"),
                                            options = list(
                                              
                                              `live-search` = TRUE)))
                                ))

server <- shinyServer(function(input, output) {
  
  # create the leaflet map  
  filteredData <- reactive({
    if (input$DISTRICT == "All districts") {
      crime_data
    } else {
      filter(crime_data, DISTRICT == input$DISTRICT)
    }
  })
  
  output$bbmap <- renderLeaflet({
    leaflet(filteredData()) %>% 
      addCircles(lng = ~Long, lat = ~Lat) %>% 
      addTiles() %>%
      addCircleMarkers(lat =  ~Lat, lng =~Long, 
                       radius = 3, popup = ~as.character(cntnt),color = ~pal(SHOOTING),
                       stroke = FALSE, fillOpacity = 0.8)%>%
      addLegend(pal=pal, values=crime_data$SHOOTING,opacity=1)%>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="ME",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
  })
  
  output$data <-DT::renderDataTable(datatable(
    crime_data[,c(-3,-9:-13,-17,-18)],filter = 'top',
    colnames = c("INCIDENT_NUMBER", "OFFENSE_CODE", "OFFENSE_DESCRIPTION", "DISTRICT", "REPORTING_AREA", "SHOOTING","OCCURRED_ON_DATE",
                 "STREET","Lat","Long")
  ))

})



shinyApp(ui = ui, server = server)