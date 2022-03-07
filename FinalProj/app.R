#------- Loading libraries ---------
library(shiny)
library(leaflet)
library(plotly)
library(ggplot2)
library(data.table)


#--------- Load Data ----------
data = fread("busstopusagebyroute.csv", verbose = FALSE)
data = select(data, c("ROUTE", "STOP_NAME", "LATITUDE", "LONGITUDE", "STOP_TYPE", "ALL_ROUTES", "FY19_AVG_ON", "FY19_AVG_OFF", "FY19_AVG_TOTAL"))


#------ App UI -------
ui <- fluidPage(

   # title
   titlePanel('Port Authority Bus Stops Usage (2019)'),

   # sidebar layout
   sidebarLayout(

      # inputs
      sidebarPanel(

         conditionalPanel(
            condition="input.tabs == 'Map'",
            selectInput(inputId = "mapType",
                        label = "Map Type",
                        choices = c("Points", "Heatmap"))),


         selectInput(inputId = 'route',
                     label = 'Route:',
                     choices = unique(data$ROUTE),
                     selected = "61D",
                     multiple = FALSE),

         selectInput(inputId = 'stopName',
                     label = 'Stop Name:',
                     choices = unique(data$STOP_NAME),
                     selected = "FORBES AVE AT MOREWOOD AVE F",
                     multiple = TRUE),

         radioButtons(inputId = 'stopType',
                     label = 'Stop Type:',
                     choices = unique(data$STOP_TYPE),
                     selected = "Bus"),

         downloadButton(outputId = "downloadData", label = "Download Data"),

      ),

      # outputs
      mainPanel(

         tabsetPanel(id= "tabs",

                     tabPanel('Map',
                              leafletOutput('leaflet')),

                     tabPanel('Barchart',
                              plotlyOutput('barchart')),

                     tabPanel('Line Plot',
                              plotlyOutput('lineplot')),

                     tabPanel('Data Table',
                              DT::dataTableOutput(outputId = 'datatable'))
         )
      )
   )
)


#------ App UI -------
server <- function(input, output) {

   # Data filtering
   filterData <- reactive({
      data %>%
         filter(
            ROUTE %in% input$route &
            STOP_NAME == input$stopName &
            STOP_TYPE == input$stopType
         )
   })

   # Basemap
   output$leaflet <- renderLeaflet({
      leaflet() %>%
         addProviderTiles("OpenStreetMap.Mapnik") %>%
         setView(lng=-80, lat=40.44, zoom=10)

   })

   # leafletProxy
   observe({
      # data <- filterData()
      if(input$selectMapType == "Points"){
         leafletProxy("leaflet", data = filterData()) %>%
            clearMarkers() %>%
            clearGroup("heat") %>%
            addMarkers(lng = LONGITUDE, lat = LATITUDE)

      }else{
         leafletProxy("leaflet", data = filterData()) %>%
            clearMarkers() %>%
            clearGroup("heat") %>%
            addHeatmap(group = "heat", lng = LONGITUDE,
                       lat = LATITUDE, blur = 20, max = 0.08, radius = 15)
      }
   })

   # Barchart
   output$barchart <- renderPlotly({
      summarizedData <- filterData() %>%
         summarise(Average_Boardings = sum(FY19_AVG_ON), Average_Alightings = sum(FY19_AVG_OFF))

      t_summarizedData <- transpose(summarizedData)
      row.names(t_summarizedData) <- colnames(summarizedData)

      ggplot(data = t_summarizedData, aes(x = row.names(t_summarizedData), y = V1)) +
         geom_bar(stat="identity") +
         labs(ylab = "Value", title = "Sum of average boardings and alightings for selected route")
   })


   # Lineplot
   output$lineplot <- renderPlotly({
      orderedData <- filterData()[order(filterData()$FY19_AVG_TOTAL, decreasing = TRUE),]
      orderedData <- orderedData[1:5, ]

      ggplot(data = orderedData, aes(x=BUS_STOP, y=FY19_AVG_TOTAL)) +
         geom_line() +
         labs(xlab = "Bus Stop",
              title = "Top 5 bus stops by total traffic for the specified route")
   })


   # Data table
   output$datatable <- DT::renderDataTable({
      DT::datatable(data = filterData,
                    options = list(pageLength = 10),
                    rownames = FALSE)

   })


   # Download data
   output$downloadData <- downloadHandler(
      filename = function() {
         paste("data.csv")
      },
      content = function(filename) {
         write.csv(filterData, filename)
      }
   )

}


# Run the application -----------------------------------------------
shinyApp(ui = ui, server = server)