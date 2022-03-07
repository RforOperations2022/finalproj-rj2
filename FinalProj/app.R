#------- Loading libraries ---------
library(shiny)
library(leaflet)
library(plotly)
library(ggplot2)
library(data.table)
library(leaflet.extras)


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
                     choices = sort(unique(data$ROUTE)),
                     selected = "61D",
                     multiple = FALSE),

         sliderInput(inputId = 'traffic',
                     label = 'Total traffic:',
                     min = min(data$FY19_AVG_TOTAL),
                     max = max(data$FY19_AVG_TOTAL),
                     value = c(100, 300)),

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

                     tabPanel('Scatterplot',
                              plotlyOutput('scatterplot')),

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
            FY19_AVG_TOTAL >= input$traffic[1] &
            FY19_AVG_TOTAL <= input$traffic[2] &
            STOP_TYPE == input$stopType
         )
   })

   # Basemap
   output$leaflet <- renderLeaflet({
      leaflet() %>%
         addProviderTiles("OpenStreetMap.Mapnik") %>%
         setView(lng=-79.95, lat=40.44, zoom=10.7)

   })

   # leafletProxy
   observe({
      if(input$mapType == "Points"){
         leafletProxy("leaflet", data = filterData()) %>%
            clearMarkers() %>%
            clearGroup("heat") %>%
            addMarkers(lng = ~LONGITUDE, lat = ~LATITUDE)

      }else{
         leafletProxy("leaflet", data = filterData()) %>%
            clearMarkers() %>%
            clearGroup("heat") %>%
            addHeatmap(group = "heat", lng = ~LONGITUDE,
                       lat = ~LATITUDE, blur = 20, max = 0.08, radius = 15)
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
         labs(y = "Value", x = "", title = "Average boardings and alightings") +
         theme_bw()
   })


   # Scatterplot
   output$scatterplot <- renderPlotly({
      orderedData <- filterData()[order(filterData()$FY19_AVG_TOTAL, decreasing = TRUE), ]
      if (nrow(orderedData) >= 10) {
         orderedData = orderedData[1:10, ]
      }

      ggplot(data = orderedData, aes(x=STOP_NAME, y=FY19_AVG_TOTAL)) +
         geom_point(size=1, color="red") +
         geom_line(size=0.5, color="blue") +
         labs(x = "Bus Stop", y="Average Traffic",
              title = "Bus stops by total traffic (top 10 or less)") +
         theme(axis.text.x = element_text(angle = 90, hjust = 2))
   })


   # Data table
   output$datatable <- DT::renderDataTable({
      DT::datatable(data = filterData(),
                    options = list(pageLength = 10),
                    rownames = FALSE)

   })


   # Download data
   output$downloadData <- downloadHandler(
      filename = function() {
         paste("data.csv")
      },
      content = function(filename) {
         write.csv(filterData(), filename)
      }
   )

}


#-------------- Run App ---------------
shinyApp(ui = ui, server = server)