library(shiny)
library(leaflet)

ui <- fluidPage(
  titlePanel("Shapefile Upload"),
  sidebarLayout(
    sidebarPanel(
      fileInput("shpfile", "Upload Shapefile (zip file)")
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

server <- function(input, output) {
  
  output$map <- renderLeaflet({
    # Check if a shapefile has been uploaded
    if(!is.null(input$shpfile)) {
      # Read the shapefile
      shp <- readOGR(dsn = input$shpfile$datapath, layer = basename(tools::file_path_sans_ext(input$shpfile$name)))
      # Create a leaflet map with the shapefile
      leaflet(shp) %>%
        addProviderTiles("Stamen.TonerLite") %>%
        addPolygons(fillOpacity = 0.5)
    } else {
      # Create an empty leaflet map
      leaflet() %>%
        addProviderTiles("Stamen.TonerLite")
    }
  })
  
}

shinyApp(ui, server)
