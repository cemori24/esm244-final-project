library(shiny)
library(tmap)
library(terra)
library(leaflet)

ui <- fluidPage(
  titlePanel("Shapefile Upload"),
  sidebarLayout(
    sidebarPanel(
      fileInput("user_shp", "Upload Shapefile")
    ),
    mainPanel(
      tmapOutput("base_map")
    )
  )
)

server <- function(input, output) {
  
  # Load basemap
  basemap <- tm_shape(lc_rast) +
    tm_raster(style = "cat", 
              palette = c("lightblue", "blue", "lightpink", "coral1", "red", "darkred", "tan", "darkgreen", 
                                     "darkgoldenrod3", "darkkhaki", "khaki1", "brown", "lightcyan", "lightseagreen"), 
                                     labels = c("NA", "Open Water", "Developed, Open Space", "Developed, Low Intensity", 
                                                "Developed, Medium Intensity", "Developed, High Intensity", "Barren Land (Rock/Sand/Clay)", 
                                                "Evergreen Forest", "Shrub/Scrub", "Grassland/Herbaceous", "Pasture/Hay", "Cultivated Crops", 
                                                "Woody Wetlands", "Emergent Herbaceous Wetlands"), 
              n= 14, 
              title = "Land Use Type")
  
  output$base_map <- renderTmap({
    basemap
  })
  
  # Read in the user uploaded shapefile
  user_shp <- reactive({
    if (is.null(input$user_shp)) {
      return(NULL)
    }
    readOGR(input$user_shp$datapath)
  })
  
  # Add shapefile to basemap as overlay
  observe({
    if (!is.null(user_shp())) {
      overlay <- tm_shape(user_shp()) +
        tm_borders(col = "black", lwd = 6)
      basemap + overlay
    }
  })
  
}


shinyApp(ui, server)
