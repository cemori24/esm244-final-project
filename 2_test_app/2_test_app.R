library(shiny)
library(bslib) ### Custom themes. Run command bs_theme_preview() in console.
library(raster)
library(here)
library(sf)
library(tidyverse)
library(terra) ### Raster data functionality
library(tmap)
library(rgdal)
library(stars)
library(dplyr)
library(janitor)
library(readxl)
library(ggplot2)
library(knitr)
library(sf)


lc_rast <- here("2_test_app",
                "lc_rast_coarse.tif") %>% 
  terra::rast()


ui <- fluidPage(
  titlePanel("Shapefile Upload"),
  sidebarLayout(
    sidebarPanel(
      fileInput("poly", "Choose spatial polygon vector file",
                multiple = FALSE,
                accept = c(".sqlite", ".kml", ".kmz", ".shp"))
      ), # end sidebar panel
      
    mainPanel(
      plotOutput("base_map")
    ) # end main panel
  ) #end sidebar layout
) #end app




server <- function(input, output) {
 
roi_poly <- reactive({
  req(input$poly)
  (read_sf(input$poly$datapath))
      #st_zm() %>% 
      
  })
  
  output$base_map <- renderPlot({
    
      req(input$poly)
    
    plot(roi_poly())
      
      # rast_reproj <- projectRaster(spat_interp()$rast, crs="+proj=longlat +datum=WGS84", method='ngb')
      # poly_reproj <- st_transform(spat_preproc()$poly, crs="+proj=longlat +datum=WGS84")
      # 
      # tm_shape(lc_rast) +
      #   tm_raster(style = "cat", 
      #             palette = c("lightblue", "blue", "lightpink", "coral1", "red", "darkred", "tan", "darkgreen", 
      #                                    "darkgoldenrod3", "darkkhaki", "khaki1", "brown", "lightcyan", "lightseagreen"), 
      #                                    labels = c("NA", "Open Water", "Developed, Open Space", "Developed, Low Intensity", 
      #                                               "Developed, Medium Intensity", "Developed, High Intensity", "Barren Land (Rock/Sand/Clay)", 
      #                                               "Evergreen Forest", "Shrub/Scrub", "Grassland/Herbaceous", "Pasture/Hay", "Cultivated Crops", 
      #                                               "Woody Wetlands", "Emergent Herbaceous Wetlands"), 
      #             n= 14, 
      #             title = "Land Use Type") +
        # tm_shape(roi_poly()) +
        # tm_borders(col = "black", lwd = 2)
      
    }
  )
   
  # # Load basemap
  # basemap <- tm_shape(lc_rast) +
  #   tm_raster(style = "cat", 
  #             palette = c("lightblue", "blue", "lightpink", "coral1", "red", "darkred", "tan", "darkgreen", 
  #                                    "darkgoldenrod3", "darkkhaki", "khaki1", "brown", "lightcyan", "lightseagreen"), 
  #                                    labels = c("NA", "Open Water", "Developed, Open Space", "Developed, Low Intensity", 
  #                                               "Developed, Medium Intensity", "Developed, High Intensity", "Barren Land (Rock/Sand/Clay)", 
  #                                               "Evergreen Forest", "Shrub/Scrub", "Grassland/Herbaceous", "Pasture/Hay", "Cultivated Crops", 
  #                                               "Woody Wetlands", "Emergent Herbaceous Wetlands"), 
  #             n= 14, 
  #             title = "Land Use Type")
  # 
  # output$base_map <- renderTmap({
  #   basemap
  # })
  # 
  # 
  # # Add shapefile to basemap as overlay
  # observe({
  #   if (!is.null(user_shp())) {
  #     overlay <- tm_shape(roi_poly()) +
  #       tm_borders(col = "black", lwd = 6)
  #     basemap + overlay
  #   }
  # })
  
}


shinyApp(ui, server)
