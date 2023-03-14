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

tmap_options(check.and.fix = TRUE)

lc_rast <- here("2_test_app",
                "lc_rast_coarse.tif") %>% 
  terra::rast()


ui <- fluidPage(
  titlePanel("Shapefile Upload"),
  sidebarLayout(
    sidebarPanel(
      fileInput("poly", "Choose spatial polygon vector file",
                multiple = FALSE,
                accept = c(".gpkg"))
      ), # end sidebar panel
      
    mainPanel(
      plotOutput("base_map")
    ) # end main panel
  ) #end sidebar layout
) #end app




server <- function(input, output) {
 
  
  # roi_poly <- reactive({
  #   req(input$poly)
  #   read_sf(input$poly$datapath)
  #  # readOGR(input$poly$datapath,layers[geom])
  # })
roi_poly <- reactive({
  req(input$poly)
  (read_sf(input$poly$datapath)) %>%
    st_as_sf %>% 
    st_transform(st_crs(lc_rast)) 

  })
  
     # output$base_map <- renderPlot({
     #   req(input$poly)
     #   #print(roi_poly)
     #   plot(roi_poly)
     # })
    
    output$base_map <- renderTmap({

       req(input$poly)
      tm_basemap(leaflet::providers$OpenStreetMap.Mapnik) +
        tm_shape(roi_poly()) +
        tm_borders(col = "black", lwd = 2)

    })
      
     
}



shinyApp(ui, server)
