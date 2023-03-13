library(shiny)
library(tmap)
library(terra)
library(leaflet)
library(here)
library(sf)
library(rgdal)
library(DT)
library(dygraphs)
library(xts)

browser()

# Data files
# Base map land cover raster
lc_rast <- here("nlcd_data",
                "lc_rast_coarse.tif") %>% 
  terra::rast()

# Default Region of Interest shapefile
roi_sf_default <- read_sf(here("nlcd_data","hawaii_2001", "parks_state","parks_state.shp"))

ui <- fluidPage(
  titlePanel("Shapefile Upload"),
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "filemap",
                label = "Upload map. Choose shapefiles",
                multiple = TRUE,
                accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj'))
    ),
    mainPanel(
      tmapOutput("base_map")
    )
  )
)

server <- function(input, output) {
  
  
  # Read in the user uploaded shapefile
  filemap <- reactive({
    if (is.null(input$filemap)) {
      return(NULL)
    } else {
      
    # shpdf is a data.frame with the name, size, type and datapath of the uploaded files
      shpdf <- input$filemap
      
    # Name of the temporary directory where files are uploaded
    tempdirname <- dirname(shpdf$datapath[1])
    browser()
    
    # Rename files
    for (i in 1:nrow(shpdf)) {
      file.rename(
        shpdf$datapath[i],
        paste0(tempdirname, "/", shpdf$name[i])
      )
    }
    
    # Now we read the shapefile with readOGR() of rgdal package
    # passing the name of the file with .shp extension.
    user_shp <- readOGR(tempdirname, "cty_council_dist_mau")
   
    return(user_shp)
    }
  })
  
  # Create a reactive object for the roi_sf based on user upload or default
  roi_sf <- reactive({
    if (is.null(filemap())) {
      return(roi_sf_default)
    } else {
      
      st_transform(filemap(), crs(lc_rast)) 
    }
  })
  
  # Render map
  output$base_map <- renderTmap({
    
    # Load basemap and roi_sf
    tmap_options(check.and.fix = TRUE)
    base_map <- tm_shape(lc_rast) +
      tm_raster(style = "cat", palette = c("lightblue", "blue", "lightpink", "coral1", "red", "darkred", "tan", "darkgreen", "darkgoldenrod3", "darkkhaki", "khaki1", "brown", "lightcyan", "lightseagreen"), 
                labels = c("NA", "Open Water", "Developed, Open Space", "Developed, Low Intensity", "Developed, Medium Intensity", "Developed, High Intensity", "Barren Land (Rock/Sand/Clay)", "Evergreen Forest", "Shrub/Scrub", "Grassland/Herbaceous", "Pasture/Hay", "Cultivated Crops", "Woody Wetlands", "Emergent Herbaceous Wetlands"), 
                n= 14, title = "Land Type") +
      tm_shape(roi_sf()) +
      tm_borders(col = "black", lwd = 2) 
    base_map
    
  })
  
}

shinyApp(ui, server)








  
  
  
  
  
  