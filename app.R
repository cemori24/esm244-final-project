### Packages
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

### Data Files
lc_rast <- here("nlcd_data",
                "lc_rast_coarse.tif") %>% 
  terra::rast()

lc_rast_df <- as.data.frame(lc_rast, xy = TRUE)

roi_vec <- read_sf(here("nlcd_data",
                        "hawaii_2001", 
                        "parks_state",
                        "parks_state.shp")) %>% 
  st_transform(st_crs(lc_rast))

carbon_tph <- read_xlsx(here("carbon_stock_cfs", "carbon_storage_ton_C_per_hectare.xlsx")) %>% 
  clean_names() %>% 
  select(class:soc) %>% 
  pivot_longer(cols = above:soc, 
               names_to = "compartment", 
               values_to = "ton_c_per_hectare")

class_names <- read_xlsx(here("carbon_stock_cfs", "class_descriptions_key.xlsx")) %>% 
  clean_names()

roi_area <- lc_rast_df %>% 
  clean_names() %>%
  group_by(land_cover_class) %>%
  tally(name = "area_hectares") %>% 
  merge(., class_names, by.x = "land_cover_class", by.y = "class")

roi_carb_per_area <- merge(roi_area, carbon_tph, 
                           by.x = "land_cover_class", by.y = "class") %>% 
  mutate(total_carbon_log_tons = log(area_hectares * ton_c_per_hectare))

roi_carbon_table <- roi_carb_per_area %>% 
  select(land_cover_class, area_hectares, description, compartment, total_carbon_log_tons) %>% 
  pivot_wider(names_from = "compartment", values_from = "total_carbon_log_tons")
roi_carbon_table <- roi_carbon_table[, c("land_cover_class", 
                                         "description", 
                                         "area_hectares", 
                                         "above", 
                                         "below", 
                                         "soc", 
                                         "dead_matter", 
                                         "litter")]

color_map <- c("NA" = "lightblue", 
              "Open Water" = "blue", 
              "Developed, Open Space" = "lightpink", 
              "Developed, Low Intensity" = "coral1", 
              "Developed, Medium Intensity" = "red", 
              "Developed, High Intensity" = "darkred", 
              "Barren Land (Rock/Sand/Clay)" = "tan", 
              "Evergreen Forest" = "darkgreen", 
              "Shrub/Scrub" = "darkgoldenrod3", 
              "Grassland/Herbaceous" = "darkkhaki", 
              "Pasture/Hay" = "khaki1", 
              "Cultivated Crops" = "brown", 
              "Woody Wetlands" = "lightcyan", 
              "Emergent Herbaceous Wetlands" = "lightseagreen")

### Start of UI Block
ui <- fluidPage(theme = bs_theme(bootswatch = "minty"),
                navbarPage(
                  "Carbon Storage Calculator",

### TAB 1: Info, Upload Button                                    
                  tabPanel(
                    'Upload Data',
                   
                    fluidRow(style='text-align:center; text-color:red;color:red; font-size:100%;', 
                             "Note: This site is still under construction (last updated: 2023-03-03)."),
                    
                    h5("This app was created by students of the Bren School to assist with analysis of carbon storage 
                       potential of geographic regions. Users can (1) upload a vector polygon of their Region of 
                       Interest (ROI) within the US, (2) view current land use data, and (3) view current carbon 
                       storage and opportunities for improvement."),
                    h5(strong("To begin, please upload a vector polygon (.tif, .img, .shp) outlining your ROI.  The file 
                       should NOT include any other polygons, because all polygons will be used for carbon storage
                       calculations.")),
                    
                    br(), 
                    
                    fluidRow(
                      column(12, 
                             align = "center", 
                             fileInput("file", label = div(style = "font-size:16px", "Upload ROI Vector")))
                    ),
                    
                    br(),
                    
                    # fluidPage(
                    #   
                    #   hr(),
                    #   fluidRow(column(4, verbatimTextOutput("value"))),
                    #   
                    #   
                    # ),
                    # sidebarLayout(
                    #   sidebarPanel(
                    #     fileInput("file", label = div(style = "font-size:20px", "Upload Area of Interest File"))
                    #   ), #end sidebar panel
                    #   
                    #   mainPanel(
                    #     tmapOutput('base_map') #, height = '600px')#, 
                    #   #textOutput('pic_dim_print')
                    #   ) #end main panel
                    # ), #end sidebar layout
                    
                    hr(),
                    tags$blockquote(HTML("<p><b>Data Sources</b></p><p><i>Land Use Data:</i>
                                    Homer, Collin G., Huang, Chengquan, Yang, Limin, Wylie, 
                                    Bruce K., Coan, Michael J., Development of a 2001 National 
                                    Land Cover Database for the United States: Photogrammetric 
                                    Engineering and Remote Sensing, v. 70, no. 7, p. 829–840, at 
                                    https://doi.org/10.14358/PERS.70.7.829.</p><p><i>Hawaii State 
                                    Park Vector File:</i> Hawaii Statewide GIS Program at
                                    https://planning.hawaii.gov/gis/download-gis-data-expanded/.</p>")),
                    hr(),
                  ), #end info tab panel
                  

            
### TAB 2: Land Cover Map, Pie Chart          
    tabPanel("Land Cover Map", 
             #imageOutput("map_img"),
             #br(),
             #hr(),
             h4(strong("Land Cover Shapefile Visualization")),
             p(style="text-align: justify; font-size = 25px",
               "Eventually, this tab will display an interactive 
                       map that visualizes the current land use and carbon storage data. There will
                       likely be a color scale, with green representing areas with a lot 
                       of stored carbon and red representing areas with little to no carbon storage.  
                       Alternatively, the color scale would represent potential for carbon storage."),
             
             plotOutput("piechart"),
             
             
             br(),
             hr(),
             tags$blockquote("This Shiny app is still under continuous development. 
           Please look forward to future updates!"),
           hr(),
           
           verbatimTextOutput("summary")
           ),

            
            
            
### TAB 3: Carbon Stock Bar Chart, Table, Transformations            
            tabPanel("Land Transformations", 
                     
                     h4(strong("Chart of Carbon Storage Options")),
                     
                     p(""),
                     p(style="text-align: justify: font-size = 25px",
                       "This tab will display a bar chart showing areas of low, medium, and high carbon
                       storage as a percentage of the mapped region. Below the chart, the user will be able to
                       choose a land transformation from a dropdown menu and input an area to be transformed.
                       For example, the user can select 'Wetland to Forest' and input 100 sq. kilometers.
                       The chart will update automatically, showing the new %s and difference from the original in
                       a shaded region of each bar in the chart. This functionality will help the user to gain 
                       insight into potential land transformations that could increase carbon storage within the
                       mapped region."),

                     plotOutput("carbon_plot"),
                     
                     
                     
                     
                     p("Image source: https://www.fs.usda.gov/ccrc/sites/default/files/2021-06/Carbon-storage-by-ecosystem.png"),
                     
                     selectInput("select", label = h3("Transform FROM:"), 
                                 choices = list("Grassland" = 1, "Forest" = 2, "Wetland" = 3), 
                                 selected = 1),
                     
                     selectInput("select", label = h3("Transform TO:"), 
                                 choices = list("Grassland" = 1, "Forest" = 2, "Wetland" = 3), 
                                 selected = 1),
                     
                     numericInput("num", label = h3("Area of Transformation:"), value = 1),
                     
                     br(),
                     hr(),
                     
                     h4(strong("Download All Data")),
                     p("Here, the user will also be able to download the data corresponding to the chart above, as
                       modified using the above widgets. The download format would be a spreadsheet showing the post-transformation
                       land use types, area (sq km), carbon storage (kg CO2e), and carbon storage (% of total) 
                       within the mapped region. We may also include other data types such as delta area or delta carbon storage 
                       compared to the original map or cost of the specified land transformation(s).
                       This functionality would be useful to a user who, for instance, intends to produce a report
                       on the most effective land transformations to increase carbon storage within the mapped region."),
                     
                     actionButton("action", label = "Download Carbon Storage Data"),
                     
                     plotOutput("testplot"),
                     
                     
                     br(),
                     hr(),
                     
                     tags$blockquote("This Shiny app is still under continuous development. 
                                     Please look forward to future updates!"),
                     
                     hr()),
          )
        )
   #)
#)
### End of UI Block


### Server Block
server <- function(input, output) {

  # tmap_options(check.and.fix = TRUE) 
  output$base_map <- renderTmap({
    tm_shape(lc_rast) +
    tm_raster(style = "cat", palette = c("lightblue", "blue", "lightpink", "coral1", "red", "darkred", "tan", "darkgreen", "darkgoldenrod3", "darkkhaki", "khaki1", "brown", "lightcyan", "lightseagreen"), 
              labels = c("NA", "Open Water", "Developed, Open Space", "Developed, Low Intensity", "Developed, Medium Intensity", "Developed, High Intensity", "Barren Land (Rock/Sand/Clay)", 
                         "Evergreen Forest", "Shrub/Scrub", "Grassland/Herbaceous", "Pasture/Hay", "Cultivated Crops", "Woody Wetlands", "Emergent Herbaceous Wetlands"), 
              n= 14, title = "Land Type") +
    tm_shape(roi_vec) +
    tm_borders(col = "black", lwd = 2)
  })
  
  
  
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white',
    #          xlab = 'Waiting time to next eruption (in mins)',
    #          main = 'Histogram of waiting times')
    #     
        
   # output$map_img <- renderImage({
      
  #    list(src = "WWW/New_York_map.jpeg",
   #        width = "100%",
    #       height = '100%')
      
    #}, deleteFile = F)
  
  
    output$piechart <- renderPlot({
      ggplot(roi_area, aes(x = "", y = area_hectares, fill = description)) +
        geom_bar(width = 1, stat = "identity", color='black', size=.3) +
        coord_polar("y", start = 0) +
        scale_fill_manual(values = color_map) + # Use the color_map vector to specify colors
        theme_void() + 
        theme(legend.position = "right") +
        labs(fill = "Land Use Cover", title = "Land Use Cover Percentage")
    })
    
    output$carbon_plot <- renderPlot({
      ggplot() +
        geom_col(data = roi_carb_per_area, 
                 mapping = aes(x = description, 
                               y = total_carbon_log_tons, 
                               fill = compartment
                 )) +
        scale_fill_manual(values = c("plum2", "slateblue1", "palegreen1", "lightgoldenrod1", "tan1"), 
                          labels = c(above = "Above Ground", 
                                     below = "Below Ground", 
                                     dead_matter = "Dead Matter", 
                                     litter = "Litter", 
                                     soc = "Soil Organic Carbon"
                          )) +
        theme(axis.text.x = element_text(angle = 45, 
                                         vjust = 1, 
                                         hjust=1
        )) +
        labs(x = "", 
             y = "Carbon (log tons)", 
             title = "Carbon Storage Per Land Use Type in Hawaii", 
             fill = "Compartment")
    })
    
    # output$carbon_table_test <- renderTable({roi_carbon_table})
    # 
    # output$carbon_table <- function() {
    #   knitr::kable(roi_carbon_table, 
    #         col.names = c("NLCD Class", 
    #                  "Land Use Description", 
    #                  "Area (ha)", 
    #                  "Above Ground", 
    #                  "Below Ground", 
    #                  "Soil Organic Carbon", 
    #                  "Dead Matter", 
    #                  "Litter"), 
    #         align = "llcccccr",
    #         capton = "Total Carbon Stored (tons) per Compartment in ROI")
    # }
  
  }


### Run App
shinyApp(ui = ui, server = server)
