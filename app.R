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

tmap_options(check.and.fix = TRUE)
### Data Files
lc_rast <- here("nlcd_data",
                "lc_rast_coarse.tif") %>% 
  terra::rast()

lc_rast_sub <-clamp(lc_rast, lower=1, values=FALSE)

# likely won't need this here anymore
lc_rast_df <- as.data.frame(lc_rast, xy = TRUE)

#changing roi_vec to roi_sf_default
roi_sf_default <- read_sf(here("nlcd_data",
                        "hawaii_2001", 
                        "parks_state",
                        "parks_state.shp")) %>% 
  st_transform(st_crs(lc_rast))

### carbon tons C per hectare data frame
carbon_tph <- read_xlsx(here("carbon_stock_cfs", "carbon_storage_ton_C_per_hectare.xlsx")) %>% 
  clean_names() %>% 
  select(class:soc) %>% 
  pivot_longer(cols = above:soc, 
               names_to = "compartment", 
               values_to = "ton_c_per_hectare")

### land cover class descriptions 
class_names <- read_xlsx(here("carbon_stock_cfs", "class_descriptions_key.xlsx")) %>% 
  clean_names()


mycol <- rgb(0, 0, 255, max = 255, alpha = 0, names = "NA")
color_map <- c("NA" = mycol, 
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
ui <- fluidPage( theme = bs_theme(bootswatch = "sandstone"),
                navbarPage(
                  "Land Use Carbon Stock & Conversion",
                  ### TAB 1: Info, Upload Button                                    
                  tabPanel(
                    'Upload Data',
               
                    h5(span(strong("Land Use Carbon Stock & Conversion"), style = "color:teal"),"was created by students of the Bren School to
                      assist with analysis of carbon storage potential in Hawaii. By default the app will
                      perform carbon stock analysis on all of the state parks in Hawaii, broken down by the
                      different land types in the parks, and distinguishing above and below ground carbon storage.",
                      br(),
                      strong("Users can:"), 
                      br(),
                      strong("(1)"), 
                         "upload a Geopackage containing polygons(s) outlining a specific region of interest
                          (ROI) they want the analysis performed on,",
                      br(),
                      strong("(2)"),
                      "view current land use data, and",
                      br(),
                      strong("(3)"),"view current carbon storage and model opportunities for improvement."
                   ),
                   
                   hr(),
                   br(),

                    sidebarLayout(
                      sidebarPanel( width = 3, h6("To begin, please upload a Geopackage(.gpkg) containging a polygon(s) that outlines
                                              your ROI.  Please note: all polygons in uploaded Geopackae will be used for carbon storage calculations."),
                        fileInput(inputId = "user_gpkg",
                                  label = strong("Upload geopackage"),
                                  multiple = FALSE,
                                  accept = '.gpkg'),
                        tags$style("
             .btn-file {  
             background-color:teal; 
             border-color: teal; 
             }

             .progress-bar {
             background-color: teal;
             }

             ")
                      ), #end sidebar panel
                      
                      mainPanel(
                        tmapOutput('roi_map', width = '1030px'), 
                      ) #end main panel
                    ), #end sidebar layout
                    hr(),
             tags$blockquote(p(span(strong("Data Sources"), style = "color:teal"), br(),
                               "Land Use Data:
                                     Homer, Collin G., Huang, Chengquan, Yang, Limin, Wylie, 
                                     Bruce K., Coan, Michael J., Development of a 2001 National 
                                     Land Cover Database for the United States: Photogrammetric 
                                     Engineering and Remote Sensing, v. 70, no. 7, p. 829–840, at
                                    https://doi.org/10.14358/PERS.70.7.829.", br(),
                               "Hawaii State 
                                    Park Vector File: Hawaii Statewide GIS Program at
                                    https://planning.hawaii.gov/gis/download-gis-data-expanded/.")),
                   
                     hr(),
                  ), #end info tab panel
                  
                  
                  
                  ### TAB 2: Land Cover Map, Pie Chart          
                  tabPanel("Shapefile Visualization", 
                      
                      h4(span(strong("Map of ROI by Land Use Classification"), style = "color:teal")),
                      p(style="text-align: justify; font-size = 30px",
                        "The map below shows the ROI on top of a base map that is colored according to NLCD land
               use classifications, with each pixel representing 1 hectare (100m x 100m). See the legend
               for a description of each land use classification. Zoom in for more details."),
               
               tmapOutput("base_map"),
               
               br(),
               
               h4(span(strong("Chart of Land Use Within ROI"), style="color:teal")),
               p(style="text-align: justify; font-size = 30px",
                 "Land use classifications within the ROI are broken down in the chart below."),
               
               plotOutput("piechart"),
               
               #  
               #   br(),
               #   hr(),
               #   tags$blockquote("This Shiny app is still under continuous development. 
               # Please look forward to future updates!"),
               # hr(),
               # 
               # verbatimTextOutput("summary")
                  ),
               
               
               
               
               ### TAB 3: Carbon Stock Bar Chart, Table, Transformations            
               tabPanel("Carbon Storage Analysis", 
                      
                      h4(span(strong("Carbon Storage in ROI by Compartment"), style="color:teal")),
                      p(style="text-align: justify; font-size = 30px",
                        "The chart below shows current carbon storage for each land use type within the ROI.
                       See the legend for a breakdown of carbon storage by compartment. The five compartments 
                       listed are above ground, below ground, soil organic carbon (organic matter within soil),
                       dead matter, and litter."),
                      
                      # p(style="text-align: justify: font-size = 25px",
                      #   "This tab will display a bar chart showing areas of low, medium, and high carbon
                      #   storage as a percentage of the mapped region. Below the chart, the user will be able to
                      #   choose a land transformation from a dropdown menu and input an area to be transformed.
                      #   For example, the user can select 'Wetland to Forest' and input 100 sq. kilometers.
                      #   The chart will update automatically, showing the new %s and difference from the original in
                      #   a shaded region of each bar in the chart. This functionality will help the user to gain 
                      #   insight into potential land transformations that could increase carbon storage within the
                      #   mapped region."),
                      
                      plotOutput("carbon_plot"),
                      
                      br(),
                      
                      h4(span(strong("Land Transformation Analysis (Interactive Tool)"), style="color:teal")),
                      p(style="text-align: justify: font-size = 30px",
                        "This tool can be used to calculate the impact of land transformations on the carbon
                       stocks plotted above. Please first select a CURRENT land use type (A), then select a 
                       TARGET land use type (B). Finally, input an area to transform. This area should be less
                       than or equal to the area of the CURRENT land use type (A)."),
                      
                      sidebarLayout(
                        sidebarPanel( width = 3,
                          selectInput("select_from", label = h5("(A) Transform FROM:"), 
                                      choices = list("Open Water"= 11, 
                                                     "Perennial Ice/Snow" = 12, 
                                                     "Developed, Open Space" = 21,
                                                     "Developed, Low Intensity" = 22,
                                                     "Developed, Medium Intensity" = 23,
                                                     "Developed, High Intensity" = 24,
                                                     "Barren Land (Rock/Sand/Clay)" = 31,
                                                     "Deciduous Forest" = 41,
                                                     "Evergreen Forest" = 42,
                                                     "Mixed Forest" = 43,
                                                     "Dwarf Scrub" = 51,
                                                     "Shrub/Scrub" = 52,
                                                     "Grassland/Herbaceous" = 71,
                                                     "Sedge/Herbaceous" = 72,
                                                     "Lichens" = 73,
                                                     "Moss" = 74,
                                                     "Pasture/Hay" = 81,
                                                     "Cultivated Crops" = 82,
                                                     "Woody Wetlands" = 90,
                                                     "Emergent Herbaceous Woodlands" = 95), 
                                      selected = 11),
                          
                          selectInput("select_to", label = h5("(B) Transform TO:"), 
                                      choices = list("Open Water"= 11, 
                                                     "Perennial Ice/Snow" = 12, 
                                                     "Developed, Open Space" = 21,
                                                     "Developed, Low Intensity" = 22,
                                                     "Developed, Medium Intensity" = 23,
                                                     "Developed, High Intensity" = 24,
                                                     "Barren Land (Rock/Sand/Clay)" = 31,
                                                     "Deciduous Forest" = 41,
                                                     "Evergreen Forest" = 42,
                                                     "Mixed Forest" = 43,
                                                     "Dwarf Scrub" = 51,
                                                     "Shrub/Scrub" = 52,
                                                     "Grassland/Herbaceous" = 71,
                                                     "Sedge/Herbaceous" = 72,
                                                     "Lichens" = 73,
                                                     "Moss" = 74,
                                                     "Pasture/Hay" = 81,
                                                     "Cultivated Crops" = 82,
                                                     "Woody Wetlands" = 90,
                                                     "Emergent Herbaceous Woodlands" = 95), 
                                      selected = 11),
                          
                          numericInput("area_transform", label = h5("(C) Area of Transformation (ha):"), value = 0),
                          
                          actionButton("recalculate", "Recalculate"),
                          tags$style("
             .btn-action {  
             background-color:teal; 
             border-color: teal; 
             }

             "),
                        ), 
                        
                        mainPanel(
                          dataTableOutput("updated_carbon_table")
                        ) 
                      ), #end sidebar layout
                      
                      br(),
                      
                      h4(span(strong("Download Transformation Data"), style="color:teal")),
                      
                      p(style="text-align: justify: font-size = 30px",
                        "Use the button below to download the land transformation chart above as a spreadsheet
                       (.csv). The spreadsheet will also include the land use classification, description,
                       area (ha), and current carbon storage (in tons and as percentage of total). This data can
                       be used to determine which land transformations would be most effective in boosting the
                       carbon storage potential of the ROI."),
                      
                      # p("Here, the user will also be able to download the data corresponding to the chart above, as
                      #   modified using the above widgets. The download format would be a spreadsheet showing the post-transformation
                      #   land use types, area (sq km), carbon storage (kg CO2e), and carbon storage (% of total) 
                      #   within the mapped region. We may also include other data types such as delta area or delta carbon storage 
                      #   compared to the original map or cost of the specified land transformation(s).
                      #   This functionality would be useful to a user who, for instance, intends to produce a report
                      #   on the most effective land transformations to increase carbon storage within the mapped region."),
                      
                      downloadButton("download", label = "Download"),
             tags$style("
             .btn-download {  
             background-color:teal; 
             border-color: teal; 
             }

             .progress-bar {
             background-color: teal;
             }

             "),
                      hr(),
                      
               ) # end of tab 3
                ) # end navbar 
) ### End of UI Block


### Server Block
server <- function(input, output) {
  
  ### Read in user uploaded geopackage
  user_gpkg <- reactive({
    if (is.null(input$user_gpkg)) {
      return(NULL)
    }
    layer_name <- ogrListLayers(dsn = input$user_gpkg$datapath)[1] # Get the first layer name
    readOGR(dsn = input$user_gpkg$datapath, layer = layer_name)
  })
 
  ### Create a reactive object for the roi_sf based on user upload or default
  roi_sf <- reactive({
    if (is.null(user_gpkg())) {
      return(roi_sf_default)
    } else {
      st_transform(as(user_gpkg(), "sf"), crs(lc_rast)) 
    }
  })
  
  ### Mask raster with vector file
  ### This is getting the land cover data from lc_rast for the ROI
  lc_roi_rast <- reactive({
    if (!is.null(roi_sf())) {
      mask(lc_rast, roi_sf())
    }
  })
  
  
  ### Interactive land cover map
  output$base_map <- renderTmap({
    tm_basemap(leaflet::providers$Stamen.Watercolor) +
    tm_shape(lc_rast_sub) +
      tm_raster(style = "cat", 
                palette = c("blue", "lightpink", "coral1", "red", "darkred", "tan", "darkgreen", 
                                       "darkgoldenrod3", "darkkhaki", "khaki1", "brown", "lightcyan", "lightseagreen"), 
                                       labels = c("Open Water", "Developed, Open Space", "Developed, Low Intensity", 
                                                  "Developed, Medium Intensity", "Developed, High Intensity", "Barren Land (Rock/Sand/Clay)", 
                                                  "Evergreen Forest", "Shrub/Scrub", "Grassland/Herbaceous", "Pasture/Hay", "Cultivated Crops", 
                                                  "Woody Wetlands", "Emergent Herbaceous Wetlands"), 
                n= 14, 
                title = "Land Use Type") +
     tm_shape(roi_sf()) +
     tm_borders(col = "black", lwd = 2)
  })
  
  
  ### Interactive streetmap 
  output$roi_map <- renderTmap({
    tm_basemap(leaflet::providers$OpenStreetMap.Mapnik) +
      tm_shape(roi_sf()) +
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
    ggplot(roi_area(), aes(x = "", y = area_hectares, fill = description)) +
      geom_bar(width = 1, stat = "identity", color='black', size=.3) +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = color_map) + # Use the color_map vector to specify colors
      theme_void() + 
      theme(legend.position = "right") +
      labs(fill = "Land Use Cover", title = "Land Use Cover Percentage")
  })
  
  ### Carbon tab calculations
  ### Convert lc_roi_rast() to df for manipulation
  lc_roi_df <- reactive({
    as.data.frame(lc_roi_rast(), xy = TRUE)
  }) 
  
  ### roi_area reactive expression
  roi_area <- reactive({
    req(lc_roi_df())
    
    lc_roi_df() %>% 
      clean_names() %>%
      group_by(land_cover_class) %>%
      tally(name = "area_hectares") %>% 
      merge(., class_names, by.x = "land_cover_class", by.y = "class")
  })
  
  ### roi_carb_per_area reactive expression
  roi_carb_per_area <- reactive({
    merge(roi_area(), carbon_tph, 
          by.x = "land_cover_class", by.y = "class") %>%
      mutate(total_carbon_tons = (area_hectares * ton_c_per_hectare)) %>% 
      mutate(total_carbon_log_tons = log(total_carbon_tons))
  })
  
  #%>% 
  # mutate(error_low = 0.7*total_carbon_log_tons) %>% 
  # mutate(error_high = 1.3*total_carbon_log_tons) ### Note to Caitlin: May want to add these in later.
  
  # roi_carbon_table <- roi_carb_per_area %>% 
  #   select(description, area_hectares, compartment, total_carbon_tons) %>% 
  #   pivot_wider(names_from = "compartment", values_from = "total_carbon_tons")
  # roi_carbon_table <- roi_carbon_table[, c("land_cover_class", 
  #                                          "description", 
  #                                          "area_hectares", 
  #                                          "above", 
  #                                          "below", 
  #                                          "soc", 
  #                                          "dead_matter", 
  #                                          "litter")]
  
  
  
  output$carbon_plot <- renderPlot({
    ggplot() +
      geom_col(data = roi_carb_per_area(), 
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
      # geom_errorbar(data = roi_carb_per_area,
      #               mapping = aes(x = description,
      #                             ymin = error_low,
      #                             ymax = error_high)) +
      theme(axis.text.x = element_text(angle = 45, 
                                       vjust = 1, 
                                       hjust=1
      )) +
      labs(x = "", 
           y = "Carbon (log tons)", 
           title = "Carbon Storage Per Land Use Type in Hawaii", 
           fill = "Compartment")
  })
  
  reactive_carbon_table <- eventReactive(input$recalculate, {
    selectfrom <- input$select_from
    selectto <- input$select_to
    areatransform <- input$area_transform
    updated_carbon_table <- roi_carb_per_area() %>% 
      mutate(area_hectares = ifelse(land_cover_class == selectfrom, area_hectares - areatransform, area_hectares)) %>% 
      mutate(area_hectares = ifelse(land_cover_class == selectto, area_hectares + areatransform, area_hectares)) %>% 
      mutate(total_carbon_tons = area_hectares * ton_c_per_hectare) %>% 
      mutate(total_carbon_tons_log = log(total_carbon_tons)) %>% 
      select(description, area_hectares, compartment, total_carbon_tons) %>% 
      pivot_wider(names_from = "compartment", values_from = "total_carbon_tons") %>% 
      mutate(total_c = above + below + soc + dead_matter + litter)
    summary_row <- data.frame(list(description = "All Land Use Types", 
                                   area_hectares = sum(updated_carbon_table$area_hectares), 
                                   above = sum(updated_carbon_table$above), 
                                   below = sum(updated_carbon_table$below), 
                                   soc = sum(updated_carbon_table$soc),
                                   dead_matter = sum(updated_carbon_table$dead_matter),
                                   litter = sum(updated_carbon_table$litter),
                                   total_c = sum(updated_carbon_table$total_c)))
    updated_carbon_table <- rbind(updated_carbon_table, summary_row)
    updated_carbon_table
  },ignoreNULL = FALSE)
  
  output$updated_carbon_table <- renderDataTable({
    reactive_carbon_table()
  })
  
  # output$carbon_table_test <- renderTable(roi_carbon_table)
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
  
  output$download <- downloadHandler(
    filename = "land_transformation.csv",
    content = function(file){
      write.csv(reactive_carbon_table(), file)
    }
  )
  
}


### Run App
shinyApp(ui = ui, server = server)

