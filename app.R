library(shiny)
library(bslib) ### Custom themes. Run command bs_theme_preview() in console.


# Define UI for application that draws a histogram
ui <- fluidPage(theme = bs_theme(bootswatch = "minty"),
                navbarPage(
                  "Land Use Carbon Stock & Conversion",
                  
                  tabPanel(
                    'Info',
                    fluidRow(""),
                    fluidRow(h5("This App will allow users to upload a shapefile 
                                with landuse data for an area, and in return will 
                                be shown how much cabon is stored in various 
                                land uses, and how their carbon storage potential 
                                would change by changing land type.")
                    ),
                    fluidPage(
                      
                      hr(),
                      fluidRow(column(4, verbatimTextOutput("value"))),
                      fluidRow(h6("Citation please."))
                      
                    ),
                    sidebarLayout(
                      sidebarPanel(
                        fileInput("file", label = h3("Upload Shapefile (.shp)"))
                      ), #end sidebar panel
                      
                      mainPanel(
              
                      plotOutput(  tmap_options(check.and.fix = TRUE),
                        tm_shape(hawaii_coarse) +
                          tm_raster(palette = c(
                            "0" = "lightblue",
                            "11" = "blue",
                            "31" = "pink",
                            "22" = "red",
                            "52" = "green",
                            "42" = "darkgreen"), n= 14) +
                          tm_shape(hawaii_parks_vector) +
                          tm_borders(col = "black", lwd = 2))
                       
                        
                        # plotOutput('load_pic_plot', height = '600px'), # i dont know what this code does but r wanted me to have something for main panel
                      #  textOutput('pic_dim_print')
                      ) #end main panel
                    ) #end sidebar layout
                    

                  ), #end info tab panel
                  

            
            
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
             
             img(src = 'New_York_map.jpeg', height = 500),
             
             br(),
             hr(),
             tags$blockquote("This Shiny app is still under continuous development. 
           Please look forward to future updates!"),
           hr(),
           
           verbatimTextOutput("summary")
           ),

            
            
            
            
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

                     img(src = 'Carbon-storage-by-ecosystem.png', height = 300),
                     
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
                     
                     
                     br(),
                     hr(),
                     
                     tags$blockquote("This Shiny app is still under continuous development. 
                                     Please look forward to future updates!"),
                     
                     hr()),
          )
        )
   #)
#)


# Define server
server <- function(input, output) {

  ### load national land cover data (nlcd)
  nlcd_file <- here("nlcd_data", 
                      "hawaii_2001", "hi_landcover_wimperv_9-30-08_se5.img")
  ### rasterize land cover data
  nlcd_rast <- terra::rast(nlce_file)
  
  ### make nlcd_rast more coarse ie smaller
  nlcd_coarse <- aggregate(nlce_rast, fact=4, fun=modal)
  
  ### convert nlcd_coarse to data frame for pie chart 
  nlcd_df <- as.data.frame(nlcd_coarse, xy = TRUE) %>% 
    filter(`Land Cover Class` != 0) #filter out the 0 no data values
  
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
        
    output$map_img <- renderImage({
      
      list(src = "WWW/New_York_map.jpeg",
           width = "100%",
           height = '100%')
      
    }, deleteFile = F)
    })


# Run the application 
shinyApp(ui = ui, server = server)
