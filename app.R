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
                        plotOutput('load_pic_plot', height = '600px'), # i dont know what this code does but r wanted me to have something for main panel
                        textOutput('pic_dim_print')
                      ) #end main panel
                    ) #end sidebar layout
                    
                  ), #end info tab panel
                  
            
            
            
    tabPanel("Land Cover Map", 
             imageOutput("map_img"),
             br(),
             hr(),
             h4(strong("Land Cover Shapefile Visualization")),
             p(style="text-align: justify; font-size = 25px",
               "Eventually, this page will display an interactive 
                       map that visualizes the carbon storage data. There will
                       likely be a color scale, with green representing a lot 
                       of stored carbon and red representing little to none. Alternatively, 
                       the color scale would represent potential for carbon storage."),
             
             tags$blockquote("This Shiny app is still under continuous development. 
           Please look forward to future updates!"),
           hr(),
           
           verbatimTextOutput("summary")
           ),
#>>>>>>> 6c58eede2c433dc40a4ee301e34d8e831839a87f
            
            
            
            
            
            
            tabPanel("Land Transformations", 
                     tableOutput("table"))
          )
        )
   #)
#)


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
        
        
    output$map_img <- renderImage({
      
      list(src = "WWW/New_York_map.jpeg",
           width = "100%",
           height = '100%')
      
    }, deleteFile = F)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
