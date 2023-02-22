library(shiny)
library(bslib) ### Custom themes. Run command bs_theme_preview() in console.


# Define UI for application that draws a histogram
ui <- fluidPage(theme = bs_theme(bootswatch = "minty"),
                navbarPage(
                  "Land Use Carbon Stock & Conversion",
                  
                  tabPanel(
                    'Info',
                    
    # Application title
    

    # Sidebar with a slider input for number of bins 
    #sidebarLayout(
       # sidebarPanel(
           # sliderInput("bins",
                      #  "Number of bins:",
                       # min = 1,
                      #  max = 50,
                       # value = 30)
        #),

        # Show a plot of the generated distribution
    
        #mainPanel(
         # tabsetPanel(
           # tabPanel("Info", 
                     fluidRow(""),
                     fluidRow(h5("This app was created by Chloe, Caitlin, and Eleanor from MESM 2024. It was a long, difficult journey, but by the end, we finally figured out how to make multiple tabs and populate them. It's not about the destination but about the Shiny apps you make along the way. Peace.")
                                      ),
                     fluidPage(
                       
                       # Copy the line below to make a file upload manager
                       fileInput("file", label = h3("Upload Shapefile (.shp)")),
                       
                       hr(),
                       fluidRow(column(4, verbatimTextOutput("value"))),
                       fluidRow(h6("Citation please."))
                       
                     )),
            
            
            
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
           
           verbatimTextOutput("summary")),
            
            
            
            
            
            
            tabPanel("Land Transformations", 
                     tableOutput("table"))
          )
        )
  # )
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
