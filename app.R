library(shiny)
library(bslib) ### Custom themes. Run command bs_theme_preview() in console.


# Define UI for application that draws a histogram
ui <- fluidPage(theme = bs_theme(bootswatch = "minty"),
                navbarPage(
                  "Land Use Carbon Stock & Conversion",
                  
                  tabPanel(
                    'Info',
                    fluidRow(""),
                     fluidRow(h5("This App will allow users to upload a shapefile with landuse data for an area, and in return will be shown how much cabon is stored in various land uses, and how their carbon storage potential would change by changing land type.")
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
                     verbatimTextOutput("summary")),
            
            
            
            
            
            
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
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
