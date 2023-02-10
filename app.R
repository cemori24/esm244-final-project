library(shiny)
library(bslib) ### Custom themes. Run command bs_theme_preview() in console.


# Define UI for application that draws a histogram
ui <- fluidPage(theme = bs_theme(bootswatch = "minty"),
                navbarPage("Land Use Carbon Stock & Conversion"),

                
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
        mainPanel(
          tabsetPanel(
            tabPanel("Info", print("plot")),
            tabPanel("Land Cover Map", verbatimTextOutput("summary")),
            tabPanel("Land Transformations", tableOutput("table"))
          )
        )
   )
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
