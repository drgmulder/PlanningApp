#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Generate normal variable"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("mean",
                     "Set population mean:",
                     min = 50,
                     max = 150,
                     value = 100),
         sliderInput("sd",
                     "Set population sd:",
                     min = 5,
                     max = 25,
                     value=15),
         sliderInput("n",
                     "Set sample size:",
                     min = 2,
                     max = 500,
                     value = 100)
      ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- rnorm(input$n, input$mean, input$sd)
            # draw the histogram with the specified number of bins
      plot(density(x))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

