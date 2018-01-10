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
        numericInput("mu1", 
                     "Mean population 1:", 
                     100,
                     min = 1,
                     max = 1000),
         numericInput("mu2",
                     "Mean population 2:",
                     100,
                     min = 1,
                     max = 1000),
         numericInput("sd",
                     "Set population sd:",
                     min = 5,
                     max = 25,
                     value=15),
        actionButton("draw", "Draw Populations")
               ),
        
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   observeEvent(input$draw, {
     mu1 <- isolate(input$mu1)
     mu2 <- isolate(input$mu2)
     sd <- isolate(input$sd)
     x <- seq(-4*sd+mu1, 4*sd+mu2, length=200)
     output$distPlot <- renderPlot({
       plot(x, dnorm(x, mu1, sd), type="l")
       points(x, dnorm(x, mu2, sd), type="l", lty=3)
     })
   })
   # output$distPlot <- renderPlot({
   #    # generate bins based on input$bins from ui.R
   #    x    <- seq()
   #          # draw the histogram with the specified number of bins
   #    plot(density(x))
   # })
}

# Run the application 
shinyApp(ui = ui, server = server)

