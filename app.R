#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI 
ui <- fluidPage(
   
   # Application title
   titlePanel("Generate normal variable"),
   
   # Sidebar with input fields for specification of
   # population
   fluidRow(column(4, 
                  wellPanel(strong("Set population values"), p(),
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
                                              "Population sd:",
                                              min = 5,
                                              max = 25,
                                              value=15),
                                 actionButton("draw", "Draw Populations")
                                 
   )),
   column(8,
          plotOutput("distPlot")) #end column
   
   ) #end fluidRow 
  ) #end fluidPage

# Define server logic 
server <- function(input, output) {
   observeEvent(input$draw, {
     mu1 <- isolate(input$mu1)
     mu2 <- isolate(input$mu2)
     sd <- isolate(input$sd)
     x <- seq(-4*sd+mu1, 4*sd+mu2, length=200)
     output$distPlot <- renderPlot({
       plot(x, dnorm(x, mu1, sd), type="l", main="Populations")
       points(x, dnorm(x, mu2, sd), type="l", lty=3)
     })
   })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

