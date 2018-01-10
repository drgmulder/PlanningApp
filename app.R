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
                            actionButton("draw", "Set values")
                            
                  )),
           column(8,
                  plotOutput("distPlot")) #end column
           
  ), #end first fluidRow
  #add panel for PFP
  fluidRow(column(4,
                  wellPanel(strong("Sample size planning"), p(),
                            numericInput("tMOE", "Target MOE:", value=0.5),
                            numericInput("assu", "Assurance:", value=.80, min = 0, max=.99),
                            actionButton("plan", "Get sample sizes")
                            
                  ) #end panel
  ), #end first column 
  column(8, verbatimTextOutput(
    "ans"
  )) # end column
  ) #end second fluidRow
) #end fluidPage




#define functions


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
    }) #end renderPlot
  }) #end observeEvent
  
  observeEvent(input$plan, {
    mu1 <- isolate(input$mu1)
    mu2 <- isolate(input$mu2)
    sd <- isolate(input$sd)
    tMOE <- isolate(input$tMOE)
    assu <- isolate(input$assu)
    
    output$ans <- "BLABLABLA"
  }) #end observeEvent2 
    
  }# end server
  


# Run the application 
shinyApp(ui = ui, server = server)