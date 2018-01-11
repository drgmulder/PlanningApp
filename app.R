#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(multicon)

# Define UI 
ui <- fluidPage(
  
  # Application title
  titlePanel("Sample size planning for two independent groups"),
  
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
                                         107.5,
                                         min = 1,
                                         max = 1000),
                            numericInput("sd",
                                         "Population sd:",
                                         min = 5,
                                         max = 25,
                                         value=15),
                            actionButton("draw", "Set values")
                            
                  )),
           column(8, tabsetPanel(
             tabPanel("Population", plotOutput("distPlot")),
             tabPanel("Sampling Distribution", plotOutput("sampDist"))
           ) #end tabsetPanel
                  ) #end column
           
  ), #end first fluidRow
  #add panel for PFP
  fluidRow(column(4,
                  wellPanel(strong("Sample size planning"), p(),
                            numericInput("tMOE", "Target MOE:", value=0.5, min = .01),
                            numericInput("assu", "Assurance:", value=.80, min = 0, max=.99),
                            actionButton("plan", "Get sample sizes")
                            
                  ) #end panel
  ), #end first column 
  column(8, tabsetPanel(
    tabPanel("Required Sample size", verbatimTextOutput("ans")),
    tabPanel("Expected Results", plotOutput("expResults"))
  ) # end tabsetPanel
  ) # end column
  ) #end second fluidRow
) #end fluidPage

#define planning functions

#Margin of Error
calcMOE <- function(n, sd) {
  df = 2*n - 2
  se = sqrt(2*sd^2/n)
  moe = qt(.975, df)*se
}

#Margin of Error with assurance 
calcMOE.assu <- function(n, sd, assu) {
  df = 2*n - 2
  sd.assu = sqrt(sd^2*qchisq(assu, df)/df)
  se.assu = sqrt(2*sd.assu^2/n)
  moe.assu = qt(.975, df)*se.assu
} 


# Define server logic 

server <- function(input, output) {
  mu1 = 100
  mu2 = 100
  sd = 15
  x <- seq(-4*sd+mu1, 4*sd+mu2, length=200)
  
  output$distPlot <- renderPlot({plot(x, 
                                     dnorm(x, 100, 15), 
                                     type="l", 
                                     ylab="Density", 
                                     main="Populations")
                                points(x, dnorm(x, 107.5, 15), lty=3, type="l")}, height=400, width=600)
  #wait for setting population values and update plot
  observeEvent(input$draw, {
    mu1 <- isolate(input$mu1)
    mu2 <- isolate(input$mu2)
    sd <- isolate(input$sd)
    x <- seq(-4*sd+mu1, 4*sd+mu2, length=200)
    output$distPlot <- renderPlot({
      plot(x, dnorm(x, mu1, sd), type="l", main="Populations", ylab="Density")
      points(x, dnorm(x, mu2, sd), type="l", lty=3)
    }, height=400, width=600) #end renderPlot
  }) #end observeEvent
  
  observeEvent(input$plan, {
    mu1 <- isolate(input$mu1)
    mu2 <- isolate(input$mu2)
    sd <- isolate(input$sd)
    tMOE <- isolate(input$tMOE) 
    assu <- isolate(input$assu)
    
    tMOE = tMOE*sd #not a fraction of sd as in input
    
    cost = function(n, tMOE) {
      cost = (calcMOE.assu(n, sd = sd, assu=assu) - tMOE)^2
    }
    answer = optimize(cost, interval=c(20, 5000), tMOE=tMOE)$minimum
    se = sqrt(2*sd^2/ceiling(answer))
    output$ans <- renderPrint(c(answer, mu1, mu2, sd, tMOE, assu, se))
    diff = abs(isolate(input$mu1 - input$mu2))
    se = sqrt(2*sd^2/ceiling(answer))
    x = seq(-4, 4, length=200)
    x = x*se + diff
    
    output$sampDist <- renderPlot(plot(x, dnorm(x, diff, se),
                                       ylab="Density",
                                       xlab="Sample difference between means",
                                       main="Sampling Distribution of Difference",
                                       type="l"), height=400, width=600)
    
    g1 <- rnorm(ceiling(answer))
    g1 <- unname(scale(g1)*sd + mu1)
    g2 <- g1 + diff
    dep <- c(g1, g2)
    ind <- rep(c(1, 2), each=length(g1))
    
    output$expResults <- renderPlot(diffPlot(ind, dep, xlab=""), height=400, width=600)
  
    }) #end observeEvent2 
    
  }# end server
  


# Run the application 
shinyApp(ui = ui, server = server)