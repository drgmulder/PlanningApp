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
                                         0,
                                         min = 0,
                                         max = 1000),
                            numericInput("mu2",
                                         "Mean population 2:",
                                         0.5,
                                         min = 0,
                                         max = 1000),
                            numericInput("sd",
                                         "Population sd:",
                                         min = 1,
                                         max = 25,
                                         value=1),
                            numericInput("sampleSize",
                                         "Sample size:", 
                                         min = 20,
                                         max = 5000,
                                         value = 20),
                            actionButton("draw", "Set values")
                            
                  )), #end fist column
           column(8, tabsetPanel(
             tabPanel("Population", plotOutput("distPlot")),
             tabPanel("Sampling Distribution", plotOutput("sampDist")),
             tabPanel("Distribution of t", plotOutput("tDist"))
           ) #end tabsetPanel
                  ) #end second column
           
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
  mu1 = 0
  mu2 = 0.5
  sd = 1
  n = 20 
  se = sqrt(2/n)*sd
  df = 2*(n - 1)
  ncp = .5/se
  
  x <- seq(-4, 4, length=200)
  
  output$distPlot <- renderPlot({plot(x, 
                                     dnorm(x, 0, 1), 
                                     type="l", 
                                     ylab="Density", 
                                     main="Populations")
                                points(x, dnorm(x, mean=.5, sd=1), lty=3, type="l")}, height=400, width=600)
  
  diffs <- seq(0.5-4*se, 0.5+4*se, length=200)
  
  output$sampDist <- renderPlot({plot(diffs, dnorm(diffs, 0.5, se), type="l", main="Sampling Distribution of Difference", 
                                      ylab="Density", xlab="Sample means difference")
                      abline(v = 0.5, lty=3)
                        }, height=400, width=600) #end RenderPlot Sampling Distribution
  
  t <- seq(ncp-4, ncp+4, length=100)
  
  output$tDist <- renderPlot({plot(t, dt(t, df, ncp), type="l", ylab="Density")
    abline(v=c(qt(.025, df), qt(.975, df)), lty=3) 
  }, height=400, width=600)
  
  
  #wait for setting population values and update plot
  observeEvent(input$draw, {
    mu1 <- isolate(input$mu1)
    mu2 <- isolate(input$mu2)
    sd <- isolate(input$sd)
    n <- isolate(input$sampleSize)
    
  
    se = sqrt(2/n)*sd
    diff = abs(mu2 - mu1)
    ncp = diff/se
    df = 2*(n - 1)
    
    x <- seq(-4*sd+mu1, 4*sd+mu2, length=200)
    
    output$distPlot <- renderPlot({
      plot(x, dnorm(x, mu1, sd), type="l", main="Populations", ylab="Density")
      points(x, dnorm(x, mu2, sd), type="l", lty=3)
    }, height=400, width=600) #end renderPlot
    
    diffs <- seq(diff-4*se, diff+4*se, length=200) #not a great idea to use vars diff and diffs...
    
    output$sampDist <- renderPlot({plot(diffs, dnorm(diffs, diff, se), type="l", main="Sampling Distribution of Difference", 
                                        ylab="Density", xlab="Sample means difference")
      abline(v = diff, lty=3)
    }, height=400, width=600) #end RenderPlot Sampling Distribution
    
    t <- seq(ncp-4, ncp+4, length=200)
    
    output$tDist <- renderPlot({plot(t, dt(t, df, ncp), type="l", ylab="Density")
      abline(v=c(qt(.025, df), qt(.975, df)), lty=3) 
    }, height=400, width=600)
    
    
  }) #end observeEvent
  
  observeEvent(input$plan, { #this happens when sample size planning button is pushed
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
    
    #update the sample size in population pane:
    
    updateNumericInput(session, "sampleSize", value=ceiling(answer))
    
    
    
    diff = abs(isolate(input$mu1 - input$mu2))
    d = diff/sd
    se = sqrt(2*sd^2/ceiling(answer))
    ncp = diff/se
    df = 2*ceiling(answer) - 2
    pow = (1 - pt(qt(.975, df), df, ncp)) + pt(qt(.025, df), df, ncp)
    eMOE = calcMOE(ceiling(answer), sd)
    
    output$ans <- renderPrint(c("Sample size:"=answer, "Expected Moe:"=eMOE, "Power:"=pow, "Cohen's d:"=d ))
    
    
    x = seq(-4, 4, length=200)
    x = x*se + diff
     
    
    output$sampDist <- renderPlot(plot(x, dnorm(x, diff, se),
                                       ylab="Density",
                                       xlab="Sample difference between means",
                                       main="Sampling Distribution of Difference",
                                       type="l"), height=400, width=600)
    t <- seq(ncp-4, ncp+4, length=100)
    output$tDist <- renderPlot({plot(t, dt(t, df, ncp), type="l", ylab="Density")
     abline(v=c(qt(.025, df), qt(.975, df)), lty=3) 
    }, height=400, width=600)
    
    g1 <- rnorm(ceiling(answer))
    g1 <- unname(scale(g1)*sd + mu1)
    g2 <- g1 + diff
    dep <- c(g1, g2)
    ind <- rep(c(1, 2), each=length(g1))
    
    output$expResults <- renderPlot(diffPlot(ind, dep, xlab="", grp.names=c("Control", "Experimental")), height=400, width=600)
  
    }) #end observeEvent2 
    
  }# end server
  


# Run the application 
shinyApp(ui = ui, server = server)