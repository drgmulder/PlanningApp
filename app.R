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
  titlePanel("Sample size planning"),
  
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
  column(8, verbatimTextOutput("ans")
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


#diffPLot function from multicon-package 
#Ryne A. Sherman 
#Maintainer: Ryne A. Sherman <rsherm13@fau.edu> 
#Compiler: David G. Serfass <dserfass@fau.edu>

diffPlot <-- function (x, y, plotFUN = mean, errFUN = c("ci", "se", "sd"), 
                       conf = 0.95, grp.names = NULL, var.equal = FALSE, paired = FALSE, 
                       ylim = NULL, ...) 
{
  se <- function(x) {
    x <- na.omit(x)
    res <- sqrt(var(x)/length(x))
    res
  }
  ci <- function(x) {
    x <- na.omit(x)
    alpha <- 1 - (1 - conf)/2
    res <- qt(alpha, length(x) - 2, lower.tail = T) * se(x)
    res
  }
  if (!is.null(errFUN)) {
    errFUN <- match.arg(errFUN)
  }
  dat <- data.frame(x, y)
  dat <- na.omit(dat)
  if (paired == FALSE) {
    res <- tapply(dat[, 2], dat[, 1], plotFUN)
    res <- c(res, res[2])
    Ns <- tapply(dat[, 2], dat[, 1], length)
    Vars <- tapply(dat[, 2], dat[, 1], var)
    if (var.equal == FALSE) {
      poolSE <- sqrt(sum(Vars/Ns))
      df <- (sum(Vars/Ns)^2)/(sum(Vars^2/(Ns^2 * (Ns - 
                                                    1))))
    }
    if (var.equal == TRUE) {
      poolSE <- sqrt(sum((Ns - 1) * Vars)/(sum(Ns - 1))) * 
        sqrt(sum(1/Ns))
      df <- sum(Ns - 1)
    }
    diffBar <- ifelse(errFUN == "ci", qt(1 - (1 - conf)/2, 
                                         df, lower.tail = T) * poolSE, ifelse(errFUN == "se", 
                                                                              poolSE, ifelse(errFUN == "sd", sqrt(sum(Vars)), 0)))
    e <- c(tapply(dat[, 2], dat[, 1], errFUN), diffBar)
  }
  if (paired == TRUE) {
    res <- apply(dat, 2, plotFUN)
    res <- c(res, res[2])
    N <- nrow(dat)
    Vars <- apply(dat, 2, var)
    diffSE <- sd(dat[, 1] - dat[, 2])/sqrt(N)
    df <- N - 1
    diffBar <- ifelse(errFUN == "ci", qt(1 - (1 - conf)/2, 
                                         df, lower.tail = T) * diffSE, ifelse(errFUN == "se", 
                                                                              diffSE, ifelse(errFUN == "sd", sd(dat[, 1] - dat[, 
                                                                                                                               2]), 0)))
    e <- c(apply(dat, 2, errFUN), diffBar)
  }
  e <- ifelse(res <= 0, -e, e)
  if (!is.null(ylim)) {
    lims <- ylim
  }
  else {
    lims <- c(min(res + e, res - e) - abs(0.4 * min(res + 
                                                      e, res - e)), max(res + e, res - e) + 0.4 * abs(max(res + 
                                                                                                            e, res - e)))
  }
  plot(c(1, 2, 4), res, pch = c(19, 19, 17), xaxt = "n", xlim = c(0.4, 
                                                                  0.4 + 4), ylim = c(lims), bty = "l", ...)
  if (!is.null(errFUN)) {
    arrows(c(1, 2, 4), res + e, c(1, 2, 4), res - e, angle = 90, 
           code = 3, length = 0.08)
    arrows(2, res[2], 4.4, res[3], angle = 90, code = 3, 
           length = 0, lty = 2)
    arrows(1, res[1], 4.4, res[1], angle = 90, code = 3, 
           length = 0, lty = 2)
    if (res[1] >= res[2]) {
      val <- axTicks(4) - axTicks(4)[min(which(axTicks(4) > 
                                                 median(axTicks(4))))]
      loc <- axTicks(4) - (axTicks(4)[min(which(axTicks(4) > 
                                                  median(axTicks(4))))] - res[1])
    }
    if (res[1] < res[2]) {
      val <- axTicks(4) - axTicks(4)[max(which(axTicks(4) < 
                                                 median(axTicks(4))))]
      loc <- axTicks(4) - (axTicks(4)[max(which(axTicks(4) < 
                                                  median(axTicks(4))))] - res[1])
    }
    axis(4, at = loc, labels = val, ...)
  }
  if (is.null(grp.names)) {
    grp.names <- c("x", "y")
  }
  axis(1, at = c(1, 2, 4), labels = c(grp.names, "Difference"))
}

# Define server logic 

server <- function(input, output) {
  mu1 = 100
  mu2 = 100
  sd = 15
  x <- seq(-4*sd+mu1, 4*sd+mu2, length=200)
  
  output$distPlot <- renderPlot(plot(x, 
                                     dnorm(x, 100, 15), 
                                     type="l", 
                                     ylab="Density", 
                                     main="Populations"))
  #wait for setting population values and update plot
  observeEvent(input$draw, {
    mu1 <- isolate(input$mu1)
    mu2 <- isolate(input$mu2)
    sd <- isolate(input$sd)
    x <- seq(-4*sd+mu1, 4*sd+mu2, length=200)
    output$distPlot <- renderPlot({
      plot(x, dnorm(x, mu1, sd), type="l", main="Populations", ylab="Density")
      points(x, dnorm(x, mu2, sd), type="l", lty=3)
    }) #end renderPlot
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
    output$ans <- renderPrint(answer)
    diff = abs(isolate(input$mu1 - input$mu2))
    se = sqrt(2*sd^2/ceiling(answer))
    x = seq(-4, 4, length=200)
    x = x*se + diff
    
    output$sampDist <- renderPlot(plot(x, dnorm(x, diff, se),
                                       ylab="Density",
                                       xlab="Sample difference between means",
                                       main="Sampling Distribution of Difference"))
  
    }) #end observeEvent2 
    
  }# end server
  


# Run the application 
shinyApp(ui = ui, server = server)