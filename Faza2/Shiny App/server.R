
library(shiny)
library(ggplot2)
library(reshape)
ciekawe <- read.table("polska2012pisa.csv", header=TRUE, sep=",")
ciekawe <- ciekawe[, -1]


shinyServer(function(input, output) {
   
  
   
   
   data <- reactive({
      na.omit(data.frame(in.rows = ciekawe[,c(input$which.col)],
                    in.columns = ciekawe[,c(input$which.row)],
                  score = ciekawe[,c(input$which.score)]))
   })
   
      
   output$summary <- renderPrint({
      print(summary(data()))
   })
   
   output$distPlot <- renderPlot({
      p <- qplot(score, 
                 facets = in.rows~in.columns,
                 data = data(),
                 geometry = "hist", 
                 margins = input$margins,
                 main = paste("Polish children test results"),
                 binwidth = input$bin)
      print(p)
            
   })
   
})




