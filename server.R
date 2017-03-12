require(shiny)
require(ggplot2)
require(grid)
require(scales)
require(caTools)

source('truth_plot.R')
source('truth_table.R')
source('stat_table.R')
source('legend_plot.R')

shinyServer(function(input, output, session) {

    dat <- reactiveValues()
    dat$stat.type <- 'None'
    observe({
        dat$FN <- -as.numeric(input$FN)
        dat$TN <- as.numeric(input$TN)
        dat$FP <- -as.numeric(input$FP)
        dat$TP <- as.numeric(input$TP)
        dat$d  <- data.frame(
            FN=-as.numeric(input$FN), 
            TN=as.numeric(input$TN), 
            FP=-as.numeric(input$FP), 
            TP=as.numeric(input$TP)
        )
        dat$N <- sum(as.numeric(c(input$FN, input$TN, input$FP, input$TP)))
        dat$square <- input$square
        dat$stat.type <- input$stat.type
    })
    
    output$stat_html <- renderUI({
        HTML(stat.table(dat, input$stat.type))
    })
    
    output$truth <- renderPlot({
        print(truth(dat$d, dat$stat.type, dat$square))
    }, width=1200, height=600)
    
    output$legend_plot <- renderPlot({
        legend_plot(dat$stat.type)
    }, width=200, height=200)
    
    createPdfPlot <- function(){
         print('createPdfPlot:: starting')
         temp <- tempfile(fileext=".pdf")
         pdf(file=temp, height=10, width=20)
         print('createPdfPlot:: pdfing')
         print(truth(dat$d, dat$stat.type, dat$square, is.pdf=T))
         dev.off()
         print('createPdfPlot:: finishing')
         return(temp)
     }
 
     output$downloadPlot <- reactive({
         pdffile <- createPdfPlot()
         b64 <- base64encode(readBin(pdffile, what=raw(),n=file.info(pdffile)$size))
         unlink(pdffile)                    
         return(paste("data:application/pdf;base64,", b64, sep=''))
     })

})
