library(shiny)
library(shinythemes)
library(forecast)
library(ggplot2)

# Define UI for application
ui = fluidPage(
    theme = shinytheme("superhero"),

    # Application title
    titlePanel(h1(align = "center", "Time Series Forecast Benchmark")),
    
    br(),
    br(),

    fluidRow(
        column(3, align = "center",
               fileInput("fileInputID",
                         "Choose file to load time series:",
                         accept = c("text/csv",
                                    "text/comma-separated-values",
                                    "text/plain",
                                    ".csv")),
               helpText("Note: file must have only one column, without header.
                        Time series frequency must be in months.")
        ),
        column(3, align = "center",
            dateRangeInput("dateRangeInputID",
                           "Time series data range:",
                           format = "mm/yyyy",
                           start = "2000/01/01",
                           end = "2013/12/31",
                           startview = "year",
                           separator = " to "),
            helpText("Note: you may select any day when defining month and year.")
        ),
        column(3, align = "center",
               selectInput("modelSelectionID",
                           "What models do you want to see in the plot?",
                           multiple = T,
                           choices = c("Naive Forecasting" = "modelNaive",
                                       "Mean Forecast" = "modelMeanf",
                                       "Random Walk Forecast with Drift" = "modelRwf",
                                       "Holt's Linear" = "modelHolt",
                                       "Holt-Winters Additive" = "modelHw",
                                       "Holt-Winters Multiplicative" = "modelHw2",
                                       "Holt-Winters Multiplicative with Drift" = "modelHw3",
                                       "ARIMA" = "modelArima",
                                       "Linear Model" = "modelTslm",
                                       "Neural Network" = "modelNnetar")),
        ),
        column(3, align = "center",
               numericInput("forecastPeriodID", "How many months do you want to
                            forecast?", value = 24, min = 1, max = 48),
               actionButton("forecastButtonID", "Forecast!")
        )
    ),
    
    hr(),
    br(),
    
    fluidRow(
        column(12, align = "center",
               plotOutput("forecastPlotID", height = "800px"))
    ),
    fluidRow(
        column(6, align = "center", 
               h2(textOutput("textNaiveID")),
               tableOutput("tableNaiveID"),
               
               h2(textOutput("textMeanfID")),
               tableOutput("tableMeanfID"),
               
               h2(textOutput("textRwfID")),
               tableOutput("tableRwfID"),
               
               h2(textOutput("textHoltID")),
               tableOutput("tableHoltID"),
               
               h2(textOutput("textHwID")),
               tableOutput("tableHwID")
        ),
        column(6, align = "center", 
               h2(textOutput("textHw2ID")),
               tableOutput("tableHw2ID"),
               
               h2(textOutput("textHw3ID")),
               tableOutput("tableHw3ID"),
               
               h2(textOutput("textTslmID")),
               tableOutput("tableTslmID"),
               
               h2(textOutput("textArimaID")),
               tableOutput("tableArimaID"),
               
               h2(textOutput("textNnetarID")),
               tableOutput("tableNnetarID")
        )
    )
)

# Define server logic
server = function(input, output) {

    observeEvent(input$forecastButtonID, {
        validate(
            need(input$fileInputID, "A file must be provided.")
        )
        
        data = read.csv(input$fileInputID$datapath, header = F)
        
        startYear = as.integer(substr(input$dateRangeInputID[1], 1, 4))
        startMonth = as.integer(substr(input$dateRangeInputID[1], 6, 7))
        endYear = as.integer(substr(input$dateRangeInputID[2], 1, 4))
        endMonth = as.integer(substr(input$dateRangeInputID[2], 6, 7))
        
        timeSeries = ts(data, start = c(startYear, startMonth),
                        end = c(endYear, endMonth), frequency = 12)
        
        forecastPeriod = input$forecastPeriodID
        
        trainData = window(timeSeries, start = c(startYear, startMonth),
                           end = c(endYear - 2, endMonth))
        testData = window(timeSeries, start = c(endYear - 2, startMonth),
                          end = c(endYear, endMonth))
        
        # naive forecasting
        modelNaive = naive(trainData, h = forecastPeriod)
        output$textNaiveID = renderText({
            "Naive Forecasting"
        })
        output$tableNaiveID = renderTable({
            accuracy(testData, modelNaive$mean)
        })
        
        # mean
        modelMeanf = meanf(trainData, h = forecastPeriod)
        output$textMeanfID = renderText({
            "Mean Forecast"
        })
        output$tableMeanfID = renderTable({
            accuracy(testData, modelMeanf$mean)
        })
        
        # drift
        modelRwf = rwf(trainData, h = forecastPeriod, drift = T)
        output$textRwfID = renderText({
            "Random Walk Forecast with Drift
        "})
        output$tableRwfID = renderTable({
            accuracy(testData, modelRwf$mean)
        })
        
        # Holt
        modelHolt = holt(trainData, h = forecastPeriod)
        output$textHoltID = renderText({
            "Holt's Linear"
        })
        output$tableHoltID = renderTable({
            accuracy(testData, modelHolt$mean)
        })
        
        # Holt-Winters additive
        modelHw = hw(trainData, h = forecastPeriod, seasonal = "additive")
        output$textHwID = renderText({
            "Holt-Winters Additive"
        })
        output$tableHwID = renderTable({
            accuracy(testData, modelHw$mean)
        })
        
        # Holt-Winters multiplicative
        modelHw2 = hw(trainData, h = forecastPeriod, seasonal = "multiplicative")
        output$textHw2ID = renderText({
            "Holt-Winters Multiplicative"
        })
        output$tableHw2ID = renderTable({
            accuracy(testData, modelHw2$mean)
        })
        
        # Holt-Winters multiplicative with drift
        modelHw3 = hw(trainData, h = forecastPeriod, seasonal = "multiplicative",
                      damped = T, phi = 0.9)
        output$textHw3ID = renderText({
            "Holt-Winters Multiplicative with Drift"
        })
        output$tableHw3ID = renderTable({
            accuracy(testData, modelHw3$mean)
        })
        
        # ARIMA
        modelArima = auto.arima(trainData)
        modelArima = forecast(modelArima, h = forecastPeriod)
        output$textArimaID = renderText({
            "ARIMA"
        })
        output$tableArimaID = renderTable({
            accuracy(testData, modelArima$mean)
        })
        
        # linear
        modelTslm = tslm(trainData ~ trend, data = trainData)
        modelTslm = forecast(modelTslm, h = forecastPeriod)
        output$textTslmID = renderText({
            "Linear Model"
        })
        output$tableTslmID = renderTable({
            accuracy(testData, modelTslm$mean)
        })
        
        # neural network
        modelNnetar = nnetar(trainData)
        modelNnetar = forecast(modelNnetar, h = forecastPeriod)
        output$textNnetarID = renderText({
            "Neural Network"
        })
        output$tableNnetarID = renderTable({
            accuracy(testData, modelNnetar$mean)
        })

        output$forecastPlotID = renderPlot({
            par(bg = "gray98")
            plot(timeSeries, main = "Forecast Benchmark")
            
            mList = input$modelSelectionID
            if ("modelNaive" %in% mList)
                lines(modelNaive$mean, col = "red", lty = 6, lwd = 4, pch = 22)
            if ("modelMeanf" %in% mList)
                lines(modelMeanf$mean, col = "blue", lty = 5, lwd = 4, pch = 22)
            if ("modelRwf" %in% mList)
                lines(modelRwf$mean, col = "green", lty = 4, lwd = 4, pch = 22)
            if ("modelHolt" %in% mList)
                lines(modelHolt$mean, col = "cyan", lty = 6, lwd = 4, pch = 22)
            if ("modelHw" %in% mList)
                lines(modelHw$mean, col = "black", lty = 2, lwd = 4, pch = 22)
            if ("modelHw2" %in% mList)
                lines(modelHw2$mean, col = "purple", lty = 1, lwd = 4, pch = 22)
            if ("modelHw3" %in% mList)
                lines(modelHw3$mean, col = "orangered3", lty = 6, lwd = 4, pch = 22)
            if ("modelArima" %in% mList)
                lines(modelArima$mean, col = "gold", lty = 5, lwd = 4, pch = 22)
            if ("modelTslm" %in% mList)
                lines(modelTslm$mean, col = "chocolate1", lty = 4, lwd = 4, pch = 22)
            if ("modelNnetar" %in% mList)
                lines(modelNnetar$mean, col = "magenta", lty = 6, lwd = 4, pch = 22)
            
            legend("topleft", 
                   legend = c("Naive",
                              "Mean",
                              "RWF Drift",
                              "Holt's Linear",
                              "HW Add.",
                              "HW Mult.",
                              "HW Mult. Drift",
                              "ARIMA",
                              "Linear Model",
                              "Neural Network"),
                   col = c("red",
                           "blue",
                           "green",
                           "cyan",
                           "black",
                           "purple",
                           "orangered3",
                           "gold",
                           "chocolate1",
                           "magenta"),
                   cex = 1.4,
                   ncol = 4,
                   lwd = 7
            )
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
