setwd("D:/shiny")
library(shiny)

library(PerformanceAnalytics)

library(zoo)
library(tseries)
library(quantmod)
library(Deducer)


# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {


  stock1ticker <- "something"
  stock2ticker <- "something"
  stock3ticker <- "something"
  stock4ticker <- "something"
  stock5ticker <- "something"

  stock1.prices <- "something"
  stock2.prices <- "something"
  stock3.prices <- "something"
  stock4.prices <- "something"
  stock5.prices <- "something"

  R.stock1 <- "something"
  R.stock2 <- "something"
  R.stock3 <- "something"
  R.stock4 <- "something"
  R.stock5 <- "something"


  formulaText <- reactive({
    paste("Stock ", input$stock1)
  })


  # Return the formula text for printing as a caption
  output$caption1 <- renderText({
    formulaText()
  })

  output$caption2 <- renderText({
    paste("Stock ", input$stock2)
  })

  output$caption3 <- renderText({
    paste("Stock ", input$stock3)
  })

  output$caption4 <- renderText({
    paste("Stock ", input$stock4)
  })

  output$caption5 <- renderText({
    paste("Stock ", input$stock5)
  })


  output$allstocksplot <- renderPlot({

    stockCompress <- switch(input$interval,
           "Daily" = "d",
           "Weekly" = "w",
           "Monthly" = "m"
     )

    stock1ticker <<- switch(input$stock1,
           "HDFC" = "HDFCBANK.NS",
           "TCS" = "TCS.NS",
           "RIL" = "RIL.BO",
           "ITC" = "ITC.NS",
           "Tata Motors" = "TATAMOTOR.NS",
           "Dr Reddy" = "DRREDDY.NS",
           "Coal India" = "COALINDIA.NS",
           "L&T" = "LT.NS",
           "UltraTech" = "ULTRACEMC.NS",
           "Airtel" = "BHARTIART.NS",
           "Tata Steel" = "TATASTEEL.NS"
     )

    stock2ticker <<- switch(input$stock2,
           "HDFC" = "HDFCBANK.NS",
           "TCS" = "TCS.NS",
           "RIL" = "RIL.BO",
           "ITC" = "ITC.NS",
           "Tata Motors" = "TATAMOTOR.NS",
           "Dr Reddy" = "DRREDDY.NS",
           "Coal India" = "COALINDIA.NS",
           "L&T" = "LT.NS",
           "UltraTech" = "ULTRACEMC.NS",
           "Airtel" = "BHARTIART.NS",
           "Tata Steel" = "TATASTEEL.NS"
     )

    stock3ticker <<- switch(input$stock3,
           "HDFC" = "HDFCBANK.NS",
           "TCS" = "TCS.NS",
           "RIL" = "RIL.BO",
           "ITC" = "ITC.NS",
           "Tata Motors" = "TATAMOTOR.NS",
           "Dr Reddy" = "DRREDDY.NS",
           "Coal India" = "COALINDIA.NS",
           "L&T" = "LT.NS",
           "UltraTech" = "ULTRACEMC.NS",
           "Airtel" = "BHARTIART.NS",
           "Tata Steel" = "TATASTEEL.NS"
     )


    stock4ticker <<- switch(input$stock4,
           "HDFC" = "HDFCBANK.NS",
           "TCS" = "TCS.NS",
           "RIL" = "RIL.BO",
           "ITC" = "ITC.NS",
           "Tata Motors" = "TATAMOTOR.NS",
           "Dr Reddy" = "DRREDDY.NS",
           "Coal India" = "COALINDIA.NS",
           "L&T" = "LT.NS",
           "UltraTech" = "ULTRACEMC.NS",
           "Airtel" = "BHARTIART.NS",
           "Tata Steel" = "TATASTEEL.NS"
     )


    stock5ticker <<- switch(input$stock5,
           "HDFC" = "HDFCBANK.NS",
           "TCS" = "TCS.NS",
           "RIL" = "RIL.BO",
           "ITC" = "ITC.NS",
           "Tata Motors" = "TATAMOTOR.NS",
           "Dr Reddy" = "DRREDDY.NS",
           "Coal India" = "COALINDIA.NS",
           "L&T" = "LT.NS",
           "UltraTech" = "ULTRACEMC.NS",
           "Airtel" = "BHARTIART.NS",
           "Tata Steel" = "TATASTEEL.NS"
     )

      stock1.prices <<- get.hist.quote(instrument=stock1ticker, start=as.character(input$dateRange[1]),
                                     end=as.character(input$dateRange[2]), quote=c("Close"),
                                     provider="yahoo", origin="2000-10-31",
                                     compression=stockCompress, retclass="zoo")
      
      stock2.prices <<- get.hist.quote(instrument=stock2ticker, start=as.character(input$dateRange[1]),
                                     end=as.character(input$dateRange[2]), quote=c("Close"),
                                     provider="yahoo", origin="2010-01-01",
                                     compression=stockCompress, retclass="zoo")
      
      stock3.prices <<- get.hist.quote(instrument=stock3ticker, start=as.character(input$dateRange[1]),
                                     end=as.character(input$dateRange[2]), quote=c("Close"),
                                     provider="yahoo", origin="2010-01-01",
                                     compression=stockCompress, retclass="zoo")
      
      stock4.prices <<- get.hist.quote(instrument=stock4ticker, start=as.character(input$dateRange[1]),
                                     end=as.character(input$dateRange[2]), quote=c("Close"),
                                     provider="yahoo", origin="2010-01-01",
                                     compression=stockCompress, retclass="zoo")
      
      stock5.prices <<- get.hist.quote(instrument=stock5ticker, start=as.character(input$dateRange[1]),
                                     end=as.character(input$dateRange[2]), quote=c("Close"),
                                     provider="yahoo", origin="2010-01-01",
                                     compression=stockCompress, retclass="zoo")
      
      
      R.stock1 <<- CalculateReturns(xts(stock1.prices), method="log")
      
      R.stock2 <<- CalculateReturns(xts(stock2.prices), method="log")
      
      R.stock3 <<- CalculateReturns(xts(stock3.prices), method="log")
      
      R.stock4 <<- CalculateReturns(xts(stock4.prices), method="log")
      
      R.stock5 <<- CalculateReturns(xts(stock5.prices), method="log")

      # create merged price data
      all.prices = merge(stock1.prices, stock2.prices, stock3.prices, stock4.prices, stock5.prices)
      all.returns = merge(R.stock1, R.stock2, R.stock3, R.stock4, R.stock5)

      colnames(all.prices) = c(input$stock1, input$stock2, input$stock3, input$stock4, input$stock5)
      colnames(all.returns) = c(input$stock1, input$stock2, input$stock3, input$stock4, input$stock5)

      ret.mat = coredata(all.returns)

      ret.mat <- ret.mat[complete.cases(ret.mat * 0), , drop=FALSE]

      my.panel <- function(...) {
        lines(...)
        abline(h=0)
      }
      
      plot(all.prices,col="blue", lwd=2, main="Returns on assets",
           panel=my.panel)
      
      
      # all on the same graph 
      plot(all.prices, plot.type="single", col=c("black","blue","red","green","brown"), lwd=2,
           main="Returns on Assets", 
           ylab="Return")
      legend(x="topleft", legend=colnames(all.prices), col=c("black","blue","red","green","brown"), lwd=4, cex=0.50)    
      abline(h=0)

  })

  output$allstocksboxplot <- renderPlot({

      tmpvar <- input$stock1
      tmpvar <- input$stock2
      tmpvar <- input$stock3
      tmpvar <- input$stock4
      tmpvar <- input$stock5
      tmpvar <- input$interval

      # create merged price data
      all.prices = merge(stock1.prices, stock2.prices, stock3.prices, stock4.prices, stock5.prices)
      all.returns = merge(R.stock1, R.stock2, R.stock3, R.stock4, R.stock5)

      colnames(all.prices) = c(input$stock1, input$stock2, input$stock3, input$stock4, input$stock5)
      colnames(all.returns) = c(input$stock1, input$stock2, input$stock3, input$stock4, input$stock5)

      ret.mat = coredata(all.returns)

      ret.mat <- ret.mat[complete.cases(ret.mat * 0), , drop=FALSE]

      chart.Boxplot(all.prices)

  })

  output$allstocksCorMat <- renderDataTable({

      tmpvar <- input$stock1
      tmpvar <- input$stock2
      tmpvar <- input$stock3
      tmpvar <- input$stock4
      tmpvar <- input$stock5
      tmpvar <- input$interval

  freqTable <- frequencies(round(R.stock1,2),r.digits=1)

      # create merged price data
      all.prices = merge(stock1.prices, stock2.prices, stock3.prices, stock4.prices, stock5.prices)
      all.returns = merge(R.stock1, R.stock2, R.stock3, R.stock4, R.stock5)

      colnames(all.prices) = c(input$stock1, input$stock2, input$stock3, input$stock4, input$stock5)
      colnames(all.returns) = c(input$stock1, input$stock2, input$stock3, input$stock4, input$stock5)

      ret.mat = coredata(all.returns)

      ret.mat <- ret.mat[complete.cases(ret.mat * 0), , drop=FALSE]

      round(cor(ret.mat), 3)

  }, options = list(bFilter=0, bSort=0, bProcessing=0, bPaginate=0, bInfo=0)) 


  output$stock1plot <- renderPlot({
   
      tmpvar <- input$stock1
      tmpvar <- input$stock2
      tmpvar <- input$stock3
      tmpvar <- input$stock4
      tmpvar <- input$stock5
      tmpvar <- input$interval

      chart.CumReturns(R.stock1,legend.loc="topleft", main="Cumulative Returns Rs 1 Invested")

  })

  output$stock1ProbMat <- renderDataTable({

      tmpvar <- input$stock1
      tmpvar <- input$stock2
      tmpvar <- input$stock3
      tmpvar <- input$stock4
      tmpvar <- input$stock5
      tmpvar <- input$interval

  freqTable <- frequencies(round(R.stock1,2),r.digits=1)

  dfx <- c()

  dfx$Return <- freqTable$Close$Frequencies$Value
  dfx$Frequency <- freqTable$Close$Frequencies$`# of Cases`
  dfx$Prob <- freqTable$Close$Frequencies$`      %`

  dfxx <- data.frame(dfx)

  dfxx$Return <- as.numeric(paste(dfxx$Return))

  dfxx$ExpReti <- dfxx$Return * dfxx$Prob

  dfxx$Riski <- dfxx$Prob * ((dfxx$Return - dfxx$ExpReti) ^ 2)
  
  dfxx  

  }, options = list(bFilter=0, bSort=0, bProcessing=0, bPaginate=0, bInfo=0))


  output$stock2plot <- renderPlot({

      tmpvar <- input$stock1
      tmpvar <- input$stock2
      tmpvar <- input$stock3
      tmpvar <- input$stock4
      tmpvar <- input$stock5
      tmpvar <- input$interval

     chart.CumReturns(R.stock2,legend.loc="topleft", main="Cumulative Returns Rs 1 Invested")

  })

  output$stock2ProbMat <- renderDataTable({

      tmpvar <- input$stock1
      tmpvar <- input$stock2
      tmpvar <- input$stock3
      tmpvar <- input$stock4
      tmpvar <- input$stock5
      tmpvar <- input$interval

  freqTable <- frequencies(round(R.stock2,2),r.digits=1)

  dfx <- c()

  dfx$Return <- freqTable$Close$Frequencies$Value
  dfx$Frequency <- freqTable$Close$Frequencies$`# of Cases`
  dfx$Prob <- freqTable$Close$Frequencies$`      %`

  dfxx <- data.frame(dfx)

  dfxx$Return <- as.numeric(paste(dfxx$Return))

  dfxx$ExpReti <- dfxx$Return * dfxx$Prob

  dfxx$Riski <- dfxx$Prob * ((dfxx$Return - dfxx$ExpReti) ^ 2)
  
  dfxx  

  }, options = list(bFilter=0, bSort=0, bProcessing=0, bPaginate=0, bInfo=0))


  output$stock3plot <- renderPlot({

      tmpvar <- input$stock1
      tmpvar <- input$stock2
      tmpvar <- input$stock3
      tmpvar <- input$stock4
      tmpvar <- input$stock5
      tmpvar <- input$interval

     chart.CumReturns(R.stock3,legend.loc="topleft", main="Cumulative Returns Rs 1 Invested")

  })

  output$stock3ProbMat <- renderDataTable({

      tmpvar <- input$stock1
      tmpvar <- input$stock2
      tmpvar <- input$stock3
      tmpvar <- input$stock4
      tmpvar <- input$stock5
      tmpvar <- input$interval

  freqTable <- frequencies(round(R.stock3,2),r.digits=1)

  dfx <- c()

  dfx$Return <- freqTable$Close$Frequencies$Value
  dfx$Frequency <- freqTable$Close$Frequencies$`# of Cases`
  dfx$Prob <- freqTable$Close$Frequencies$`      %`

  dfxx <- data.frame(dfx)

  dfxx$Return <- as.numeric(paste(dfxx$Return))

  dfxx$ExpReti <- dfxx$Return * dfxx$Prob

  dfxx$Riski <- dfxx$Prob * ((dfxx$Return - dfxx$ExpReti) ^ 2)
  
  dfxx  

  }, options = list(bFilter=0, bSort=0, bProcessing=0, bPaginate=0, bInfo=0))

  output$stock4plot <- renderPlot({

      tmpvar <- input$stock1
      tmpvar <- input$stock2
      tmpvar <- input$stock3
      tmpvar <- input$stock4
      tmpvar <- input$stock5
      tmpvar <- input$interval

    chart.CumReturns(R.stock4,legend.loc="topleft", main="Cumulative Returns Rs 1 Invested")

  })

  output$stock4ProbMat <- renderDataTable({

      tmpvar <- input$stock1
      tmpvar <- input$stock2
      tmpvar <- input$stock3
      tmpvar <- input$stock4
      tmpvar <- input$stock5
      tmpvar <- input$interval

  freqTable <- frequencies(round(R.stock4,2),r.digits=1)

  dfx <- c()

  dfx$Return <- freqTable$Close$Frequencies$Value
  dfx$Frequency <- freqTable$Close$Frequencies$`# of Cases`
  dfx$Prob <- freqTable$Close$Frequencies$`      %`

  dfxx <- data.frame(dfx)

  dfxx$Return <- as.numeric(paste(dfxx$Return))

  dfxx$ExpReti <- dfxx$Return * dfxx$Prob

  dfxx$Riski <- dfxx$Prob * ((dfxx$Return - dfxx$ExpReti) ^ 2)
  
  dfxx  

  }, options = list(bFilter=0, bSort=0, bProcessing=0, bPaginate=0, bInfo=0))

  output$stock5plot <- renderPlot({

      tmpvar <- input$stock1
      tmpvar <- input$stock2
      tmpvar <- input$stock3
      tmpvar <- input$stock4
      tmpvar <- input$stock5
      tmpvar <- input$interval

     chart.CumReturns(R.stock5,legend.loc="topleft", main="Cumulative Returns Rs 1 Invested")

  })



  output$stock5ProbMat <- renderDataTable({

      tmpvar <- input$stock1
      tmpvar <- input$stock2
      tmpvar <- input$stock3
      tmpvar <- input$stock4
      tmpvar <- input$stock5
      tmpvar <- input$interval


  freqTable <- frequencies(round(R.stock5,2),r.digits=1)

  dfx <- c()

  dfx$Return <- freqTable$Close$Frequencies$Value
  dfx$Frequency <- freqTable$Close$Frequencies$`# of Cases`
  dfx$Prob <- freqTable$Close$Frequencies$`      %`

  dfxx <- data.frame(dfx)

  dfxx$Return <- as.numeric(paste(dfxx$Return))

  dfxx$ExpReti <- dfxx$Return * dfxx$Prob

  dfxx$Riski <- dfxx$Prob * ((dfxx$Return - dfxx$ExpReti) ^ 2)
  
  dfxx  

  }, options = list(bFilter=0, bSort=0, bProcessing=0, bPaginate=0, bInfo=0))

  output$riskreturnplot <- renderPlot({

      tmpvar <- input$stock1
      tmpvar <- input$stock2
      tmpvar <- input$stock3
      tmpvar <- input$stock4
      tmpvar <- input$stock5
      tmpvar <- input$interval      
      
      freqTable1 <- frequencies(round(R.stock1,2),r.digits=1)
      
      freqTable2 <- frequencies(round(R.stock2,2),r.digits=1)
      
      freqTable3 <- frequencies(round(R.stock3,2),r.digits=1)
      
      freqTable4 <- frequencies(round(R.stock4,2),r.digits=1)
      
      freqTable5 <- frequencies(round(R.stock5,2),r.digits=1)
      
      dfx <- c()
      
      dfx$Return1 <- freqTable1$Close$Frequencies$Value
      dfx$Frequency1 <- freqTable1$Close$Frequencies$`# of Cases`
      dfx$Prob1 <- freqTable1$Close$Frequencies$`      %`
      
      dfxx1 <- data.frame(dfx)
      
      dfxx1$Return1 <- as.numeric(paste(dfxx1$Return1))
      
      dfxx1$ExpReti1 <- dfxx1$Return1 * dfxx1$Prob1
      
      er.stock1 <- sum(dfxx1$ExpReti1)
      
      dfxx1$Riski1 <- dfxx1$Prob1 * ((dfxx1$Return1 - er.stock1) ^ 2)
      
      risk.stock1 <- sqrt(sum(dfxx1$Riski1))
      
      dfx <- c()
      
      dfx$Return2 <- freqTable2$Close$Frequencies$Value
      dfx$Frequency2 <- freqTable2$Close$Frequencies$`# of Cases`
      dfx$Prob2 <- freqTable2$Close$Frequencies$`      %`
      
      dfxx2 <- data.frame(dfx)
      
      dfxx2$Return2 <- as.numeric(paste(dfxx2$Return2))
      
      dfxx2$ExpReti2 <- dfxx2$Return2 * dfxx2$Prob2
      
      er.stock2 <- sum(dfxx2$ExpReti2)
      
      dfxx2$Riski2 <- dfxx2$Prob2 * ((dfxx2$Return2 - er.stock2) ^ 2)
      
      risk.stock2 <- sqrt(sum(dfxx2$Riski2))
      
      dfx <- c()
      
      dfx$Return3 <- freqTable3$Close$Frequencies$Value
      dfx$Frequency3 <- freqTable3$Close$Frequencies$`# of Cases`
      dfx$Prob3 <- freqTable3$Close$Frequencies$`      %`
      
      dfxx3 <- data.frame(dfx)
      
      dfxx3$Return3 <- as.numeric(paste(dfxx3$Return3))
      
      dfxx3$ExpReti3 <- dfxx3$Return3 * dfxx3$Prob3
      
      er.stock3 <- sum(dfxx3$ExpReti3)
      
      dfxx3$Riski3 <- dfxx3$Prob3 * ((dfxx3$Return3 - er.stock3) ^ 2)
      
      risk.stock3 <- sqrt(sum(dfxx3$Riski3))
      
      
      dfx <- c()
      
      dfx$Return4 <- freqTable4$Close$Frequencies$Value
      dfx$Frequency4 <- freqTable4$Close$Frequencies$`# of Cases`
      dfx$Prob4 <- freqTable4$Close$Frequencies$`      %`
      
      dfxx4 <- data.frame(dfx)
      
      dfxx4$Return4 <- as.numeric(paste(dfxx4$Return4))
      
      dfxx4$ExpReti4 <- dfxx4$Return4 * dfxx4$Prob4
      
      er.stock4 <- sum(dfxx4$ExpReti4)
      
      dfxx4$Riski4 <- dfxx4$Prob4 * ((dfxx4$Return4 - er.stock4) ^ 2)
      
      risk.stock4 <- sqrt(sum(dfxx4$Riski4))
      
      dfx <- c()
      
      dfx$Return5 <- freqTable5$Close$Frequencies$Value
      dfx$Frequency5 <- freqTable5$Close$Frequencies$`# of Cases`
      dfx$Prob5 <- freqTable5$Close$Frequencies$`      %`
      
      dfxx5 <- data.frame(dfx)
      
      dfxx5$Return5 <- as.numeric(paste(dfxx5$Return5))
      
      dfxx5$ExpReti5 <- dfxx5$Return5 * dfxx5$Prob5
      
      er.stock5 <- sum(dfxx5$ExpReti5)
      
      dfxx5$Riski5 <- dfxx5$Prob5 * ((dfxx5$Return5 - er.stock5) ^ 2)
      
      risk.stock5 <- sqrt(sum(dfxx5$Riski5))
      
      ReturnRisk <- data.frame(Return=numeric(), 
                               Risk=numeric())
      
      for (i in 1:100 ) {
        
        rndweight <- floor(prop.table(runif(5)) * 1000)
        
        weight1 <- rndweight[1]/1000
        weight2 <- rndweight[2]/1000
        weight3 <- rndweight[3]/1000
        weight4 <- rndweight[4]/1000
        weight5 <- rndweight[5]/1000
        
        
        totreturn <- weight1 * er.stock1 + weight2 * er.stock2 + weight3 * er.stock3 + 
          weight4 * er.stock4 + weight5 * er.stock5
        
        totrisk <- sqrt((weight1 ^ 2) * risk.stock1 + (weight2 ^ 2) * risk.stock2 + 
          2 * weight1 * weight2 * CoVariance(R.stock1,R.stock2)) +
          sqrt((weight1 ^ 2) * risk.stock1 + (weight3 ^ 2) * risk.stock3 + 
          2 * weight1 * weight3 * CoVariance(R.stock1,R.stock3)) +
          sqrt((weight1 ^ 2) * risk.stock1 + (weight4 ^ 2) * risk.stock4 + 
          2 * weight1 * weight4 * CoVariance(R.stock1,R.stock4)) +
          sqrt((weight1 ^ 2) * risk.stock1 + (weight5 ^ 2) * risk.stock5 + 
          2 * weight1 * weight5 * CoVariance(R.stock1,R.stock5)) +
          sqrt((weight2 ^ 2) * risk.stock2 + (weight3 ^ 2) * risk.stock3 + 
          2 * weight2 * weight3 * CoVariance(R.stock2,R.stock3)) +
          sqrt((weight2 ^ 2) * risk.stock2 + (weight4 ^ 2) * risk.stock4 + 
          2 * weight2 * weight4 * CoVariance(R.stock2,R.stock4)) +
          sqrt((weight2 ^ 2) * risk.stock2 + (weight5 ^ 2) * risk.stock5 + 
          2 * weight2 * weight5 * CoVariance(R.stock2,R.stock5)) +
          sqrt((weight3 ^ 2) * risk.stock3 + (weight4 ^ 2) * risk.stock4 + 
          2 * weight3 * weight4 * CoVariance(R.stock3,R.stock4)) +
          sqrt((weight3 ^ 2) * risk.stock3 + (weight5 ^ 2) * risk.stock5 + 
          2 * weight3 * weight5 * CoVariance(R.stock3,R.stock5)) +
          sqrt((weight4 ^ 2) * risk.stock4 + (weight5 ^ 2) * risk.stock5 + 
          2 * weight4 * weight5 * CoVariance(R.stock4,R.stock5))  
        
        newrow = c(totreturn, totrisk)           
        
        ReturnRisk <- rbind(ReturnRisk, newrow)  
        
      }
      
      
      names(ReturnRisk)[1]<-paste("Return")
      names(ReturnRisk)[2]<-paste("Risk")
      
      plot(ReturnRisk)


  })


  output$riskreturnMCplot <- renderPlot({

      tmpvar <- input$stock1
      tmpvar <- input$stock2
      tmpvar <- input$stock3
      tmpvar <- input$stock4
      tmpvar <- input$stock5
      tmpvar <- input$interval
      
      datefield <- index(R.stock1)
      
      newdf <- data.frame(R.stock1,datefield)
      
      meanR.stock1 <- mean(newdf$Close,na.rm = TRUE)
      
      sdR.stock1 <- sd(newdf$Close,na.rm = TRUE)
      
      datefield <- index(R.stock2)
      
      newdf <- data.frame(R.stock2,datefield)
      
      meanR.stock2 <- mean(newdf$Close,na.rm = TRUE)
      
      sdR.stock2 <- sd(newdf$Close,na.rm = TRUE)
      
      datefield <- index(R.stock3)
      
      newdf <- data.frame(R.stock3,datefield)
      
      meanR.stock3 <- mean(newdf$Close,na.rm = TRUE)
      
      sdR.stock3 <- sd(newdf$Close,na.rm = TRUE)
      
      datefield <- index(R.stock4)
      
      newdf <- data.frame(R.stock4,datefield)
      
      meanR.stock4 <- mean(newdf$Close,na.rm = TRUE)
      
      sdR.stock4 <- sd(newdf$Close,na.rm = TRUE)
      
      datefield <- index(R.stock5)
      
      newdf <- data.frame(R.stock5,datefield)
      
      meanR.stock5 <- mean(newdf$Close,na.rm = TRUE)
      
      sdR.stock5 <- sd(newdf$Close,na.rm = TRUE)
      
      allStockReturns <- data.frame(stock1 = rnorm(5000, meanR.stock1, sdR.stock1),
                                    stock2 = rnorm(5000, meanR.stock2, sdR.stock2),
                                    stock3 = rnorm(5000, meanR.stock3, sdR.stock3),
                                    stock4 = rnorm(5000, meanR.stock4, sdR.stock4),
                                    stock5 = rnorm(5000, meanR.stock5, sdR.stock5))
      
      ReturnRiskSim <- data.frame(Return=numeric(), 
                                  Risk=numeric())
      
      sims <- c(10,20,40,80,100) 
      
      for (i in sims) {
        
        #  allStockReturns <- data.frame(stock1 = rnorm(5000, meanR.stock1, sdR.stock1),
        #                                stock2 = rnorm(5000, meanR.stock2, sdR.stock2),
        #                                stock3 = rnorm(5000, meanR.stock3, sdR.stock3),
        #                                stock4 = rnorm(5000, meanR.stock4, sdR.stock4),
        #                                stock5 = rnorm(5000, meanR.stock5, sdR.stock5))  
        
        ReturnRisk <- data.frame(Return=numeric(), 
                                 Risk=numeric())
        
        
        for (j in 1:i) {
          
          rndweight <- floor(prop.table(runif(5)) * 1000)
          
          weight1 <- rndweight[1]/1000
          weight2 <- rndweight[2]/1000
          weight3 <- rndweight[3]/1000
          weight4 <- rndweight[4]/1000
          weight5 <- rndweight[5]/1000
          
          
          totreturn <- weight1 * meanR.stock1 + weight2 * meanR.stock2 + weight3 * meanR.stock3 + 
            weight4 * meanR.stock4 + weight5 * meanR.stock5
          
          totrisk <- sqrt((weight1 ^ 2) * sdR.stock1 + (weight2 ^ 2) * sdR.stock2 + 
                            2 * weight1 * weight2 * CoVariance(R.stock1,R.stock2)) +
            sqrt((weight1 ^ 2) * sdR.stock1 + (weight3 ^ 2) * sdR.stock3 + 
                   2 * weight1 * weight3 * CoVariance(R.stock1,R.stock3)) +
            sqrt((weight1 ^ 2) * sdR.stock1 + (weight4 ^ 2) * sdR.stock4 + 
                   2 * weight1 * weight4 * CoVariance(R.stock1,R.stock4)) +
            sqrt((weight1 ^ 2) * sdR.stock1 + (weight5 ^ 2) * sdR.stock5 + 
                   2 * weight1 * weight5 * CoVariance(R.stock1,R.stock5)) +
            sqrt((weight2 ^ 2) * sdR.stock2 + (weight3 ^ 2) * sdR.stock3 + 
                   2 * weight2 * weight3 * CoVariance(R.stock2,R.stock3)) +
            sqrt((weight2 ^ 2) * sdR.stock2 + (weight4 ^ 2) * sdR.stock4 + 
                   2 * weight2 * weight4 * CoVariance(R.stock2,R.stock4)) +
            sqrt((weight2 ^ 2) * sdR.stock2 + (weight5 ^ 2) * sdR.stock5 + 
                   2 * weight2 * weight5 * CoVariance(R.stock2,R.stock5)) +
            sqrt((weight3 ^ 2) * sdR.stock3 + (weight4 ^ 2) * sdR.stock4 + 
                   2 * weight3 * weight4 * CoVariance(R.stock3,R.stock4)) +
            sqrt((weight3 ^ 2) * sdR.stock3 + (weight5 ^ 2) * sdR.stock5 + 
                   2 * weight3 * weight5 * CoVariance(R.stock3,R.stock5)) +
            sqrt((weight4 ^ 2) * sdR.stock4 + (weight5 ^ 2) * sdR.stock5 + 
                   2 * weight4 * weight5 * CoVariance(R.stock4,R.stock5))  
          
          newrow = c(totreturn, totrisk)           
          
          ReturnRiskSim <- rbind(ReturnRiskSim, newrow)                    
          
          names(ReturnRiskSim)[1]<-paste("Return")
          names(ReturnRiskSim)[2]<-paste("Risk")      
          
        } 
        
        #  newrow = c(mean(ReturnRisk$Return), mean(ReturnRisk$Risk))           
        
        #  ReturnRiskSim <- rbind(ReturnRiskSim, newrow)                    
        
      }  
      
      
      names(ReturnRiskSim)[1]<-paste("Return")
      names(ReturnRiskSim)[2]<-paste("Risk")
      
      plot(ReturnRiskSim)


  })

  output$varEachStock <- renderDataTable({

      tmpvar <- input$stock1
      tmpvar <- input$stock2
      tmpvar <- input$stock3
      tmpvar <- input$stock4
      tmpvar <- input$stock5
      tmpvar <- input$interval
      
      all.prices = merge(stock1.prices, stock2.prices, stock3.prices, stock4.prices, stock5.prices)
      all.returns = merge(R.stock1, R.stock2, R.stock3, R.stock4, R.stock5)

      colnames(all.prices) = c(input$stock1, input$stock2, input$stock3, input$stock4, input$stock5)
      colnames(all.returns) = c(input$stock1, input$stock2, input$stock3, input$stock4, input$stock5)

      ret.mat = coredata(all.returns)
      ret.mat <- ret.mat[complete.cases(ret.mat * 0), , drop=FALSE]

      Value.at.Risk = function(x,p=0.05,w=100000) {
        x = as.matrix(x)
        q = apply(x, 2, mean) + apply(x, 2, sd)*qnorm(p)
        VaR = (exp(q) - 1)*w
        VaR
      }
      
      dfx <- merge(as.data.frame(Value.at.Risk(ret.mat,p=0.05,w=20000)), as.data.frame(Value.at.Risk(ret.mat,p=0.01,w=20000)), by = "row.names")

      names(dfx)[1]<-paste("Stock")

      dfx

  })


  output$varPortfolio <- renderDataTable({

      tmpvar <- input$stock1
      tmpvar <- input$stock2
      tmpvar <- input$stock3
      tmpvar <- input$stock4
      tmpvar <- input$stock5
      tmpvar <- input$interval
      
      all.prices = merge(stock1.prices, stock2.prices, stock3.prices, stock4.prices, stock5.prices)
      all.returns = merge(R.stock1, R.stock2, R.stock3, R.stock4, R.stock5)

      colnames(all.prices) = c(input$stock1, input$stock2, input$stock3, input$stock4, input$stock5)
      colnames(all.returns) = c(input$stock1, input$stock2, input$stock3, input$stock4, input$stock5)

      ret.mat = coredata(all.returns)
      ret.mat <- ret.mat[complete.cases(ret.mat * 0), , drop=FALSE]

      weights<-c(20000,20000,20000,20000,20000)
      
      mu<-apply(ret.mat,2,mean)
      
      sigma<-cov(ret.mat)
      
      MC<-mvrnorm(10000,mu,sigma)
      
      MCportfolio<-MC%*%as.matrix(weights)
      
      dfx <- merge(quantile(MCportfolio,p=0.05), quantile(MCportfolio,p=0.01))
      
      names(dfx)[1]<-paste("95% Confidence")
      names(dfx)[2]<-paste("99% Confidence")      

      dfx

  })

}) 
