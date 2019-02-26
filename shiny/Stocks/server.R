library(shiny)
library(shinyjs)
library(forecast)
library(tseries)


# Reading list of Nasdaq - listed stock names ans clean it
readNasdaqListings <- function() {
  nasdaqTraded <- read.csv(
    file = "ftp://ftp.nasdaqtrader.com/SymbolDirectory/nasdaqtraded.txt",
    sep = "|"  )
  
  # last line in the file is the date of creation as " File Creation Time: mmddyyyyhhmm"
  creationDate <- as.list(levels(nasdaqTraded[-1,1]))[[1]]
  print(creationDate)
  nasdaqTraded <- nasdaqTraded[1:nrow(nasdaqTraded)-1,]
  
  # remove unnecessary rows and columns
  nasdaqTraded <- nasdaqTraded[
    (nasdaqTraded$Nasdaq.Traded=="Y" & nasdaqTraded$Financial.Status=="N"),
    c(2,3)]
  nasdaqTraded$Symbol <- factor(nasdaqTraded$Symbol)
  nasdaqTraded$Security.Name <- factor(nasdaqTraded$Security.Name)
  
  return(nasdaqTraded)
}

# Read stocks from Yahoo
readStocks <- function(stocksList, startDate, quote = "Close", isMonthly=TRUE) {
  print("Entering readStocks")
  z <- list()
  compression <- if (isMonthly == TRUE) "m" else "d"
  len <- nrow(stocksList)
  for (i in 1:len) {
    name <- toString(stocksList[i, "Symbol"][1])
    setProgress(1.0*i/len, message = paste("Loading",name))
    tryCatch({
      z[[name]] <- get.hist.quote(
        instrument = name, 
        quote = quote,
        compression = compression,
        start = startDate)
    },
    error=function(cond) {
      message(cond)      
    },
    warning=function(cond) {
      message(cond)      
    })
  }
  print("Exiting readStocks")
  return (z)
}

plotForecast <- function(stockZoo, stockSymbol, freq, toForecast, isMonthly=TRUE) {
  x <- ts(stockZoo, frequency=freq)
  dmin <- start(z[[n]])
  dmax <- end(z[[n]])
  
  tryCatch({
    models <- list(
      mod_stl = stlm(x, s.window=freq, ic='aicc', robust=TRUE, method='ets')
    )
    
    forecasts <- lapply(models, forecast, toForecast)
    len <- ceiling(1 + length(x)/freq) 
    format <- if (isMonthly) "%b-%Y" else "%Y-%m-%d"
    by <- if (isMonthly) paste(freq, "months") else paste(freq, "days")
    to <- if (isMonthly) as.Date(dmax)+toForecast*30 else as.Date(dmax)+toForecast
    las <- if (isMonthly) 1 else 2
    for (f in forecasts) {
      ticks<-format(seq(as.Date(dmin),to=to, length.out=len), format=format)
      print (ticks)
      plot(f, xlab="", xaxt="n", main = paste(stockSymbol, "Stock Forecast"))
      axis(1, at=1:len, labels=ticks, par(las=las))
    }
  },
  error=function(cond) {
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, paste("Can't forecast",stockSymbol, "Stock"), cex = 1.6, col = "red")    
    print (cond)
    str(cond)
  },
  warning=function(cond) {
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, paste("Can't forecast",stockSymbol, "Stock"), cex = 1.6, col = "red")    
    print (cond)
  })
}

# Server session logic
shinyServer(function(input, output, session) {

  #set.seed(99)

  isMonthly <- TRUE
  quote <- "Close"
  nStocks <- 3
  freq <- 12
  toPredict <- 12
  startDate <- as.POSIXct("2016-01-01")
  
  withProgress(message = "Loading stocks symbols", {
    nasdaqTraded <- readNasdaqListings()
    stocks <- nasdaqTraded[sample(1:nrow(nasdaqTraded), nStocks),]
    z <- readStocks(stocks, startDate, quote, isMonthly)
  })
  
  print(paste("Stocks loaded:", length(z), "out of", nStocks))
  
  output$distPlot <- renderPlot({
    par(mfrow = c(nStocks, 1))
    for (n in 1:length(z)) {
      plotForecast(z[[n]], names(z)[n], freq, toPredict, isMonthly)
    }
  })
  
})
