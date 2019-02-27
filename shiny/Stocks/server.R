library(shiny)
library(shinyjs)
library(forecast)
library(tseries)
library(lubridate)
library(zoo)
library(lattice)

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
  
  print(paste("nasdaqTraded: ", str(nasdaqTraded)))
  
  return(nasdaqTraded)
}

# Read stocks from Yahoo
readStocks <- function(stocksList, startDate, quote = "Close", isMonthly=TRUE, session) {
  print("Entering readStocks")
  z <- list()
  compression <- if (isMonthly == TRUE) "m" else "d"
  len <- nrow(stocksList)
  for (i in 1:len) {
    name <- toString(stocksList[i, "Symbol"][1])
    
    shinyWidgets::updateProgressBar(session = session, id = "pb", 
                                    value = 100.0*i/len,
                                    title = paste("Loading",name))
    
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
  tryCatch({

    tryCatch({
      x <- ts(stockZoo, frequency=freq)
    }, error=function(cond) {
      stop(": not enough data")
    })

    # print dates
    dmin <- start(stockZoo)
    dmax <- end(stockZoo)
    print(paste(stockSymbol, ": dmin=", dmin, "dmax=", dmax))
    
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
    text(x = 0.5, y = 0.5, paste("Can't forecast",stockSymbol, ":", cond$message), cex = 1.6, col = "red")    
    print (cond)
    str(cond)
  },
  warning=function(cond) {
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, paste("Can't forecast",stockSymbol, "Stock:", cond$message), cex = 1.6, col = "red")    
    print (cond)
  })
}

#startDateDefault <- as.POSIXct("2016-01-01")
startDateDefault <- "2016-01-01"


# Server session logic
shinyServer(function(input, output, session) {

  shinyjs::disable("dateFrom")
  
  # sesion data
  isMonthly <- reactiveVal(TRUE)
  startDate <- reactiveVal(NA)
  quote <- reactiveVal("Close")
  nasdaqTraded <- reactiveVal(NULL)
  nStocks <- reactiveVal(3)
  freq <- reactiveVal(12)
  toPredict <- reactiveVal(12)
  
  # init ui
  init <- function(back, compression) {
    isMonthly(compression=="m")
    if (isMonthly()) {
      startDate(floor_date(Sys.Date() - years(-back), "years"))
      updateSliderInput(session, "back", min = -10, max = -2, value = back, label = "Years Back")  
    } else {
      startDate(floor_date(Sys.Date() - months(-back), "months"))
      updateSliderInput(session, "back", min = -12, max = -2, value = back, label = "Month Back")  
    }
    updateDateInput(session, "dateFrom", value=startDate())
    output$distPlot <- NULL
  }  
  
  observe({
    init(input$back,input$compression)
  })
  
  observe({
    nStocks(input$stocksNum)
    freq(input$freq)
    toPredict(input$predict)
    output$distPlot <- NULL
    print("here")
  })

  # On "Run Forecast"
  observeEvent(input$go, {
    
    # launch the modal
    shinyWidgets::updateProgressBar(session = session, id = "pb", value = 0)
    session$sendCustomMessage(type = 'launch-modal', "my-modal") 
    
    # load Nasdaq symbols (once per user session)
    nt <- nasdaqTraded()
    if (is.null(nt)) {
      shinyWidgets::updateProgressBar(session = session, id = "pb", value = 5, title = "Loading Nasdaq Symbols")
      nt = readNasdaqListings()
      nasdaqTraded(nt)
    }
    
    # Load stock series data
    stocks <- nt[sample(1:nrow(nt), nStocks()),]
    z <- readStocks(stocks, input$dateFrom, quote(), isMonthly(), session)
    print(paste("Stocks loaded:", length(z), "out of", nStocks()))

    # Forecast and plot stock series
    output$distPlot <- renderPlot({
      par(mfrow = c(nStocks(), 1))
      for (n in 1:length(z)) {
        plotForecast(z[[n]], names(z)[n], freq(), toPredict(), isMonthly())
      }
    })
    
    # close modal
    session$sendCustomMessage(type = 'remove-modal', "my-modal")
  })  
  
})
