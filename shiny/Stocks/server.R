library(shiny)
library(shinyjs)
library(forecast)
library(tseries)
library(lubridate)
library(zoo)
library(lattice)
library(DT)

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
    format <- if (isMonthly) "%b-%Y" else " %Y-%m-%d"
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

# Server session logic
shinyServer(function(input, output, session) {

  # ui state machine
  shinyjs::disable("dateFrom") # temporary
  shinyjs::hide("stocksNum")   # temporary
  shinyjs::disable("go")

  # sesion data
  isMonthly <- reactiveVal(TRUE)
  startDate <- reactiveVal(NA)
  quote <- reactiveVal("Close")
  nasdaqTraded <- reactiveVal(NULL)
  nStocks <- reactiveVal(3)
  freq <- reactiveVal(12)
  toPredict <- reactiveVal(12)
  selectedStocks <- reactiveVal(NULL)
  
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
  })

  # will run on app start
  observeEvent( input$refresh_helper, {
    
    # load Nasdaq symbols (once per user session) with modal progress
    nt <- nasdaqTraded()
    if (is.null(nt)) {
      session$sendCustomMessage(type = 'launch-modal', "my-modal") 
      shinyWidgets::updateProgressBar(session = session, id = "pb", value = 50,
                                      title = "Loading Nasdaq Symbols")
      nasdaqTraded(readNasdaqListings())
      session$sendCustomMessage(type = 'remove-modal', "my-modal")
    }
    
    output$m1 = renderText("\nPage under construction")
    output$m2 = renderText("\nPage under construction")

    shinyjs::enable("go")
  })
  
  # fill the tab when stocks loaded
  observe({
    output$tbl2 = DT::renderDataTable(
    nasdaqTraded(), 
#    selection = 'single',
    server = FALSE, 
    options = list (
      lengthChange=FALSE, 
      pageLength=10)
    )
  })

  # when stock selection changes
  observe({
    if (is.null(selectedStocks()) | length(selectedStocks()) == 0 ) {
      shinyjs::disable("go")
    } else {
      output$distPlot <- NULL
      shinyjs::enable("go")
    }
  })
  
  observe({
    selectedStocks(nasdaqTraded()[input$tbl2_rows_selected,1])
    output$msg2 = renderText(paste0(selectedStocks(),"; "))
    print(paste("Stocks selected:",selectedStocks()))
  })
  
  # On "Run Forecast"
  observeEvent(input$go, {
    
    # launch the modal
    shinyWidgets::updateProgressBar(session = session, id = "pb", value = 0)
    session$sendCustomMessage(type = 'launch-modal', "my-modal") 
    
    # Load stock series data
    nt <- nasdaqTraded()
    
    #stocks <- nt[sample(1:nrow(nt), nStocks()),]
    stocks <- nt[selectedStocks(),]
    nStocks(length(selectedStocks()))
    
    z <- readStocks(stocks, input$dateFrom, quote(), isMonthly(), session)
    print(paste("Stocks loaded:", length(z), "out of", nStocks()))

    # Forecast and plot stock series
    output$distPlot <- renderPlot({
      par(mfrow = c(nStocks(), 1), mar=c(6,2,2,1))
      for (n in 1:length(z)) {
        plotForecast(z[[n]], names(z)[n], freq(), toPredict(), isMonthly())
      }
    }, height = 500 + 100*(nStocks()-1), res=100)
    
    # close modal
    session$sendCustomMessage(type = 'remove-modal', "my-modal")

    # switch to "Forecast" panel    
    updateTabsetPanel(session, "tabPanels", selected = "forecastPanel")
  })  
})
