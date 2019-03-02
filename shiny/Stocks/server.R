library(shiny)
library(shinyjs)
library(forecast)
library(tseries)
library(lubridate)
library(zoo)
library(lattice)
library(DT)
library(TSclust)
library(imputeTS)

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
readStocksAsSingleZoo <- 
  function(stocksList, startDate, quote = "Close", isMonthly=TRUE, session) 
{
  z <- NULL
  compression <- if (isMonthly == TRUE) "m" else "d"
  len <- nrow(stocksList)
  for (i in 1:len) {
    name <- toString(stocksList[i, "Symbol"][1])

    shinyWidgets::updateProgressBar(session = session, id = "pb", 
                                    value = 80.0*i/len,
                                    title = paste("Loading",name))
    
    tryCatch({
      tmp <- get.hist.quote(
        instrument = name, 
        quote = quote,
        compression = compression,
        start = startDate)
      
      colnames(tmp) <- name
      if (is.null(z)) {
        z <- tmp
        str(z)
      } else  {
        z <- cbind(z,tmp)
      }
    },
    error=function(cond) {
      message(cond)      
    })
  }
  return (z)
}

plotForecast2 <- function(stockZoo, stockSymbol, freq, toForecast, isMonthly=TRUE) {
  x <- ts(stockZoo, frequency=freq)
  tryCatch({
    x <- ts(stockZoo, frequency=freq)
  }, error=function(cond) {
    stop(": not enough data")
  })

  dmin <- start(stockZoo)
  dmax <- end(stockZoo)

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
    text(x = 0.5, y = 0.5, paste("Can't forecast",stockSymbol, ":", cond$message), cex = 1.6, col = "red")    
    print (cond)
  },
  warning=function(cond) {
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, paste("Can't forecast",stockSymbol, "Stock:", cond$message), cex = 1.6, col = "red")    
    print (cond)
  })
}


# Server session logic
shinyServer(function(input, output, session) {
  cleanUI <- function(){
    output$distPlot <- NULL
    output$compPlot <- NULL
    output$clustPlot <- NULL
    hideTab(inputId = "tabPanels", target = "compPanel")
    hideTab(inputId = "tabPanels", target = "forecastPanel")
    hideTab(inputId = "tabPanels", target = "clustPanel")
  }
  
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
    cleanUI()
  }  
  
  observe({
    init(input$back,input$compression)
  })
  
  observe({
    input$clustm
    nStocks(input$stocksNum)
    freq(input$freq)
    toPredict(input$predict)
    cleanUI()
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
    
    shinyjs::enable("go")
    hideTab(inputId = "tabPanels", target = "clustPanel")
  })

  
  fillStockTable <- function() {
    output$tbl2 = DT::renderDataTable(
      nasdaqTraded(), 
      #    selection = 'single',
      server = FALSE, 
      options = list (
        lengthChange=FALSE, 
        pageLength=10)
    )
    output$msg2 <- NULL
    cleanUI()
  }
  
  observeEvent(input$reset, {
    fillStockTable()
  })

  # fill the tab when stocks loaded
  observe({
    fillStockTable()
  })

  # when stock selection changes
  observe({
    if (is.null(selectedStocks()) | length(selectedStocks()) == 0 ) {
      shinyjs::disable("go")
      shinyjs::disable("reset")
    } else {
      cleanUI()
      shinyjs::enable("go")
      shinyjs::enable("reset")
    }
  })
  
  observe({
    selectedStocks(nasdaqTraded()[input$tbl2_rows_selected,1])
    output$msg2 = renderText(paste0(selectedStocks(),"; "))
  })
  
  # On "Run Forecast"
  observeEvent(input$go, {
    
    # launch the modal
    shinyWidgets::updateProgressBar(session = session, id = "pb", value = 0)
    session$sendCustomMessage(type = 'launch-modal', "my-modal") 
    
    # Load stock series data
    nt <- nasdaqTraded()
    
    stocks <- nt[selectedStocks(),]
    nStocks(length(selectedStocks()))
    
    z <- readStocksAsSingleZoo(stocks, input$dateFrom, quote(), isMonthly(), session)
    print(paste("Stocks loaded:", ncol(z), "out of", nStocks()))

    # Plot stock series (single plot)
    showTab(inputId = "tabPanels", target = "compPanel")
    output$compPlot <- renderPlot({
      plot(z, plot.type="s", col=1:ncol(z), xlab="", ylab="")
      legend("topleft",legend=colnames(z), text.col=1:ncol(z), bty='o')
    }, height = 600, res=100)
    
    # Forecast and plot stock series
    showTab(inputId = "tabPanels", target = "forecastPanel")
    output$distPlot <- renderPlot({
      par(mfrow = c(ncol(z), 1), mar=c(6,3,2,2))
      for (n in 1:ncol(z)) {
        plotForecast2(z[,n], colnames(z)[n], freq(), toPredict(), isMonthly())
      }
    }, height = 500 + 100*(nStocks()-1), res=100)
    
    # Culculate and plot clusters
    
    shinyWidgets::updateProgressBar(session = session, id = "pb", 
                                    value = 80,
                                    title = "Analyzing...")

    if (ncol(z) > 2) {
      tsStocks <- as.ts(z)
      tsStocks <- na.interpolation(tsStocks)
      diffs <- rep(1, ncol(tsStocks))
      logs <- rep(TRUE, ncol(tsStocks))
      set.seed(74)
      tryCatch({
        if (input$clustm == "PRED") {
          dpred <- diss(tsStocks, "PRED", h = 6, B = 1200, logarithms = logs, differences = diffs,  plot = FALSE)    
          output$clustPlot <- renderPlot({
            hc.dpred <- hclust(dpred$dist)
            plot(hc.dpred, main = "", sub = "", xlab = "", ylab = "")
          })
        } else {
          IP.dis <- diss(tsStocks, "CORT")
          output$clustPlot <- renderPlot({
            clust <- hclust(IP.dis)
            plot(clust, main = "", sub = "", xlab = "", ylab = "")
          })
        }
        showTab(inputId = "tabPanels", target = "clustPanel")
      },
      error=function(cond) {
        output$clustPlot <- renderPlot({
          plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
          text(x = 0.5, y = 0.5, 
               paste0("Clustefirg failed due to data,\ntry different time window, stock series or distance algorithm.",
                      "\nOriginal error message:\n[",cond$message, "]"), col = "red")    
        })
        showTab(inputId = "tabPanels", target = "clustPanel")
      })
      
    } else {
      output$clustPlot <- NULL
      hideTab(inputId = "tabPanels", target = "clustPanel")
    }
    
    # Close modal
    session$sendCustomMessage(type = 'remove-modal', "my-modal")

    # Switch to "Forecast" panel    
    updateTabsetPanel(session, "tabPanels", selected = "compPanel")
  })  
})
