library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(shinyBS)

## UI extensions
jscode <- "
shinyjs.refocus = function(e_id) {
document.getElementById(e_id).focus();
}"

shinyUI(fluidPage(
  
  theme=shinytheme("cerulean"),
  shinyjs::useShinyjs(),
  extendShinyjs(text = jscode, functions = "refocus"),
  tags$style(HTML(" .shiny-input-container:not(.shiny-input-container-inline) { width: 100%; height: 100%; }")),
  tags$head(tags$style(type="text/css", ".container-fluid {  max-width: 1000px;}")),
  
  # Fancy progress bar
  # Source: https://stackoverflow.com/questions/44043475/adjust-size-of-shiny-progress-bar-and-center-it
  tags$script("Shiny.addCustomMessageHandler('launch-modal', function(d) {$('#' + d).modal().focus();})"),
  tags$script("Shiny.addCustomMessageHandler('remove-modal', function(d) {$('#' + d).modal('hide');})"),
  tags$div(
    id = "my-modal",
    class="modal fade", tabindex="-1", `data-backdrop`="static", `data-keyboard`="false",
    tags$div(
      class="modal-dialog",
      tags$div(
        class = "modal-content",
        tags$div(
          class="modal-body",
          shinyWidgets::progressBar(id = "pb", value = 0, title = "")
        )
      )
    )
  ),  
  
  titlePanel("Stock Analyzer"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("go", "Run Forecast", width = "100%", class="btn btn-primary"),
      hr(),              
      dateInput(inputId="dateFrom", label = "From", format = "yyyy-mm-dd", width = "100%", value = NA),
      sliderInput("back", min=-100, max=-1, value=-3, label=""),
      radioButtons("compression", "Aggregate Data:",
                   c("Monthly" = "m","Daily" = "d")),
      sliderInput("stocksNum", min=1, max=5, value=1, label="Stocks to Load"),
      sliderInput("freq", min=6, max=24, value=12, label="Window"),
      sliderInput("predict", min=6, max=24, value=12, label="To Predict"),
      # for potencial use as modal dialog
      # bsModal("modalExample", "Nasdaq Listed Stocks", "selectStocks", 
      #         size = "large", 
      #         verbatimTextOutput('msg'),
      #         p(),
      #         DT::dataTableOutput('tbl')
      #       ),
      hidden(numericInput(inputId='refresh_helper', value=0, label=NULL)) # on app start trigger
    ),

    mainPanel(
      tabsetPanel(id="tabPanels",
        tabPanel("Nasdaq Listed", value="nasdaqtPanel", 
                 p(),
                 span(textOutput('msg2'), style="color:red"),
                 p(), 
                 DT::dataTableOutput('tbl2')
        ),
        tabPanel("Forecast", value="forecastPanel", plotOutput("distPlot")),
        tabPanel("Decomposition", value="decomptPanel", span(textOutput("m1"), style="color:red")),
        tabPanel("Comparision", value="compPanel", span(textOutput("m2"), style="color:red"))      
      )
    )
  )
))