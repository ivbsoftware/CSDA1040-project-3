library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)

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
      actionButton("select", "Select Stocks", width = "100%"),
      p(),              
      actionButton("go", "Run Forecast", width = "100%", class="btn btn-primary"),
      hr(),              
      dateInput(inputId="dateFrom", label = "From", format = "yyyy-mm-dd", width = "100%", value = NA),
      sliderInput("back", min=-100, max=-1, value=-3, label=""),
      radioButtons("compression", "Aggregate Data:",
                   c("Monthly" = "m","Daily" = "d")),
      sliderInput("stocksNum", min=1, max=5, value=3, label="Stocks to Load"),
      sliderInput("freq", min=6, max=24, value=12, label="Window"),
      sliderInput("predict", min=6, max=24, value=12, label="To Predict")
#      p(),              
#      actionButton("updateRandom", "Load Random Stocks",width = "100%"),  
#      actionButton("reset", "Reset", width = "100%", class="btn btn-warning")
    ),
    
    mainPanel(
      plotOutput("distPlot")
    )
  )
))
