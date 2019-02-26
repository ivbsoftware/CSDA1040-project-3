library(shiny)
library(shinythemes)
library(shinyjs)

## UI extensions
jscode <- "
shinyjs.refocus = function(e_id) {
document.getElementById(e_id).focus();
}"

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  theme=shinytheme("journal"),
  shinyjs::useShinyjs(),
  extendShinyjs(text = jscode, functions = "refocus"),
  tags$style(HTML(" .shiny-input-container:not(.shiny-input-container-inline) { width: 100%; height: 100%; }")),
  tags$head(tags$style(type="text/css", ".container-fluid {  max-width: 1000px;}")),
  
  titlePanel("Stock Analyzer"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      dateInput(inputId = "dateFrom", label = "From", value = seq(Sys.Date(), length=2, by = "-364 days")[2],
               format = "yyyy-mm-dd", width = "100%")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      column(12,
        plotOutput("distPlot")
      )
    )
  )
))
