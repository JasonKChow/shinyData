library(shiny)
library(plotly)

# Load environment
load('exEnv.RData')

# Helper function to convert console outputs into text string
consolePrint <- function(expr) {
  txt <- capture.output(expr) %>% paste(collapse = '</br>')
  gsub('\t', '&nbsp;&nbsp;&nbsp;&nbsp;', txt)
}

#### UI ####
ui <- fluidPage(theme = 'style.css',
  # Title
  fluidRow(
    column(4, titlePanel('Shiny Data Example')),
    column(8, HTML('<a href="https://www.google.com"><b>Raw Data</b></a>'))
  ),
  
  # Plot row
  fluidRow(sidebarLayout(
    sidebarPanel('This will display a plot.'),
    mainPanel(plotlyOutput('plot'))
  )),
  
  # Table row
  fluidRow(sidebarLayout(
    sidebarPanel('This will display a table.'),
    mainPanel(dataTableOutput('table'))
  )),
  
  # Output row
  fluidRow(sidebarLayout(
    sidebarPanel('This will display R outputs.'),
    mainPanel(htmlOutput('text'), class = 'code')
  ))
)

#### Server ####
server <- function(input, output) {
  output$plot <- renderPlotly({
    trialAcc <- plot_ly(trialSummary,
                        x = ~Acc,
                        type = 'histogram',
                        name = 'Accuracy')
    trialRT <- plot_ly(trialSummary,
                       x = ~MeanRT,
                       type = 'histogram',
                       name = 'Reaction Time')
    subplot(trialAcc, trialRT)
  })
  
  output$table <- renderDataTable({
    sbjSummary
  }, options = list(
    pageLength = 5
  ))
  
  output$text <- renderText({
    consolePrint({t.test(sbjSummary$Acc, mu = 1/3)})
  })
}

#### Show it! ####
shinyApp(ui = ui, server = server)