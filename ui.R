library(shiny)

shinyUI(pageWithSidebar(
  headerPanel('Atlantic Tropical Storm and Hurricane Durations by Category'),
  sidebarPanel(
    uiOutput('year'),
    helpText("Note: The data view will show all of the observations",
             "for the selected year. The plot is arranged to show the",
             "duration of the individual storm going from weaker storms",
             "to stronger storms labeled by named storms.",
             "In addition to the observations, the summary data for that",
             "is also shown.")
  ),
    
  mainPanel(
    plotOutput('plot1'),
    h4("Summary"),
    verbatimTextOutput('summary'),
    h4("Observations"),
    tableOutput('view')
  )
))
