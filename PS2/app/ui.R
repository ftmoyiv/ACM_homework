library(shiny)
library(devtools)

devtools::install_github('rstudio/shinyapps')
library(shinyapps)

shinyUI(pageWithSidebar(
  
  headerPanel('LoanData Characterization'),
  
  sidebarPanel(
    numericInput('muApp.Solvency', 'Mean Approved Solvency', 50, min = 1, max = 500),
    numericInput('stdApp.Solvency', 'Std.Dev Approved Solvency', 5, min = 1, max = 500),
    numericInput('muApp.PIratio', 'Mean Approved PI Ratio', 50, min = 1, max = 500),
    numericInput('stdApp.PIratio', 'Std.Dev Approved for PI Ratio', 5, min = 1, max = 500),
    
    numericInput('muDeny.Solvency', 'Mean Approved Solvency', 50, min = 1, max = 500),
    numericInput('stdDeny.Solvency', 'Std.Dev Approved Solvency', 5, min = 1, max = 500),
    numericInput('muDeny.PIratio', 'Mean Approved PI Ratio', 50, min = 1, max = 500),
    numericInput('stdDeny.PIratio', 'Std.Dev Approved Solvency', 5, min = 1, max = 500)
  ),
  
  mainPanel(
    plotOutput('plot1'),
    tableOutput('table1')
  )
))