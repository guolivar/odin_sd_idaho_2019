#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(openair)
library(readr)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("ODIN-SD summary"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        fileInput("file1", "Choose CSV File",
                  multiple = TRUE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        # Horizontal line ----
        tags$hr(),
        textInput('odinID', 'ODIN-SD serial number:'),
        tags$hr(),
        textInput('start_timestamp', 'Start time [YYYY/MM/NN HH:MM:SS]:'),
        tags$hr(),
        textInput('lat', 'Latitude [+/-XX.XXXX]:'),
        textInput('lon', 'Longitude [+/-XX.XXXX]:')
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("Tseries_PM"),
         plotOutput("TVar_PM"),
         plotOutput("Tseries_TRH"),
         plotOutput("TVar_TRH")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  req(input$file1)
  odin.data <- read.delim(input$file1$datapath,
                          header = FALSE)
  names(odin.data) <- c('framelength',
                        'PM1',
                        'PM2.5',
                        'PM10',
                        'PM1x',
                        'PM2.5x',
                        'PM10x',
                        'GAS1sn',
                        'GAS1',
                        'TGAS1',
                        'GAS2',
                        'Temperature',
                        'RH',
                        'DeviceID',
                        'Serialn',
                        'Day',
                        'Time')
  
   output$Tseries_PM <- renderPlot({
      # TODO plot
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

