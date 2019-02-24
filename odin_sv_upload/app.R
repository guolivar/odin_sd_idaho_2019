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
library(RCurl)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("ODIN-SD summary"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("odintype",
                    "Type of dataset",
                    choices = c("GSM","WIFI","OFFLINE")),
        tags$hr(),
        conditionalPanel(condition = "input.odintype == 'OFFLINE'",
        textInput('start_timestamp', 'GMT Start time [YYYY/MM/DD HH:MM:SS]:'),
        tags$hr()),
        fileInput("file1", "Choose CSV File",
                  multiple = TRUE,
                  accept = c("text/csv",
                             "text/plain",
                             ".txt",
                             ".csv"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("PM Timeseries",plotOutput("Tseries_PM")),
          tabPanel("PM Time Variation",plotOutput("TVar_PM")),
          tabPanel("T&RH Timeseries",plotOutput("Tseries_TRH")),
          tabPanel("T&RH Time Variation",plotOutput("TVar_TRH")),
          tabPanel("Download data",downloadButton('downloadData', 'Download Dataset'))
        )
      )
   )
)

# Define server logic required to draw a histogram
options(shiny.maxRequestSize = 90*1024^2)
server <- function(input, output) {
  odin.data <- reactive({
    # Get the type of data (GSM, WIFI, OFFLINE)
    req(input$odintype)
    # Get the file to parse
    req(input$file1)
    # Load the file
    df <- read.delim(input$file1$datapath,
                     header = FALSE,
                     sep = ';',
                     blank.lines.skip = TRUE)
    # OFFLINE deployment doesn't have correct internal clock and has only 1 date-time field
    if (input$odintype == "OFFLINE"){
      names(df) <- c('framelength',
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
    } else {
      # GSM and Wifi deployments are expected to have correct internal clock and 2 date-time fields
      names(df) <- c('framelength',
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
                     'Time',
                     'Day2',
                     'Time2')
    }
    #print(head(df))
    # Get correct start date for OFFLINE
    if (input$odintype == "OFFLINE"){
      req(input$start_timestamp)
      print(input$start_timestamp)
      start_timestamp <- as.POSIXct(input$start_timestamp,
                                    format = '%Y/%m/%d %H:%M:%S',
                                    tz='GMT')
      #print(start_timestamp)
      # Create the correct date field for OFFLINE dataset
      timediff <- start_timestamp - as.POSIXct(paste(df$Day,df$Time)[1],
                                               format = '%Y/%M/%d %H:%M:%S',
                                               tz = 'GMT')
      df$date <- as.POSIXct(paste(df$Day,df$Time),
                            format = '%Y/%M/%d %H:%M:%S', tz = 'GMT') + timediff
    } else {
      df$date <- as.POSIXct(paste(df$Day,df$Time),
                            format = '%Y/%M/%d %H:%M:%S', tz = 'GMT')
    }
    
    # Remove records that are too far in the past
    df <- subset(df,date > as.POSIXct('2017/01/01 00:00:00',
                                      format = '%Y/%M/%d %H:%M:%S', tz = 'GMT'))
    
    #print(head(df))
    return(df)
  })
  
  # Plots
  output$Tseries_PM <- renderPlot({
    timePlot(odin.data(),pollutant = c('PM1','PM2.5','PM10'),
             group = TRUE,
             avg.time = '10 min',
             y.relation = 'free',
             main = odin.data()$Serialn[1])
   })
  output$TVar_PM <- renderPlot({
    timeVariation(odin.data(),pollutant = c('PM1','PM2.5','PM10'),
                  main = odin.data()$Serialn[1])
  })
  output$Tseries_TRH <- renderPlot({
    timePlot(odin.data(),pollutant = c('Temperature','RH'),
             group = FALSE,
             avg.time = '1 hour',
             y.relation = 'free',
             main = odin.data()$Serialn[1])
  })
  output$TVar_TRH <- renderPlot({
    timeVariation(odin.data(),pollutant = c('Temperature','RH'),
                  main = odin.data()$Serialn[1])
  })
  
  # Download data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(odin.data()$Serialn[1],
             "_",
             strftime(odin.data()$date[1], "%Y%m%d%H%M%S"),
             '.csv')
    },
    content = function(file) {
      # Calculate the output file (10 minute average)
      avg.odin.data <- timeAverage(odin.data()[,c('date',
                                                  'PM1',
                                                  'PM2.5',
                                                  'PM10',
                                                  'Temperature',
                                                  'RH')],avg.time = '10 min')
      # Build the unique file name
      output.file <- paste0(odin.data()$Serialn[1],
                            "_",
                            strftime(odin.data()$date[1], "%Y%m%d%H%M%S"),
                            '.csv')
      # Write the data to a local file for upload to FTP
      write.csv(avg.odin.data,paste0('./data/',
                                   output.file),
                row.names = FALSE)
      # Upload to FTP server
      ftpUpload(paste0('./data/',
                       output.file),
                paste0("ftp://ftp.niwa.co.nz/incoming/GustavoOlivares/Idaho/",
                       output.file))
      # Remove local file
      system(paste0('rm ./data/',
                    output.file))
      # Send the data to download for client
      write.csv(avg.odin.data,file,
                row.names = FALSE)

    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

