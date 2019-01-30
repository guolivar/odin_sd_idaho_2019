###################################################
# Read Idaho files (TXT format)
# Author: Gustavo Olivares
###################################################
#' ---
#' title: "ODIN-SD data from IDAHO 2019"
#' author: "Gustavo Olivares"
#' ---

# Libraries setup
library(librarian)
shelf(readr,
      openair)

# Paths
datapath <- path.expand('~/data/ODIN_IDAHO/2019/WORKING/')
metadatafile <- 'ODIN0068.meta'
datafile <- 'ODIN0068.txt'
# Read metadata file
raw_meta <- read_file(paste0(datapath,metadatafile))
# Read data file
odin.data <- read.delim(paste0(datapath,datafile),
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

# Parse start and end dates
pos1 <- regexpr('On',raw_meta)
pos2 <- regexpr('@',raw_meta)
pos3 <- regexpr('hrs',raw_meta)
start_date <- substr(raw_meta,pos1+2,pos2-1)
start_time <- substr(raw_meta,pos2+1,pos3-1)
start_timestamp <- as.POSIXct(paste0(start_date,start_time),
                              format = '%m/%d/%y %H:%M',
                              tz = 'PST')

pos1 <- regexpr('Off',raw_meta)
pos2 <- regexpr('@',raw_meta)
pos3 <- regexpr('hrs',raw_meta)
end_date <- substr(raw_meta,pos1+3,pos2-1)
end_time <- substr(raw_meta,pos2+1,pos3-1)
end_timestamp <- as.POSIXct(paste0(end_date,end_time),
                            format = '%m/%d/%y %H:%M',
                            tz = 'PST')

# Parse location
pos1 <- regexpr('lat',raw_meta)
pos2 <- regexpr('lon',raw_meta)
lat <- as.numeric(substr(raw_meta,pos1+3,pos2-1))
lon <- as.numeric(substr(raw_meta,pos2+3,pos2+11))

# Parse sample interval
pos1 <- regexpr('Sample interval',raw_meta)
pos2 <- regexpr('[0-9]{3}',substring(raw_meta,pos1))
interval <- as.numeric(substr(raw_meta,pos1+pos2-1,pos1+pos2+2))

# Correct date in data
timediff <- start_timestamp - as.POSIXct(paste(odin.data$Day,odin.data$Time)[1],
                                         format = '%d/%m/%Y %H:%M:%S', tz = 'PST')
odin.data$date <- as.POSIXct(paste(odin.data$Day,odin.data$Time),
                             format = '%d/%m/%Y %H:%M:%S', tz = 'PST') + timediff

# Plot timeseries
timePlot(odin.data,pollutant = c('PM1','PM2.5','PM10'),
         group = TRUE,
         avg.time = '10 min',
         y.relation = 'free',
         main = odin.data$Serialn[1])

timeVariation(odin.data,pollutant = c('PM1','PM2.5','PM10'),
              main = odin.data$Serialn[1])

timePlot(odin.data,pollutant = c('Temperature','RH'),
         group = FALSE,
         avg.time = '1 hour',
         y.relation = 'free',
         main = odin.data$Serialn[1])

timeVariation(odin.data,pollutant = c('Temperature','RH'),
              main = odin.data$Serialn[1])
