###################################################
# Read Idaho metadata files (TXT format)
# Author: Gustavo Olivares
###################################################

# Libraries setup
library(librarian)
shelf(readr)

# Paths
datapath <- path.expand('~/data/ODIN_IDAHO/2019/WORKING/')
datafile <- 'ODIN0068.meta'
# Read metadata file
raw_meta <- read_file(paste0(datapath,datafile))

# Parse start and end dates
pos1 <- regexpr('On',raw_meta)
pos2 <- regexpr('@',raw_meta)
pos3 <- regexpr('hrs',raw_meta)
start_date <- substr(raw_meta,pos1+2,pos2-1)
start_time <- substr(raw_meta,pos2+1,pos3-1)
start_timestamp <- as.POSIXct(paste0(start_date,start_time), format = '%m/%d/%y %H:%M', tz = 'PST')

pos1 <- regexpr('Off',raw_meta)
pos2 <- regexpr('@',raw_meta)
pos3 <- regexpr('hrs',raw_meta)
end_date <- substr(raw_meta,pos1+3,pos2-1)
end_time <- substr(raw_meta,pos2+1,pos3-1)
end_timestamp <- as.POSIXct(paste0(end_date,end_time), format = '%m/%d/%y %H:%M', tz = 'PST')

# Parse location
pos1 <- regexpr('lat',raw_meta)
pos2 <- regexpr('lon',raw_meta)
lat <- as.numeric(substr(raw_meta,pos1+3,pos2-1))
lon <- as.numeric(substr(raw_meta,pos2+3,pos2+11))

# Parse sample interval
pos1 <- regexpr('Sample interval',raw_meta)
pos2 <- regexpr('[0-9]{3}',substring(raw_meta,pos1))
interval <- as.numeric(substr(raw_meta,pos1+pos2-1,pos1+pos2+2))
