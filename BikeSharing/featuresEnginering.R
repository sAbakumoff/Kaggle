setwd('~/Documents/Projects/Kaggle/BikeSharing/')
#setwd('f:/Projects/Kaggle/BikeSharing/')
adjust.columns <- function(data){
  data$datetime <- strptime(data$datetime, format='%Y-%m-%d %H:%M:%S')
  data$month <- as.numeric(format(data$datetime, '%m'))
  data$weekday <- as.numeric(format(data$datetime, '%w'))
  data$hour <- as.numeric(format(data$datetime, '%H'))
  data$year <- as.numeric(format(data$datetime, '%y'))
  return(data)
}

missing.types <- c("NA", "")

process.dataset<-function(input.file, column.types, output.file){
  data <- read.csv(input.file, na.strings=missing.types, colClasses=column.types)
  data <- adjust.columns(data)
  #write.csv(data, output.file, row.names=FALSE)
  return(data)
}


test.column.types <- c('character',   # datetime
                  'factor',    # season 
                  'factor',    # holiday
                  'factor',    # workingday
                  'factor',    # weather
                  'numeric',   # temp
                  'numeric',   # atemp
                  'numeric',   # humidity
                  'numeric'    # windspeed
)
train.column.types <- c(test.column.types,
                        'numeric',   # casual
                        'numeric',   # registered
                        'numeric'   # count                        
                        )

train.data <- read.csv('train.csv', na.strings=missing.types, colClasses=train.column.types)
test.data <- read.csv('test.csv', na.strings=missing.types, colClasses=test.column.types)

test.data$casual <- NA
test.data$registered <- NA
test.data$count <- NA

full.data <- rbind(train.data, test.data)
#full.data$datetime <- as.POSIXct(full.data$datetime, format='%Y-%m-%d %H:%M:%S', tz='GMT')
full.data <- adjust.columns(full.data)

full.data[full.data$temp < 15, 'temp.cat']<-'low'
full.data[full.data$temp >= 15 & full.data$temp < 30, 'temp.cat']<-'normal'
full.data[full.data$temp >= 30, 'temp.cat']<-'high'
full.data$temp.cat<-as.factor(full.data$temp.cat)

