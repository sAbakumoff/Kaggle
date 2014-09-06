setwd('~/Documents/Projects/Kaggle/BikeSharing/')
#setwd('f:/Projects/Kaggle/BikeSharing/')
adjust.columns <- function(data){
  datetime <- strptime(data$datetime, format='%Y-%m-%d %H:%M:%S')
  #data$month <- as.numeric(format(data$datetime, '%m'))
  #data$week.day <- as.numeric(format(data$datetime, '%w'))
  #data$hour <- as.numeric(format(data$datetime, '%H'))
  #data$year <- as.numeric(format(data$datetime, '%y'))
  #data$month.day <- as.numeric(format(data$datetime, '%d'))
  data$year.month.weekday.hour <- as.factor(format(datetime, '%y.%m.%w.%H'))
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

train.data$count<-NULL
test.data$casual <- NA
test.data$registered <- NA
#test.data$count <- NA

full.data <- rbind(train.data, test.data)
#full.data$datetime <- as.POSIXct(full.data$datetime, format='%Y-%m-%d %H:%M:%S', tz='GMT')
#full.data <- adjust.columns(full.data)

full.data <- full.data[order(as.Date(full.data$datetime, format='%Y-%m-%d %H:%M:%S')), ]
full.data$datetime <- strptime(full.data$datetime, format='%Y-%m-%d %H:%M:%S')
full.data$year.month.weekday.hour <- as.factor(format(full.data$datetime, '%y.%m.%w.%H'))
median.casual <- tapply(full.data$casual, year.month.weekday.hour, median, na.rm = TRUE)
#full.data <- adjust.columns(full.data)

f<-function(dt, hour){
  month <- format(dt, '%m')
  week.day <- format(dt, '%w')
  hour <- as.numeric(format(dt, '%H')) - hour
  year <- format(dt, '%y')
  y.m.wd.h<-paste(year, month, week.day, hour, sep='.')
  return(median.casual[y.m.wd.h])
}

base.column <- 10
column.after <- 11
prev.column <- 10
n.rows<-nrow(full.data)
for(i in 1 : 12){
  new.column <- paste('casual.in.-', i, '.hours', sep='')
  
  full.data[1:i, new.column] <- full.data[1:i, prev.column]
  
  full.data[(1+i) : n.rows, new.column] <- full.data[1: (n.rows-i), base.column]   
  
  na.subset <- full.data[is.na(full.data[, new.column]),]
  
  full.data[ is.na( full.data[, new.column] ), new.column ] <- mapply( function(x) f (x, i ),  full.data[ is.na( full.data[, new.column ] ), 'datetime' ] )
  
  
  prev.column <- new.column
}
