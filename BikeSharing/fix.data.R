#setwd('~/Documents/Projects/Kaggle/BikeSharing/')
setwd('f:/Projects/Kaggle/BikeSharing/')
adjust.columns <- function(data){
  data$datetime <- strptime(data$datetime, format='%Y-%m-%d %H:%M:%S')
  data$month <- as.numeric(format(data$datetime, '%m'))
  data$weekday <- as.numeric(format(data$datetime, '%w'))
  data$hour <- as.numeric(format(data$datetime, '%H'))
  data$year <- as.numeric(format(data$datetime, '%y'))
  data$day <- as.numeric(format(data$datetime, '%d'))
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

weather.column.types  <- c('numeric',   # TemperatureC 
                                       'numeric',   # Dew PointC
                                       'numeric',   # Humidity
                                       'numeric',   # Sea Level PressurehPa
                                       'numeric',   # VisibilityKm
                                       'factor',    # Wind Direction
                                       'character',   # Wind SpeedKm/h
                                       'numeric',   # Gust SpeedKm/h
                                       'numeric',   #Precipitationmm
                                       'factor',    #Events
                                       'factor',    #Conditions,
                                       'numeric',   # WindDirDegrees
                                       'character',  # date.time
                                       'numeric',    #year
                                       'numeric',    #month
                                       'numeric',    #day
                                       'numeric',    #hour
                                       'numeric'    #weekday
)

missing.types <- c("NA", "")

train.data <- read.csv('train.csv', na.strings=missing.types, colClasses=train.column.types)
test.data <- read.csv('test.csv', na.strings=missing.types, colClasses=test.column.types)

test.data$casual <- NA
test.data$registered <- NA
test.data$count <- NA

full.data <- rbind(train.data, test.data)
full.data <- adjust.columns(full.data)

weather.data <- read.csv('weather.data.csv', na.strings=missing.types, colClasses=weather.column.types)

full.data <- merge(full.data, weather.data, all.y = FALSE, all.x=FALSE, by=c('year', 'month', 'day', 'hour', 'weekday'))

full.data <- full.data[order(full.data$date.time), ]

full.data$datetime<-full.data$date.time<-full.data$weather<-full.data$temp<-full.data$atemp<-full.data$humidity<-
  full.data$windspeed<-full.data$Events<-full.data$Precipitationmm<-full.data$Gust.SpeedKm.h<-NULL

full.data[full.data$Wind.SpeedKm.h == 'Calm', 'Wind.SpeedKm.h'] <- '0'
full.data$Wind.SpeedKm.h <- as.numeric(full.data$Wind.SpeedKm.h)

fix.missing.value<-function(value, is.missing,  year, month, day, hour, data, col.name){
  if(!is.missing(value)) return(value)
  return(data[data$year==year & data$month == month & data$day==day & data$hour==hour-1, col.name])
}

is.missing<-function(x){
  return(x==-9999)
}

full.data[is.na(full.data$Humidity), 'Humidity'] <- -9999

for(col in c('Humidity', 'TemperatureC', 'Dew.PointC', 'Sea.Level.PressurehPa', 'VisibilityKm', 'Wind.SpeedKm.h', 'WindDirDegrees')){
  full.data[, col]<-mapply(function(value, year, month, day, hour) fix.missing.value(value, is.missing, year, month, day, hour, full.data, col), 
                                    full.data[, col], 
                                    full.data$year, 
                                    full.data$month, 
                                    full.data$day, 
                                    full.data$hour)
}

