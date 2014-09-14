dates <- seq(as.Date('2011/1/1'), as.Date('2012/12/31'), by = 'day')
dates.formatted <- sapply(dates, function(x) format(x, '%Y/%m/%d'))
urls <- paste('http://www.wunderground.com/history/airport/KDCA/', dates.formatted, '/DailyHistory.html?format=1', sep='')

column.types <- test.column.types <- c('character', # TimeEST
                                       'numeric',   # TemperatureC 
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
                                       'character'  # DateUTC
)

missing.types <- c("N/A", "", '-')

get.data <- function(url){
  print(url)
  data.set <- read.csv(url(url), colClasses=column.types, na.strings=missing.types)
  data.set[,1] <- NULL
  weather.data.set <<- rbind(weather.data.set, data.set)
}
sapply(urls, get.data)

names(weather.data.set)[ncol(weather.data.set)] <- 'date.time'
weather.data.set$date.time <- gsub('<br />','',weather.data.set$date.time)
weather.data.set$date.time <- as.POSIXct(weather.data.set$date.time, tz='UTC')
weather.data.set$date.time <- format(weather.data.set$date.time, tz='EST', usetz=TRUE)

weather.data.set$date.time <- strptime(weather.data.set$date.time, format='%Y-%m-%d %H:%M:%S')

weather.data.set$year <- as.numeric(format(weather.data.set$date.time, '%y'))
weather.data.set$month <- as.numeric(format(weather.data.set$date.time, '%m'))
weather.data.set$day <- as.numeric(format(weather.data.set$date.time, '%d'))
weather.data.set$hour <- as.numeric(format(weather.data.set$date.time, '%H'))
weather.data.set$weekday <- as.numeric(format(weather.data.set$date.time, '%w'))
