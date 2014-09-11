dates <- seq(as.Date('2011/1/1'), as.Date('2011/1/2'), by = 'day')
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
  data.set <- read.csv(url(url), colClasses=column.types, na.strings=missing.types)
  weather.data.set <<- rbind(w, data.set)
}

sapply(urls, get.data)