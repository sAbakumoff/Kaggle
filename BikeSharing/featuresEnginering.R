#setwd('~/Documents/Projects/Kaggle/BikeSharing/')
setwd('f:/Projects/Kaggle/BikeSharing/')
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

#idea : split temperature to groups using 10 day interval values for each hour! : levels(cut(march.data$temp, breaks=4)), extract breakpoints, assign each value to group!
temp.groups<-split(full.data$temp, list(full.data$year, full.data$month, full.data$hour))
build.ranges <- function(values, breaks){
  levels <- levels(cut(values,breaks=breaks))
  return(
    cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", levels) ),
          upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", levels) ),
          range = c('low', 'ave', 'high', 'extra-high')
          )    
    )
}
temp.levels <- lapply(temp.groups, function(item) build.ranges(item, 4))

full.data[,'year.month.hour'] <- paste(full.data$year, '.', full.data$month, '.', full.data$hour, sep='')

get.temp.range <- function(y.m.h, temp){
  #print(temp)
  ranges <- temp.levels[y.m.h][[1]]
  #print(length(ranges))
  print(ranges[temp >= ranges[,1] & temp < ranges[,2], 3])
  #return(colnames(ranges[ranges[, 'low'] <= temp & ranges[, 'high'] > temp]))
}

tempo<-mapply(get.temp.range, full.data$year.month.hour, full.data$temp)
