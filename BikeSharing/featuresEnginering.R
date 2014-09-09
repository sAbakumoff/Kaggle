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
full.data <- adjust.columns(full.data)





get.col.value <- function(weekday, hour, v, data, col.name){
  if(!is.na(v)) return(v)
  col.values <- data[data$weekday == weekday & data$hour==hour, col.name ]
  return(median(col.values, na.rm=TRUE))
}

append.stat.data <- function(data, base.column, new.cols.number, factor, get.new.col.name){
  prev.column <- base.column
  rows <- nrow(data)
  for(i in 1:new.cols.number){
    interval <- i * factor
    new.col.name <- get.new.col.name(i)
    data[(1+interval) : rows, new.col.name] <- data[1:(rows-interval), base.column]
    data[1: interval, new.col.name] <- data[1:interval, prev.column]
    prev.column <- new.col.name
    data[, new.col.name] <- mapply(function(weekday, hour, value) get.col.value(weekday, hour, value, data, new.col.name), data$weekday, data$hour, data[, new.col.name])
  }
  return(data)
}

casual.train<-NULL
casual.test<-NULL

reg.train<-NULL
reg.test<-NULL

for(y in 11:12){
  for(m in 1:12){
    month.data <- subset(full.data, year==y & month==m)
    
    month.casual <- append.stat.data(month.data, 'casual', 4, 1, function(x) paste('casual', 'in', 'minus', x,'hours', sep='.'))
    month.casual <- append.stat.data(month.casual, 'casual', 6, 24, function(x) paste('casual', 'in', 'minus', x,'days', sep='.'))
    month.casual <- append.stat.data(month.casual, 'casual', 3, 24*7, function(x) paste('casual', 'in', 'minus', x,'weeks', sep='.'))
    month.casual.train <- subset(month.casual, !is.na(casual))
    month.casual.test <- subset(month.casual, is.na(casual))
    casual.train <- rbind(casual.train, month.casual.train)
    casual.test <- rbind(casual.test, month.casual.test)
    
    month.reg <- append.stat.data(month.data, 'registered', 4, 1, function(x) paste('registered', 'in', 'minus', x,'hours', sep='.'))
    month.reg <- append.stat.data(month.reg, 'registered', 6, 24, function(x) paste('registered', 'in', 'minus', x,'days', sep='.'))
    month.reg <- append.stat.data(month.reg, 'registered', 3, 24 * 7, function(x) paste('registered', 'in', 'minus', x,'weeks', sep='.'))
    month.reg.train <- subset(month.reg, !is.na(casual))
    month.reg.test <- subset(month.reg, is.na(casual))
    reg.train <- rbind(reg.train, month.reg.train)
    reg.test <- rbind(reg.test, month.reg.test)
    
  }
}

casual.train$registered<-NULL
casual.test$casual<-NULL
casual.test$count<-NULL
casual.test$registered<-NULL

reg.train$casual<-NULL
reg.test$casual<-NULL
reg.test$count<-NULL
reg.test$registered<-NULL

write.csv(casual.train, 'casual.train.csv', row.names=FALSE)
write.csv(casual.test, 'casual.test.csv', row.names=FALSE)

write.csv(reg.train, 'reg.train.csv', row.names=FALSE)
write.csv(reg.test, 'reg.test.csv', row.names=FALSE)

