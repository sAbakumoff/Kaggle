setwd('~/Documents/Projects/Kaggle/BikeSharing/')
missing.types <- c("NA", "")
column.types <- c('character',   # datetime
                  'factor',    # season 
                  'factor',    # holiday
                  'factor', # workingday
                  'factor',    # weather
                  'numeric',   # temp
                  'numeric',   # atemp
                  'numeric',   # humidity
                  'numeric', # windspeed
                  'numeric',   # casual
                  'numeric', # registered
                  'numeric'     # count
)
train <- read.csv("train.csv", na.strings=missing.types, colClasses=column.types)
train$datetime <- strptime(train$datetime, format='%Y-%m-%d %H:%M:%S')
train$month <- format(train$datetime, '%m')
train$weekday <- format(train$datetime, '%w')
train$hour <- format(train$datetime, '%H')
train$year <- format(train$datetime, '%y')