setwd('f:/projects/kaggle/titanic/')
missing.types <- c("NA", "")
column.types <- c('integer',   # PassengerId
                        'factor',    # Survived 
                        'factor',    # Pclass
                        'character', # Name
                        'factor',    # Sex
                        'numeric',   # Age
                        'integer',   # SibSp
                        'integer',   # Parch
                        'character', # Ticket
                        'numeric',   # Fare
                        'character', # Cabin
                        'factor'     # Embarked
)
train <- read.csv("train.csv", na.strings=missing.types, colClasses=column.types)
test <- read.csv("test.csv", na.strings=missing.types, colClasses=column.types[-2])
test$Survived <- NA
full <- rbind(test, train)

#Feature engineering step 1 : replace missing and zero-es Fare values.
for(passenger.class in levels(full$Pclass)){
  class.fare.mean <- mean(full[full$Pclass == passenger.class,]$Fare, na.rm = TRUE)
  train[train$Pclass == passenger.class & (is.na(train$Fare) | train$Fare == 0), ]$Fare = class.fare.mean
}

#Feature engineering step 2 : replace missing Embarked values