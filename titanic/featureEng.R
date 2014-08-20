setwd('f:/projects/kaggle/titanic/')
#setwd('~/Documents/Projects/Kaggle/titanic/')
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
  full[full$Pclass == passenger.class & (is.na(full$Fare) | full$Fare == 0), ]$Fare = class.fare.mean
}

#Feature engineering step 2 : replace missing Embarked values
embarked.most.frequent = names(which.max(table(full$Embarked)))
full[is.na(full$Embarked),]$Embarked = embarked.most.frequent

#Feature engineering step 3: extract the Title & Family Name
name.info <- strsplit(full$Name, ',\\s|\\.\\s')
full$FamilyName <- factor(sapply(name.info, function(x) x[1]))
full$Title <- as.factor(sapply(name.info, function(x) x[2]))

#Feature engineering step 5 : merge the titles!
full[full$Title %in% c('Capt', 'Don', 'Major', 'Sir', 'Rev', 'Dr', 'Col'), 'Title'] <- 'Sir'
full[full$Title %in% c('Dona', 'Lady', 'Jonkheer'), 'Title'] <- 'Lady'
full[full$Title %in% c('the Countess', 'Ms'), 'Title'] <- 'Mrs'
full[full$Title %in% c('Mme', 'Mlle'), 'Title'] <- 'Miss'


#Feature engineering step 4: Fill the missing ages
for(title in levels(full$Title)){
  title.age.mean <- mean(full[full$Title == title,]$Age, na.rm = TRUE)
  full[full$Title == title & (is.na(full$Age) | full$Age == 0), 'Age'] = title.age.mean
}

#step 4.5 categorize the ages
full$AgeRange <- as.character('Old')
full[full$Age <= 15, 'AgeRange'] <- 'Child'
full[full$Age > 15 & full$Age<=40, 'AgeRange'] <- 'Adult'
full$AgeRange <- as.factor(full$AgeRange)


#Step 6: "women and children first" protocol support
full$Boat.dibs <- 'No'
full$Boat.dibs[which(full$Sex == 'female' | full$Age < 15)] <- 'Yes'
full$Boat.dibs <- as.factor(full$Boat.dibs)

#step6 : family size
full$FamilySize <- full$SibSp + full$Parch
full$FamilyRange <- as.character('alone')
full[full$FamilySize > 0 & full$FamilySize < 3, 'FamilyRange'] <- 'small'
full[full$FamilySize == 3, 'FamilyRange'] <- 'avg'
full[full$FamilySize > 3, 'FamilyRange'] <- 'large'
full$FamilyRange <- as.factor(full$FamilyRange)


train_tidy <- full[!is.na(full$Survived),]
test_tidy <- full[is.na(full$Survived),]
test_tidy$Survived <- NULL

write.csv(test_tidy, 'test_tidy.csv')
write.csv(train_tidy, 'train_tidy.csv')