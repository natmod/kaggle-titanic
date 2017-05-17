library(caret)
library(randomForest)
library(fields)

#load data
trainSet <- read.table("train.csv", sep = ",", header = TRUE)
testSet <- read.table("test.csv", sep = ",", header = TRUE)

#find best features
table(trainSet[,c("Survived", "Pclass")])
table(trainSet[,c("Survived", "Sex")])
table(trainSet[,c("Survived", "Embarked")])
table(trainSet[,c("Survived", "SibSp")])
table(trainSet[,c("Survived", "Parch")])

bplot.xy(trainSet$Survived, trainSet$Fare)

#train random forest model using 5-fold cross validation
trainSet$Survived <- factor(trainSet$Survived)
model <- train(Survived ~ Pclass + Sex + Embarked + SibSp + Parch + Fare,
               data = trainSet,
               method = "rf",
               trControl = trainControl(method = "cv", number = 5))

model

#make predictions and write table for submission
testSet$Fare <- ifelse(is.na(testSet$Fare), mean(testSet$Fare, na.rm = TRUE), testSet$Fare)
testSet$Survived <- predict(model, newdata = testSet)
submission <- testSet[,c("PassengerId", "Survived")]
write.table(submission, file = "submission.csv", col.names = TRUE, row.names = FALSE, sep = ",")