#setting up wd
setwd("C:/Users/Rafiki/Documents/Github/titanic-kaggle")

#loading datasets
trainData <- read.csv("train.csv", header= TRUE, stringsAsFactors = FALSE)
testData <- read.csv("test.csv", header= TRUE, stringsAsFactors = FALSE)

#Decision tree model
library(rpart)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data = trainData,
             method = "class")