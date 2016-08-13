#setting up wd
setwd("C:/Users/Rafiki/Documents/Kaggle/Titanic")

#loading datasets
trainData <- read.csv("train.csv", header= TRUE, stringsAsFactors = FALSE)
testData <- read.csv("test.csv", header= TRUE, stringsAsFactors = FALSE)

##### EXPLORATORY DATA ANALYSIS - trainData #####

#plotting densities for numeric variables
plot(density(trainData$Age, na.rm = TRUE))
plot(density(trainData$Age, na.rm = TRUE))

#survival rate by sex bar plot
counts <- table(trainData$Survived, trainData$Sex)
barplot(counts, xlab = "Sex", ylab = "Number of people", 
        main = "Survived and deceased - male and female")
paste("% of females who survived:",counts[2]/(counts[1]+counts[2]), sep = " ")
paste("% of males who survived:", counts[4]/(counts[3]+counts[4]), sep = " ")

#survival rate by passenger class plot
pclass_survival <- table(trainData$Survived, trainData$Pclass)
barplot(pclass_survival, xlab = "Passenger class", ylab = "Number of people",
        main = "Survived and deceased by passanger class")
paste("% of 1st class passengers who survived:", pclass_survival[2]/(pclass_survival[1]+pclass_survival[2]))
paste("% of 2nd class passengers who survived:", pclass_survival[4]/(pclass_survival[3]+pclass_survival[4]))
paste("% of 3rd class passengers who survived:", pclass_survival[6]/(pclass_survival[5]+pclass_survival[6]))

#removing unwanted variables
trainData = trainData[-c(1,9:12)]

#replacing sex with dummy variable
trainData$Sex = gsub("female", 1, trainData$Sex)
trainData$Sex = gsub("male", 0, trainData$Sex)

##### DATA CLEANING - trainData #####

#simplifying names to titles
master_vector = grep("Master.", trainData$Name, fixed = TRUE)
miss_vector = grep("Miss.", trainData$Name, fixed = TRUE)
mrs_vector = grep("Mrs.", trainData$Name, fixed = TRUE)
mr_vector = grep("Mr.", trainData$Name, fixed = TRUE)
dr_vector = grep("Dr.", trainData$Name, fixed = TRUE)

for (i in master_vector){
  trainData$Name[i] = "Master"

}
for (i in miss_vector){
  trainData$Name[i] = "Miss"
}

for (i in mrs_vector){
  trainData$Name[i] = "Mrs"
}

for (i in mr_vector){
  trainData$Name[i] = "Mr"
}

for (i in dr_vector){
  trainData$Name[i] = "Dr"
}

#filling up missing values for age
master_age = round(mean(trainData$Age[trainData$Name == "Master"], na.rm = TRUE), digits = 2)
miss_age = round(mean(trainData$Age[trainData$Name == "Miss"], na.rm = TRUE), digits = 2)
mrs_age = round(mean(trainData$Age[trainData$Name == "Mrs"], na.rm = TRUE), digits = 2)
mr_age = round(mean(trainData$Age[trainData$Name == "Mr"], na.rm = TRUE), digits = 2)
dr_age = round(mean(trainData$Age[trainData$Name == "Dr"], na.rm = TRUE), digits = 2)

for (i in 1:nrow(trainData)) {
  if (is.na(trainData[i,"Age"])) {
    if (trainData[i,"Name"] == "Master") {
      trainData$Age[i] = master_age
    } else if (trainData[i,"Name"] == "Miss"){
      trainData$Age[i] = miss_age
    } else if (trainData[i, "Name"] == "Mrs"){
      trainData$Age[i] = mrs_age
    } else if (trainData[i, "Name"] == "Mr"){
      trainData$Age[i] = mr_age
    } else if (trainData[i,"Name"] == "Dr"){
      trainData$Age[i] = dr_age
    } else {
      print("Uncaught title")
    }
    
  }
}

#creating child variable
trainData$Child = NA
for (i in 1:nrow(trainData)){
  if (trainData$Age[i] <= 12){
    trainData$Child[i] = 1
  } else {
    trainData$Child[i] = 2
  }
}

#creating family size variable
trainData$Family = NA
for (i in 1:nrow(trainData)){
  trainData$Family[i] = 1 + trainData$SibSp[i] + trainData$Parch[i]
  }

#creating mother variable
trainData$Mother = NA
for (i in 1:nrow(trainData)){
  if (trainData$Name[i] == "Mrs" & trainData$Parch[i] > 0) {
    trainData$Mother[i] = 1
  } else {
    trainData$Mother[i] = 2
  }
}

##### DATA CLEANING - testData #####

PassengerId = testData[1]
testData = testData[-c(1, 8:11)]

#replacing sex with a dummy
testData$Sex = gsub("female", 1, testData$Sex)
testData$Sex = gsub("male", 0, testData$Sex)

#simplifying names to titles
master_vector = grep("Master.", testData$Name, fixed = TRUE)
miss_vector = grep("Miss.|Ms.", testData$Name)
mrs_vector = grep("Mrs.", testData$Name, fixed = TRUE)
mr_vector = grep("Mr.", testData$Name, fixed = TRUE)
dr_vector = grep("Dr.", testData$Name, fixed = TRUE)

for (i in master_vector){
  testData$Name[i] = "Master"

}
for (i in miss_vector){
  testData$Name[i] = "Miss"
}

for (i in mrs_vector){
  testData$Name[i] = "Mrs"
}

for (i in mr_vector){
  testData$Name[i] = "Mr"
}

for (i in dr_vector){
  testData$Name[i] = "Dr"
}

#filling up missing values for age
master_age = round(mean(testData$Age[testData$Name == "Master"], na.rm = TRUE), digits = 2)
miss_age = round(mean(testData$Age[testData$Name == "Miss"], na.rm = TRUE), digits = 2)
mrs_age = round(mean(testData$Age[testData$Name == "Mrs"], na.rm = TRUE), digits = 2)
mr_age = round(mean(testData$Age[testData$Name == "Mr"], na.rm = TRUE), digits = 2)
dr_age = round(mean(testData$Age[testData$Name == "Dr"], na.rm = TRUE), digits = 2)

for (i in 1:nrow(testData)) {
  if (is.na(testData[i,"Age"])) {
    if (testData[i,"Name"] == "Master") {
      testData$Age[i] = master_age
    } else if (testData[i,"Name"] == "Miss"){
      testData$Age[i] = miss_age
    } else if (testData[i, "Name"] == "Mrs"){
      testData$Age[i] = mrs_age
    } else if (testData[i, "Name"] == "Mr"){
      testData$Age[i] = mr_age
    } else if (testData[i,"Name"] == "Dr"){
      testData$Age[i] = dr_age
    } else {
      print(paste("Uncaught title at: ", i))
      print(paste("The unrecognized title was: ", testData[i,"Name"]))
    }
    
  }
}

#creating child variable
testData$Child = NA
for (i in 1:nrow(testData)){
  if (testData$Age[i] <= 12){
    testData$Child[i] = 1
  } else {
    testData$Child[i] = 2
  }
}

#creating family size variable
testData$Family = NA
for (i in 1:nrow(testData)){
  testData$Family[i] = 1 + testData$SibSp[i] + testData$Parch[i]
  }

#creating mother variable
testData$Mother = NA
for (i in 1:nrow(testData)){
  if (testData$Name[i] == "Mrs" & testData$Parch[i] > 0) {
    testData$Mother[i] = 1
  } else {
    testData$Mother[i] = 2
  }
}

##### LOGISTIC REGRESSION #####

train.glm <- glm(Survived ~ Pclass + Sex + Age + Child + Sex*Pclass + Family + Mother,
                 family = binomial, data = trainData)
summary(train.glm)

p.hats <- predict.glm(train.glm, newdata = testData, type = "response")

survival <- vector()
for (i in 1:length(p.hats)){
  if (p.hats[i] >= 0.5){
    survival[i] <- 1
  } else {
    survival[i] <- 0
  }
}

kaggle.sub <- cbind(PassengerId,survival)
colnames(kaggle.sub) <- c("PassengerID", "Survived")
write.csv(kaggle.sub, file = "final.csv", row.names = FALSE)





