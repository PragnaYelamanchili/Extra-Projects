setwd("C:/Users/pragn/Desktop/ABA")
#import data
mydata <- read.csv("Train.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))
nrow(mydata)
summary(mydata)

#4. Droping the data fields: "cand_id", "last_name", "first_name", "twitterbirth", "facebookdate", "facebookjan", "youtubebirth".
mydata$Loan_ID <- NULL

mydata$last_name <- NULL
mydata$first_name <- NULL
mydata$twitterbirth <- NULL
mydata$facebookdate <- NULL
mydata$facebookjan <- NULL
mydata$youtubebirth <- NULL

#5.	Convert the following variables into factor variables using function 
#as.factor(): "twitter", "facebook", "youtube", "cand_ici", and "gen_election"
mydata$twitter <- as.factor(mydata$twitter)
mydata$facebook <- as.factor(mydata$facebook)
mydata$youtube <- as.factor(mydata$youtube)
mydata$cand_ici <- as.factor(mydata$cand_ici)

mydata$gen_election <- ifelse(mydata$gen_election == "L", 0, 1)
mydata$gen_election <- as.factor(mydata$gen_election)
mydata$gen_election
#7. Complete cases 
mydata <- mydata[complete.cases(mydata),]

#8. Splitting the training and test data to 70 and 30 respectively
dataframe <- sample(1:nrow(mydata), 0.7 * nrow(mydata))
train_data <- mydata[dataframe,]
train_data
test_data <- mydata[-dataframe,]
test_data

#9. Use train_data to build a random forest classifier with 10 trees. 
install.packages("randomForest")
library(randomForest)

set.seed(32)
# for 10 trees
rf <-randomForest(gen_election~., data=train_data, ntree=10, na.action=na.exclude, importance=T,proximity=T)
print(rf)
# for 20 trees
rf_20 <-randomForest(gen_election~., data=train_data, ntree=20, na.action=na.exclude, importance=T,proximity=T)
print(rf_20)
# for 30 trees
rf_30 <-randomForest(gen_election~., data=train_data, ntree=30, na.action=na.exclude, importance=T,proximity=T)
print(rf_30)
# for 40 trees
rf_40 <-randomForest(gen_election~., data=train_data, ntree=40, na.action=na.exclude, importance=T,proximity=T)
print(rf_40)
# for 50 trees
rf_50 <-randomForest(gen_election~., data=train_data, ntree=50, na.action=na.exclude, importance=T,proximity=T)
print(rf_50)
# for 60 trees
rf_60 <-randomForest(gen_election~., data=train_data, ntree=60, na.action=na.exclude, importance=T,proximity=T)
print(rf_60)
# for 70 trees
rf_70 <-randomForest(gen_election~., data=train_data, ntree=70, na.action=na.exclude, importance=T,proximity=T)
print(rf_70)

# Replace n with the number of trees you recommended in 9.5. What is the recommended value for mtry? 
mtry <- tuneRF(train_data[-26], train_data$gen_election, ntreeTry=60,  
               stepFactor=1.5, improve=0.01, trace=TRUE, plot=TRUE , na.action=na.exclude) 
#Use your recommended number of trees and mtry value to build a 
#new random forest classifier using train_data. What is OOB estimate of error rate
rf_40 <-randomForest(gen_election~., data=train_data, ntree=60, mtry = 3,na.action=na.exclude, importance=T,proximity=T)
rf_40
install.packages("ROCR")
library(ROCR)
library(ggplot2)
library(class)
library(caret)


predicted_values <- predict(rf, test_data, type= "prob")
head(predicted_values)
threshold <- 0.5

pred <- factor( ifelse(predicted_values[,1] > threshold, 1, 0))
head(pred)
levels(pred)
levels(test_data$gen_election)
confusionMatrix(pred, test_data$gen_election, 
                positive = levels(test_data$gen_election)[2])
predicted_values <- predict(rf, test_data, type= "prob") # Use the classifier to make the predictions
pred <- prediction(predicted_values[,2], test_data$gen_election)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc


roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="Decision Tree")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))

varImpPlot(rf)

#ANN

install.packages("nnet")
library(nnet)

ann <- nnet(gen_election ~ ., data=train_data, size=5, maxit=1000) 
ann
library(ROCR)
library(ggplot2)
library(class)
library(caret)
predicted_values <- predict(ann, test_data, type= "raw")
head(predicted_values)
threshold <- 0.5

pred <- factor( ifelse(predicted_values[,1] > threshold, 1, 0))
head(pred)
levels(pred)
levels(test_data$gen_election)
confusionMatrix(pred, test_data$gen_election, 
                positive = levels(test_data$gen_election)[2])

predicted_values <- predict(ann, test_data, type= "raw") # Use the classifier to make the predictions
pred <- prediction(predicted_values[,1], test_data$gen_election)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="ANN")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))

varImpPlot(rf) 

#######################################
ann <- nnet(gen_election ~ ., data=train_data, size=10, maxit=1000) 
ann

predicted_values <- predict(ann, test_data, type= "raw")
head(predicted_values)
threshold <- 0.5

pred <- factor( ifelse(predicted_values[,1] > threshold, 1, 0))
head(pred)
levels(pred)
levels(test_data$gen_election)
confusionMatrix(pred, test_data$gen_election, 
                positive = levels(test_data$gen_election)[2])

predicted_values <- predict(ann, test_data, type= "raw") # Use the classifier to make the predictions
pred <- prediction(predicted_values[,1], test_data$gen_election)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="ANN")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))
#############HIDDEN LAYERS 15###############
ann <- nnet(gen_election ~ ., data=train_data, size=15, maxit=1000) 
ann

predicted_values <- predict(ann, test_data, type= "raw")
head(predicted_values)
threshold <- 0.5

pred <- factor( ifelse(predicted_values[,1] > threshold, 1, 0))
head(pred)
levels(pred)
levels(test_data$gen_election)
confusionMatrix(pred, test_data$gen_election, 
                positive = levels(test_data$gen_election)[2])

predicted_values <- predict(ann, test_data, type= "raw") # Use the classifier to make the predictions
pred <- prediction(predicted_values[,1], test_data$gen_election)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="ANN")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))

##################HIDDEN LAYERS 20###########
ann <- nnet(gen_election ~ ., data=train_data, size=20, maxit=1000) 
ann

predicted_values <- predict(ann, test_data, type= "raw")
head(predicted_values)
threshold <- 0.5

pred <- factor( ifelse(predicted_values[,1] > threshold, 1, 0))
head(pred)
levels(pred)
levels(test_data$gen_election)
confusionMatrix(pred, test_data$gen_election, 
                positive = levels(test_data$gen_election)[2])

predicted_values <- predict(ann, test_data, type= "raw") # Use the classifier to make the predictions
pred <- prediction(predicted_values[,1], test_data$gen_election)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="ANN")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))
#################hidden layers 25###########3
ann2 <- nnet(gen_election ~ ., data=train_data, size=24, maxit=1000) 
ann2

predicted_values <- predict(ann2, test_data, type= "raw")
head(predicted_values)
threshold <- 0.5

pred <- factor( ifelse(predicted_values[,1] > threshold, 1, 0))
head(pred)
levels(pred)
levels(test_data$gen_election)
confusionMatrix(pred, test_data$gen_election, 
                positive = levels(test_data$gen_election)[2])

predicted_values <- predict(ann2, test_data, type= "raw") # Use the classifier to make the predictions
pred <- prediction(predicted_values[,1], test_data$gen_election)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="ANN")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))

ftable(xtabs(~facebook+gen_election, data=mydata))
