setwd("C:/Users/pragn/Desktop/ABA")
#import data
mydata <- read.table("salary_data.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))
nrow(mydata)
summary(mydata)
mydata$fnlwgt <- NULL
mydata$education.num <- NULL
mydata$relationship <- NULL
mydata$salary.class <- ifelse(mydata$salary.class == "<=50K", 0, 1)
## Note: Since categorical variables enter into statistical models differently than continuous variables, storing data as factors insures that the modeling functions will treat such data correctly:
mydata$education <- as.factor(mydata$education)
mydata$martital.status <- as.factor(mydata$martital.status)
mydata$occupation <- as.factor(mydata$occupation)
mydata$race <- as.factor(mydata$race)
mydata$sex <- as.factor(mydata$sex)
mydata$native.country <- as.factor(mydata$native.country)
mydata$salary.class <- as.factor(mydata$salary.class)
summary(mydata)

# Install packages required for random forest:
install.packages("nnet")
# Load packages required for random forest:
library(nnet)

set.seed(32) 
# Since the data is large, we sample the first 5k observations:
train_data <- head(mydata, n = 5000)
train_data <- train_data[complete.cases(train_data),] # We only keep the observations with no missing values. 
train_data$salary.class <- as.factor(train_data$salary.class) # Make sure that the target (salary.class) is a factor vairable.
summary(train_data)

ann <- nnet(salary.class ~ ., data=train_data, size=10, maxit=1000) # Size is the number of units (nodes) in the hidden layer.
summary(ann)
print(ann)


test_data = tail(mydata, n = 1000)
test_data <- test_data[complete.cases(test_data),]
predicted_values <- predict(ann, test_data,type= "raw")
predicted_values
final_data <- cbind(test_data, predicted_values)
colnames <- c(colnames(test_data),"prob.one")
final_data
