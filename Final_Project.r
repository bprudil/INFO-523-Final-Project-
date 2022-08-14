library(caret)
library(here)

#Data taken from https://www.kaggle.com/datasets/uciml/adult-census-income?select=adult.csv
adult_data <- read.csv(file = here('Data/adult.csv'))

#Removing columns we don't want part of our model
data <- adult_data[-c(3, 5, 8, 11, 12, 13)]

#Replacing '?' with NA (help from https://stackoverflow.com/questions/28061122/how-do-i-remove-question-mark-from-a-data-set-in-r)
idx <- data == "?"
is.na(data) <- idx

#Transforming the numerical 'age' column into a categorical feature using is. 
AgeGroup <- cut(data$age, breaks = c(0,30,45,65,Inf), labels = c("Early Career","Mid Career", "Late Career", "Retirement"))
data$age <- AgeGroup

#Checking how many NAs are in some columns to see if we can just remove those instances
sum(is.na(data$workclass))
sum(is.na(data$occupation))
sum(is.na(data$native.country))

#Not a huge loss to remove NAs overall, so we remove rows with NA
data1 <- na.omit(data)     

#Checking the data to see if there are any outliers or weird instances 
summary(data1)

#Creating a training (70% of observations) and testing dataset (30%)
set.seed(3)
sample <- sample(c(TRUE, FALSE), nrow(data1), replace=TRUE, prob=c(0.7,0.3))
train <- data1[sample, ]
test <- data1[!sample, ]

#Separate the labels from the attributes
x = train[,-9]
y = train$income

#Create a model trained witht the train set
model <- train(x,y,'nb',trControl=trainControl(method='cv',number=10))
model
confusionMatrix(model)

#Check the efficiency of the model using the test set
Predict <- predict(model,newdata = test) 
confusionMatrix(Predict, as.factor(test$income)) #Get the confusion matrix
