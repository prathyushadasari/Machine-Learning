#Viewing the first few rows of the data
head(Data_Airbnb)
str(Data_Airbnb)

#Removing the predictor Borough which has all NA's
Data_Airbnb$borough <- NULL
View(Data_Airbnb)

#Checking for missing values in Predictors and response variables
sum(is.na(Data_Airbnb))
sum(is.na(Data_Airbnb$room_id))
sum(is.na(Data_Airbnb$host_id))
sum(is.na(Data_Airbnb$room_type))
sum(is.na(Data_Airbnb$neighborhood))
sum(is.na(Data_Airbnb$reviews))
sum(is.na(Data_Airbnb$overall_satisfaction))
sum(is.na(Data_Airbnb$accommodates))
sum(is.na(Data_Airbnb$bedrooms))
sum(is.na(Data_Airbnb$price))
sum(is.na(Data_Airbnb$minstay))
sum(is.na(Data_Airbnb$latitude))
sum(is.na(Data_Airbnb$longitude))
sum(is.na(Data_Airbnb$last_modified))


#Handling missing values in overall_satisfaction and replacing it with mean
Data_Airbnb$overall_satisfaction[is.na(Data_Airbnb$overall_satisfaction)] = mean(Data_Airbnb$overall_satisfaction, na.rm=TRUE)
sum(is.na(Data_Airbnb$overall_satisfaction))

#Handling missing values in bedrooms and replacing it with mean
Data_Airbnb$bedrooms[is.na(Data_Airbnb$bedrooms)] = mean(Data_Airbnb$bedrooms, na.rm=TRUE)
sum(is.na(Data_Airbnb$bedrooms))

#Handling missing values in minstay and replacing it with mean
Data_Airbnb$minstay[is.na(Data_Airbnb$minstay)] = mean(Data_Airbnb$minstay, na.rm=TRUE)
sum(is.na(Data_Airbnb$minstay))

#Handling missing values in last modified and replacing it with mean
Data_Airbnb$last_modified[is.na(Data_Airbnb$last_modified)] = mean(Data_Airbnb$last_modified, na.rm=TRUE)
sum(is.na(Data_Airbnb$last_modified))

set.seed(1234)

#Splitting the data into Training and Test Data
index <- sample(2,nrow(Data_Airbnb),replace = TRUE,prob = c(0.7,0.3))
train_data <- Data_Airbnb[index == 1, ]
test_data <- Data_Airbnb[index == 2, ]

#Model 1 - Linear Regression -- Fitting the model only with selected variables
# Variable Selection
one <- lm(overall_satisfaction~1, data= Data_Airbnb) # Includes only the intercept
all <- lm(overall_satisfaction ~ ., data= Data_Airbnb)
#Forward Selection Method
step(one, scope=list(lower=one, upper=all), direction="forward")
#Backward Selection Method
step(all, scope=list(lower=one, upper=all), direction="backward")

#Linear Regression -> Fitting the model only with selected variables
lin_reg <- lm(overall_satisfaction ~ room_id +reviews + accommodates + bedrooms + price + longitude, data = train_data)
summary(lin_reg)

# To get predicted values
lin_reg$fitted.values

# To get the slope coefficients
lin_reg$coefficients

# To get the residuals
lin_reg$residual

# Histogram of residuals
hist(lin_reg$residuals, main="Histogram of Residuals", xlab = "bf residuals") #The histogram has normal distribution shape

# Analyzing the fit
layout(matrix(c(1,2,3,4),2,2))
plot(lin_reg)

#predicting Linear Fit
pred_lr<-predict(lin_reg,test_data)
#RMSE Value
lr_mse <- mean((pred_lr-test_data$overall_satisfaction)^2)*100
lr_mse
accuracy_lr = 100 - lr_mse 
accuracy_lr


#Model 2 - Random Forest --> Fitted the model only with all predictors except categorical variables
library(randomForest)
rf_fit <- randomForest(overall_satisfaction ~ . -room_type -neighborhood, data = train_data, ntree = 100, proximity = TRUE, replace= TRUE, sampsize = nrow(train_data), importance = TRUE)
print(rf_fit)
plot(rf_fit)

#attributes offered by random forest
attributes(rf_fit)
#Importance of variables
?importance
importance(rf_fit)
#plots the important variables based on the MSEdecrease or Ginidecrease values
varImpPlot(rf_fit)
#Get accuracy of prediction on Test Data
Pred_rf <- predict(rf_fit,newdata = test_data)
Pred_rf

# Test MSE Value
rf_mse = mean((Pred_rf-test_data$overall_satisfaction)^2)*100
rf_mse
accuracy_rf = 100 - rf_mse 
accuracy_rf

#Model 3 - Support Vector Machine Model --> Fitted the model with all predictor variables except categorical variables. 
library(e1071)
library(lubridate)
#MOdel
svm_fit <- svm(overall_satisfaction ~ .-room_type -neighborhood, data = train_data, cost = 100, gamma = 1, kernel = "linear", type = "eps-regression") 
svm_fit

#predicting SVM model
pred_svm<-as.data.frame(predict(svm_fit, newdata=test_data))
summary(svm_fit)
print(svm_fit)

#Install Package
install.packages("hydroGOF")
#Load Library
library(hydroGOF)
#Calculate RMSE 
#RMSE Value
svm_mse <- mean((pred_svm-test_data$overall_satisfaction)^2)*100
svm_mse
accuracy_svm = 100 - svm_mse 
accuracy_svm

