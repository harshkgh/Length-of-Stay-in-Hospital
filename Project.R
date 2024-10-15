#####################################
# MIE 622 
# Project  (Team 3)
# Harsh Panchal, Vaishnav Kaldate, Yash Javalkar
# Topics: Hospital Length of Stay
#####################################

library(dplyr)
library(tibble)
library(DescTools)
library(leaps)

library(dplyr)       # for data wrangling
library(DescTools)   # for descriptive statistics and visualization
library(rpart)       # for decision tree application
library(caret)       # for decision tree application and feature importance plot
library(rpart.plot)  # for plotting decision trees
library(vip)         # for feature importance plot
library(ISLR2)       # for accessing Carseats data
library(rattle)
library(randomForest)

# Set the working directory and read LengthOfStay dataset
LengthOfStay <- read.csv("LengthOfStay.csv", as.is=T)
str(LengthOfStay)

#LengthOfStay <- subset( LengthOfStay, select = -X )


# Know the Variables in the dataset
colnames(LengthOfStay)


# Factorize categorical variables
cols <- c("gender","dialysisrenalendstage", "asthma", "irondef","pneum", "substancedependence", "psychologicaldisordermajor", "depress", "psychother", "fibrosisandother", "malnutrition","hemo","facid")
LengthOfStay[  , cols] <- lapply(LengthOfStay[  , cols], factor)
str(LengthOfStay)

head(LengthOfStay)
# Check if data has any rows with missing/NA values
PlotMiss(LengthOfStay)
any(is.na(LengthOfStay))

# split hflights data into 70% training and 30% testing
set.seed(123) 

index <- sample(1:nrow(LengthOfStay), round(nrow(LengthOfStay)*0.7))
train <- LengthOfStay[index,     ]
test  <- LengthOfStay[-index,     ]



model <- lm(lengthofstay ~ ., data = train)
summary(model)


# develop an MLR model
model1 <- lm(lengthofstay ~ rcount+ gender + asthma + dialysisrenalendstage + pneum  + psychologicaldisordermajor +substancedependence
             + depress+malnutrition+psychother, data = train)
summary(model1)

vip(model)

model2 <- lm(lengthofstay ~  hematocrit + neutrophils+ sodium+ glucose+bloodureanitro+creatinine
             +bmi+pulse+respiration+secondarydiagnosisnonicd9+fibrosisandother+hemo, data = train)
summary(model2)


# signifient variables from model 2
model3 <- lm(lengthofstay ~  hematocrit + neutrophils+bloodureanitro+
            +secondarydiagnosisnonicd9+fibrosisandother+hemo, data = train)
summary(model3)

# MSE (mean squared error)
sigma(model1)^2

# RMSE (Root mean squared error)
sqrt(sigma(model1)^2)

# calculating RMSE

sqrt(0.6518)
# 0.8073413

sqrt(0.6777882)
# 0.8232789

plot(model1)

plot (model2)




# logistic regression model to predict Depression of the patient
lr_model1 <- glm(depress ~ ., data = train,family =binomial(link = "logit")) 
summary(lr_model1)


lr_model1 <- glm(depress ~ hematocrit + neutrophils+ sodium+ glucose+bloodureanitro+creatinine
                 +bmi+pulse+respiration+secondarydiagnosisnonicd9+fibrosisandother+hemo, data = train,family =binomial(link = "logit")) 
summary(lr_model1)


log_odds_W = -5.821e+00 -2.331e-01*0 -7.644e-01*0 -5.143e-02*0 + 1.619e-01*0 -3.106e-01*0 -7.678e-02*0  -9.876e-01*1 + 4.520e-01*0 -1.120e+00*0 +2.157e-01*0 -3.439e-02*0 + 1.576e-01*0 + 8.434e-02*11.9 -3.527e-02*9.4 -4.626e-03*135.980274
5.197e-05*100.2831995 -3.171e-03*12 -3.545e-02*1.178786278 +7.085e-04*28.80268173 +3.565e-03*69 +4.294e-02*6.5 -7.531e-05*1 +9.827e-02*0 + 9.627e-01*0 + 6.697e-01*0 +2.779e+00*1 + 2.466e-01*3
exp(log_odds_W)
exp(log_odds_W)/(1+exp(log_odds_W))





pr_lr_Testing <- predict(lr_model1, type="response",newdata = train)
pr_lr_coded <- ifelse(pr_lr_Testing > 0.5, 1, 0)
matrix <- table(train$depress,pr_lr_coded)

precision <- matrix[2, 2] / sum(matrix[, 2])
recall <- matrix[2, 2] / sum(matrix[2, ])


# Best subset selection on trainig dataset
model.bestsubset <- regsubsets(lengthofstay ~ .,data=train, nvmax = 25)
summary(model.bestsubset)

#adjusted R-squared for best model

sm.bestsubset <- summary(model.bestsubset)
names(sm.bestsubset)
plot(sm.bestsubset$adjr2, xlab = " Number of Variables ", ylab = " Adjusted R-squared", type = "b",col="purple")

coef(model.bestsubset, 11)

#forward and backward selection methods
model.fwd <- regsubsets(lengthofstay~ ., data=train, nvmax = 25, method = "forward")
summary(model.fwd)
sm.fwd <- summary(model.fwd)
plot(sm.fwd$adjr2, xlab = " Number of Variables ", ylab = " Adjusted R-squared", type = "b",col="red")

model.bwd <- regsubsets(lengthofstay~ ., data=train, nvmax = 25, method = "backward")
summary(model.bwd)
sm.bwd <- summary(model.bwd)
plot(sm.bwd$adjr2, xlab = " Number of Variables ", ylab = " Adjusted R-squared", type = "b",col="green")


coef(model.bestsubset,7)
coef(model.fwd,7)
coef(model.bwd,7)



modelysj <- lm(lengthofstay ~ rcount+ dialysisrenalendstage + substancedependence + psychologicaldisordermajor + psychother+hemo+neutrophils+bloodureanitro+facid, data = train)
summary(modelysj)




treemodel1 <- rpart(formula = lengthofstay ~ ., data=train, method="anova",control=list(cp = 0.012, xval = 10))
rpart.plot(treemodel1)

set.seed(123)
forestmodel1 <- randomForest(lengthofstay ~  ., data=train, mtry = 3,ntree = 50, importance=TRUE)
forestmodel1

str(train)

set.seed(123)
forestmodel1 <- randomForest(lengthofstay ~  ., data=test, mtry = 3,ntree = 50, importance=TRUE)
forestmodel1

str(test)

