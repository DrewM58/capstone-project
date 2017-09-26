# Load packages

library(readr)
library(dplyr)
library(caTools)

# - Load csv file

Targets <- read_csv("targets_tidy.csv")

str(Targets)

# - Introduce factors for Logistic Regression:
Targets$At <- factor(Targets$At, levels = c(0, 1))
Targets$ethnic <- factor(Targets$ethnic, 
                         levels = c('Other', 'NZMaori', 'NZEuro', 'Pasifika', 'Asian'))
Targets$gender <- factor(Targets$gender, levels = c('Female', 'Male'))
Targets$additive <- factor(Targets$additive, levels = c('S2', 'S4', 'S5'))
Targets$numberID <- factor(Targets$numberID, levels = c('S1', 'S2', 'S4', 'S5'))
Targets$forwardSequence <- factor(Targets$forwardSequence, levels = c('S1', 'S2', 'S4', 'S5'))
Targets$backwardSequence <- factor(Targets$backwardSequence, levels = c('S1', 'S2', 'S4', 'S5'))
Targets$fractions <- factor(Targets$backwardSequence, levels = c('S1', 'S2', 'S3', 'S4', 'S5'))
Targets$placeValue <- factor(Targets$placeValue, levels = c('S1', 'S2', 'S4', 'S5'))
Targets$basicFacts <- factor(Targets$basicFacts, levels = c('S1', 'S2', 'S4', 'S5'))

# - Baseline model: 
# - If we assumed that all students were achieving standard then we would have an accuracy 
# - of 81%. This is based on 119 of the 147 students being judged as 'not'At' or 'Above' standard.

# - Install rpart package

install.packages("rpart")
library(rpart)

install.packages("rpart.plot")
library(rpart.plot)

# - Creat model frame for CART models
CartFrame <- select(Targets, At, ethnic, gender, additive, numberID, forwardSequence, 
                     backwardSequence, fractions, placeValue, basicFacts)

# - Try differnet CART models with minbucket size ranging from 2:10

# - Create CART model minbucket = 2
TreeA <- rpart(At ~ ., 
                     data = CartFrame, 
                     method = "class", 
                     control = rpart.control(minbucket = 2))
summary(TreeA)
# - Plot tree using prp function 
prp(TreeA)
# - See how well model works 
PredictCARTA <- predict(TreeA, newdata = Targets, type = "class")
# - Create confusion matrix 
table(Targets$At, PredictCARTA)
# - Calculate accuracy of model 
(11+116)/(11+17+3+116)
# - Accuracy of model is 86.4%

# - Create CART model minbucket = 3
TreeB<- rpart(At ~ ., 
               data = CartFrame, 
               method = "class", 
               control = rpart.control(minbucket = 3))
summary(TreeB)
# - Plot tree using prp function 
prp(TreeB)
# - See how well model works 
PredictCARTB <- predict(TreeB, newdata = Targets, type = "class")
# - Create confusion matrix 
table(Targets$At, PredictCARTB)
# - Calculate accuracy of model 
(16+112)/(16+12+7+112)
# - Accuracy of model is 87.1%

# - Create CART model minbucket = 4
TreeC <- rpart(At ~ ., 
               data = CartFrame, 
               method = "class", 
               control = rpart.control(minbucket = 4))
summary(TreeC)
# - Plot tree using prp function 
prp(TreeC)
# - See how well model works 
PredictCARTC <- predict(TreeC, newdata = Targets, type = "class")
# - Create confusion matrix 
table(Targets$At, PredictCARTC)
# - Calculate accuracy of model 
(11+116)/(11+17+3+116)
# - Accuracy of model is 86.4%

# - Create CART model minbucket = 5
TreeD <- rpart(At ~ ., 
               data = CartFrame, 
               method = "class", 
               control = rpart.control(minbucket = 5))
summary(TreeD)
# - Plot tree using prp function 
prp(TreeD)
# - See how well model works 
PredictCARTD <- predict(TreeD, newdata = Targets, type = "class")
# - Create confusion matrix 
table(Targets$At, PredictCARTD)
# - Calculate accuracy of model 
(12+114)/(12+16+5+114)
# - Accuracy of model is 85.7%

# - Create CART model minbucket = 6
TreeE <- rpart(At ~ ., 
               data = CartFrame, 
               method = "class", 
               control = rpart.control(minbucket = 6))
summary(TreeE)
# - Plot tree using prp function 
prp(TreeE)
# - See how well model works 
PredictCARTE <- predict(TreeE, newdata = Targets, type = "class")
# - Create confusion matrix 
table(Targets$At, PredictCARTE)
# - Calculate accuracy of model 
(14+111)/(14+14+8+111)
# - Accuracy of model is 85%

# - Create CART model minbucket = 7
TreeF <- rpart(At ~ ., 
               data = CartFrame, 
               method = "class", 
               control = rpart.control(minbucket = 7))
summary(TreeF)
# - Plot tree using prp function 
prp(TreeF)
# - See how well model works 
PredictCARTF <- predict(TreeF, newdata = Targets, type = "class")
# - Create confusion matrix 
table(Targets$At, PredictCARTF)
# - Calculate accuracy of model 
(7+116)/(7+21+3+116)
# - Accuracy of model is 83.7%

# - Create CART model minbucket = 8
TreeG<- rpart(At ~ ., 
               data = CartFrame, 
               method = "class", 
               control = rpart.control(minbucket = 8))
summary(TreeG)
# - Plot tree using prp function 
prp(TreeG)
# - See how well model works 
PredictCARTG <- predict(TreeG, newdata = Targets, type = "class")
# - Create confusion matrix 
table(Targets$At, PredictCARTG)
# - Calculate accuracy of model 
(7+116)/(7+21+3+116)
# - Accuracy of model is 83.7%

# - Create CART model minbucket = 9
TreeH <- rpart(At ~ ., 
               data = CartFrame, 
               method = "class", 
               control = rpart.control(minbucket = 9))
summary(TreeH)
# - Plot tree using prp function 
prp(TreeH)
# - See how well model works 
PredictCARTH <- predict(TreeH, newdata = Targets, type = "class")
# - Create confusion matrix 
table(Targets$At, PredictCARTH)
# - Calculate accuracy of model 
(7+116)/(7+21+3+116)
# - Accuracy of model is 83.7%

# - Create CART model minbucket = 10
TreeI<- rpart(At ~ ., 
               data = CartFrame, 
               method = "class", 
               control = rpart.control(minbucket = 10))
summary(TreeI)
# - Plot tree using prp function 
prp(TreeI)
# - See how well model works 
PredictCARTI <- predict(TreeI, newdata = Targets, type = "class")
# - Create confusion matrix 
table(Targets$At, PredictCARTI)
# - Calculate accuracy of model 
(7+116)/(7+21+3+116)
# - Accuracy of model is 83.7%


# - CART Model with (minbucket = 3) had accuracy rate of 87.1%
# - CART Model with (minbucket = 4) had accuracy rate of 86.4%




# - Question
# - Below is the process I went through in the CART excercises on Springboard. I wondered if  
# - this process or something similar was necessary for my model? Is it possible to use the train
# - function to find the optimal minbucket size?

# Install new packages for cross validation of CART models
install.packages("caret")
library(caret)

install.packages("e1071")
library(e1071)

fitControl <- trainControl(method = "cv", number = 10)
cartGrid <- expand.grid(.cp = (1:50)*0.01)

train(At ~ ., 
      data = Targets,
      method = "rpart", 
      trControl = fitControl, tuneGrid = cartGrid)









