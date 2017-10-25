# - Load Required Packages
library(readr)
library(dplyr)
library(ggplot2)

# - Load Data
Targets <- read_csv("targets_tidy.csv")

# - Inspect Data
str(Targets)
# - Baseline model: 
# - If we assumed that all students were achieving standard then we would have an accuracy 
# - of 81%. This is based on 119 of the 147 students being judged as not below standard.

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


# - Binary Logistic Regression
ModelFrameA <-select(Targets, At, year, ethnic, gender, additive, numberID, forwardSequence, 
                backwardSequence, fractions, placeValue, basicFacts)

ModelA <- glm(At ~ ., 
              family = binomial, 
              data = ModelFrameA)

# - Warning message:
# - glm.fit: fitted probabilities numerically 0 or 1 occurred 
# - In conclusion: perfect separation

# - ModelA1: 
modelFrameA1 <- select(Targets,
                       At, 
                       gender)

# - Binary Logistic Regression:
ModelA1 <- glm(At ~ ., 
               family = binomial, 
               data = modelFrameA1)
summary(ModelA1)
# - null effect...

# - ModelA2: 
modelFrameA2 <- select(Targets,
                       At,
                       gender, ethnic)
# - Binary Logistic Regression:
ModelA2 <- glm(At ~ ., 
               family = binomial, 
               data = modelFrameA2)
summary(ModelA2)
# - negative effect: ethnicPasifika

# - ModelA3: 
modelFrameA3 <- select(Targets,
                       At,
                       gender, ethnic,
                       additive)
# - Binary Logistic Regression:
ModelA3 <- glm(At ~ ., 
               family = binomial, 
               data = modelFrameA3)
summary(ModelA3)
# - positive effect: additive

# - ModelA4: 
modelFrameA4 <- select(Targets,
                       At,
                       gender, ethnic,
                       additive, numberID)
# - Binary Logistic Regression:
ModelA4 <- glm(At ~ ., 
               family = binomial, 
               data = modelFrameA4)
summary(ModelA4)
# - some effect: additive, no effect: numberID, ethnic

# - ModelA5: 
modelFrameA5 <- select(Targets,
                       At,
                       gender, ethnic,
                       additive, forwardSequence)
# - Binary Logistic Regression:
ModelA5 <- glm(At ~ ., 
               family = binomial, 
               data = modelFrameA5)
summary(ModelA5)
# - some effect: forwardSequence, no effect: additive, negative effect: ethnicPasifika

# - ModelA6: 
modelFrameA6 <- select(Targets,
                       At,
                       gender, ethnic,
                       additive, backwardSequence)
# - Binary Logistic Regression:
ModelA6 <- glm(At ~ ., 
               family = binomial, 
               data = modelFrameA6)
summary(ModelA6)
# - some effect: additive, backwardSequence, negative effect: ethnicPasifika

# - ModelA7: 
modelFrameA7 <- select(Targets,
                       At,
                       gender, ethnic,
                       additive, forwardSequence, backwardSequence)
# - Binary Logistic Regression:
ModelA7 <- glm(At ~ ., 
               family = binomial, 
               data = modelFrameA7)
summary(ModelA7)
# - some effect: forwardSequenceS4, some negative: ethnicPasifika, no effect: additive, backwardSequence

# - ModelA8: 
modelFrameA8 <- select(Targets,
                       At,
                       gender, ethnic,
                       forwardSequence, backwardSequence)
# - Binary Logistic Regression:
ModelA8 <- glm(At ~ ., 
               family = binomial, 
               data = modelFrameA8)
summary(ModelA8)
# - some effect: forwardSequence, some negative effect: ethnicPasifika, no effect: backwardSequence

ModelFrameA9 <- select(Targets, 
                       At, 
                       gender, ethnic, 
                       forwardSequence, placeValue)
# - Binary Logistic Regression:
ModelA9 <- glm(At ~.,
               family = binomial, 
               data = ModelFrameA9)
# - Warning message:
# - glm.fit: fitted probabilities numerically 0 or 1 occurred 
# - Perfect Separation!

ModelFrameA10 <- select(Targets, 
                        At, 
                        gender, ethnic, 
                        forwardSequence, basicFacts)
# - Binary Logistic Regression:
ModelA10 <- glm(At ~., 
                family = binomial, 
                data = ModelFrameA10)
# - Warning message:
# - glm.fit: fitted probabilities numerically 0 or 1 occurred 
# - Perfect Separation!

# - Try removing ethnic and gender variables:

ModelFrameB1 <- select(Targets, 
                       At,
                       additive)
# - Binary Logistic Regression:
ModelB1 <- glm(At ~.,
               family = binomial, 
               data = ModelFrameB1)
summary(ModelB1)
# - some effect: additiveS4 *, additiveS5 *

ModelFrameB2 <- select(Targets, 
                       At, 
                       forwardSequence)
# - Binary Logistic Regression
ModelB2 <- glm(At~., 
               family = binomial,
               data = ModelFrameB2)
summary(ModelB2)
# - more signigicant effect: forwardSequenceS4 **, forwardSequenceS5 **

ModelFrameB3 <- select(Targets, 
                       At, 
                       backwardSequence)
# - Binary Logistic Regression:
ModelB3 <- glm(At ~., 
               family = binomial, 
               data = ModelFrameB3)
summary(ModelB3)
# - some effect: backwardSequenceS4 **, backwardSequenceS5 **

ModelFrameB4 <- select(Targets, 
                       At, 
                       placeValue)
# - Binary Logistic Regression:
ModelB4 <- glm(At ~., 
               family = binomial, 
               data = ModelFrameB4)
summary(ModelB4)
# - some effect: placeValueS4 **

ModelFrameB5 <- select(Targets, 
                       At, 
                       fractions)
# - Binary Logistic Regression:
ModelB5 <- glm(At ~., 
               family = binomial, 
               data = ModelFrameB5)
summary(ModelB5)
# - some effect: fractionsS4 **, fractionsS5 **

ModelFrameB6 <- select(Targets, 
                       At, 
                       basicFacts)
# - Binary Logistic Regression:
ModelB6 <- glm(At ~., 
               family = binomial, 
               data = ModelFrameB6)
summary(ModelB6)
# - some effect: basicFactsS2 ***, basicFactsS4 **

ModelFrameB7 <- select(Targets, 
                       At, 
                       fractions)

# - Binary Logistic Regression:
ModelB7 <- glm(At ~., 
               family = binomial, 
               data = ModelFrameB7)
summary(ModelB7)
# - some effect: fractionsS4 **, fractionsS5 **

# - ModelB8: basicFacts 
modelFrameB8 <- select(modelFrame,
                       At,
                       basicFacts)
# - Binary Logistic Regression:
ModelB8 <- glm(At ~ ., 
               family = binomial, 
               data = modelFrameB8)
summary(ModelB8)

# - ModelB2: placeValue 
modelFrameB9 <- select(modelFrame,
                       At,
                       placeValue)
# - Binary Logistic Regression:
ModelB9 <- glm(At ~ ., 
               family = binomial, 
               data = modelFrameB9)
# - perfect separation(!)
summary(ModelB2)

# - It seems like basicFacts is best predictor.



