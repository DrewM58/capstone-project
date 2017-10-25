# Load packages
library(readr)
library(dplyr)
library(caTools)
library(rpart)
library(rpart.plot)
library(data.table)
library(ggplot2)

# - Load csv file
Targets <- read_csv("targets_tidy.csv")
str(Targets)

# - Introduce factors:
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
# - of 81%. This is based on 119 of the 147 students being judged as 'At' or 'Above' standard.

# - Creat model frame for CART models
CartFrame <- select(Targets, At, ethnic, gender, additive, numberID, forwardSequence, 
                     backwardSequence, fractions, placeValue, basicFacts)


# - Create function to take random sample of 100 random numbers between 1:147  
samples <- listfunction(x) {
  list(sample(1:147, 100, replace = FALSE))
}

# - Use lapply with 'samples' function to create list with 100 items, each with 100 
# - random numbers between 1:147 to create training and test splits for Monte Carlo CV.
splits <- lapply(1:100, samples)
str(splits)


# - Create function to take a list of 100 random numbers (1:147) and split data into train and test sets.
# - Run CART model 19 times with each split, each time using different minbucket size between 2:20.
# - Function will return a list of 100 items, each is list of 19 model accuracy results from running
# - CART model with a diferent minbucket size.

cvfunction <- function(y) {
  trainset <- CartFrame[splits[[y]], ]
  testset <- CartFrame[-splits[[y]], ]
  bucketsize <- c(2:20) # - Create vector of range of minbucket sizes to try

  buckettest <- function(x) {
          Tree <- rpart(At ~ ., 
                  data = trainset, 
                  method = "class", 
                  control = rpart.control(minbucket = x))
    PredictCART <- predict(Tree, newdata = testset, type = "class")
    accuracyTable <- table(testset$At, PredictCART)
    modelAccuracy <- (accuracyTable[1,1] + accuracyTable[2,2]) / sum(accuracyTable)
    return(modelAccuracy)
  }
    return(lapply(bucketsize, buckettest))
}


#- Run cvfunction with lapply 
cvfunctionresults <- lapply(seq_along(splits), cvfunction)

# - Unlist cvfunctionresults items
cvfunctionresults <- lapply(cvfunctionresults, unlist)

# - Create tbl_df from results
CVresultsDF <- tbl_df(t(sapply(cvfunctionresults,c)))

# - Change column names to minbucket size
colnames(CVresultsDF) <- c("2", "3", "4", "5", "6", "7", "8", "9", "10", 
                           "11", "12", "13", "14", "15", "16", "17", "18", 
                           "19", "20")

# - Find mean of model accuracy for each minbucket size.
minbucketMean <- t(summarise_all(CVresultsDF, funs(mean)))
minbucketMean <- tbl_df(minbucketMean)
minbucketMean$bucketsize <- 2:20
colnames(minbucketMean) <- c("mean_model_accuracy", "bucket_size")

# - Plot minbucketMean
ggplot(minbucketMean, aes(bucket_size, mean_model_accuracy)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean CART Model Accuracy with Range of bucketsize ", 
       x = "Bucket Size", y = "Mean Model Accuracy") +
  coord_cartesian(ylim = c(0.75, 0.85)) 

# Models with minbucket of size 20 have the largest mean model accuracy. 

# - Run CART model with minbucket size of 20.  
CARTmodel <- rpart(At ~ ., 
              data = CartFrame, 
              method = "class", 
              control = rpart.control(minbucket = 20))

# - Plot tree using prp function 
prp(CARTmodel)

# - See how well model works on test set
PredictCART <- predict(CARTmodel, newdata = Targets, type = "class")

# - Create confusion matrix 
table(Targets$At, PredictCART)

# - Calculate accuracy of model 
(16+105)/(16+105+12+14)

#- Model accuracy is 82%. A very small improvement on the baseline. 

# - Saving objects for markdown

save(CARTmodel, minbucketMean, file = "cartmodel.RData")

