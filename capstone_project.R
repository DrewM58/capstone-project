library(tidyr)
library(dplyr)
library(readr)
library(stringr)

## Load JAM Results .csv

JAM_results <- read_csv("JAM_results.csv")

## Assign ID number to observations

JAM_results <- mutate(JAM_results, id = rownames(JAM_results))

## Move ID number to 1st column

JAM_results <- JAM_results[, c(70, 1:69)]

## Unite columns for each domain of assessment extract first instance of stage being assigned

JAM_results <- JAM_results %>% unite(additive, 5:10, sep = "_", remove = TRUE)
JAM_results <- mutate(JAM_results, additive = str_extract(additive, "S[0-9]" ))

JAM_results <- JAM_results %>% unite(numberID, 6:11, sep = "_", remove = TRUE)
JAM_results <- mutate(JAM_results, numberID = str_extract(numberID, "S[0-9]" ))

JAM_results <- JAM_results %>% unite(forwardSequence, 7:12, sep = "_", remove = TRUE)
JAM_results <- mutate(JAM_results, forwardSequence = str_extract(forwardSequence, "S[0-9]" ))

JAM_results <- JAM_results %>% unite(backwardSequence, 8:13, sep = "_", remove = TRUE)
JAM_results <- mutate(JAM_results, backwardSequence = str_extract(backwardSequence, "S[0-9]" ))

JAM_results <- JAM_results %>% unite(fractions, 9:14, sep = "_", remove = TRUE)
JAM_results <- mutate(JAM_results, fractions = str_extract(fractions, "S[0-9]" ))

JAM_results <- JAM_results %>% unite(placeValue, 10:15, sep = "_", remove = TRUE)
JAM_results <- mutate(JAM_results, placeValue = str_extract(placeValue, "S[0-9]" ))

JAM_results <- JAM_results %>% unite(basicFacts, 11:16, sep = "_", remove = TRUE)
JAM_results <- mutate(JAM_results, basicFacts = str_extract(basicFacts, "S[0-9]" ))

JAM_results <- JAM_results %>% unite(overall, 12:17, sep = "_", remove = TRUE)
JAM_results <- mutate(JAM_results, overall = str_extract(overall, "S[0-9]" ))

JAM_results <- JAM_results %>% unite(multiplicative, 13:18, sep = "_", remove = TRUE)
JAM_results <- mutate(JAM_results, multiplicative = str_extract(multiplicative, "S[0-9]" ))

JAM_results <- JAM_results %>% unite(multiplicative, 14:19, sep = "_", remove = TRUE)
JAM_results <- mutate(JAM_results, multiplicative = str_extract(multiplicative, "S[0-9]" ))

## Remove columns that are not required

JAM_results[c(14:25)] <- list(NULL)

## Rename columns for consistency

colnames(JAM_results) [2:4] <- c("year", "ethnic", "gender")
 