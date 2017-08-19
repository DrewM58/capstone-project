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

JAM_results <- JAM_results %>% unite(multiplicative, 13:19, sep = "_", remove = TRUE)
JAM_results <- mutate(JAM_results, multiplicative = str_extract(multiplicative, "S[0-9]" ))



## Remove columns that are not required

JAM_results[c(14:25)] <- list(NULL)

## Rename columns for consistency

colnames(JAM_results) [2:4] <- c("year", "ethnic", "gender")

## Change "year" variable to single digit

JAM_results$year <- gsub(".Y(\\d).", "\\1", JAM_results$year)

## Tidy gender column

JAM_results$gender <- gsub("'Male'", "Male", JAM_results$gender)
JAM_results$gender <- gsub("'Female'", "Female", JAM_results$gender)


## Load OTJ Results .csv
OTJS <- read_csv("OTJ.csv")

## Remove columns not required
OTJS[c(1:8)] <- list(NULL)

## Unite columns from each year, so all judgements are in 1 column

OTJS <- OTJS %>% unite(OTJ, 1:4, sep = "_", remove = TRUE)
OTJS <- mutate(OTJS, OTJ = str_extract(OTJ, "[[:alpha:]].*[[:alpha:]]" ))

## Bind OTJS to JAM_results

JAM_results <- bind_cols(JAM_results, OTJS)

## Filter only Year 4-6 Students

JAM_results <- JAM_results %>% filter(as.numeric(year) >= 4)

## Remove observations with no JAM results
JAM_results <- JAM_results %>% filter(!is.na(additive))


## Change variables to factors
JAM_results$gender <- as.factor(JAM_results$gender)
JAM_results$year <- as.factor(JAM_results$year)
JAM_results$additive <- as.factor(JAM_results$additive)
JAM_results$numberID <- as.factor(JAM_results$numberID)
JAM_results$forwardSequence <- as.factor(JAM_results$forwardSequence)
JAM_results$backwardSequence <- as.factor(JAM_results$backwardSequence)
JAM_results$fractions <- as.factor(JAM_results$fractions)
JAM_results$placeValue <- as.factor(JAM_results$placeValue)
JAM_results$basicFacts <- as.factor(JAM_results$basicFacts)
JAM_results$multiplicative <- as.factor(JAM_results$multiplicative)

## Remove Overall column as this will not be required

JAM_results$overall <- NULL

## Remove Multiplicative column as there are too many NA 

JAM_results$multiplicative <- NULL

## Tidy-up ethnic column to be factor with levels "NZEuro" "NZMaori" "Pasific" "Asian" "Other"
table(JAM_results$ethnic)

JAM_results$ethnic <- gsub(".NZ European.", "NZEuro", JAM_results$ethnic)
JAM_results$ethnic <- gsub(".NZ Maori.", "NZMaori", JAM_results$ethnic)
JAM_results$ethnic <- gsub(".Fijian.|.Samoan.|.Other Pacific Isl Group.|.Tongan.|.Cook Isl Maori.", "Pasifika", JAM_results$ethnic)
JAM_results$ethnic <- gsub(".Japanese.|.Korean.|.Vietnamese.|.Other South East Asian.|.Other Asian.", "Asian", JAM_results$ethnic)
JAM_results$ethnic <- gsub(".Australian.|.Other European.|.British / Irish.|.German.|.Indian.|.Middle East.", "Other", JAM_results$ethnic)

JAM_results$ethnic <- as.factor(JAM_results$ethnic)

## Make OTJ factor
JAM_results$OTJ <- as.factor(JAM_results$OTJ)


##
plot(basicFacts, OTJ)

## Save cvs file

write_csv(JAM_results, "JAM_results_tidy.csv")
 