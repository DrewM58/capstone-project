# - Load Packages
library(dplyr)
library(ggplot2)
library(miscset) 

# - Import Dataset
JAM_results <- read_csv("targets_tidy.csv")
glimpse(JAM_results)

# - There are 147 obeservations in this data set. Each observation is 1 student, 
#   with their JAM assessment results and their OTJ from after 3 years at school. 

# - Calculate number of students who have been judged as achieving below the National Standard.
JAM_results %>% count(At)
JAM_results %>% summarise(At = mean(At)) %>% mutate(Below = 1 - At) 
# - 28 (19%) students are achieving 'Below' or 'Well Below', 119 (81%) are achieving 'At' or 'Above'.

# - Counting the size of each ethnic group
JAM_results %>% count(ethnic, sort = TRUE)

# - Percentage of ethnic groups achieving 'At' or 'Above' National Standard
JAM_results %>% group_by(ethnic) %>% 
                    summarise(At = mean(At)) %>% mutate(Below = 1 - At)

# - Plotting gender achievement
ggplot(JAM_results, aes(x = ethnic)) +
  geom_bar(aes(fill = factor(At)), position = "fill") +
  labs(title = "Achievement by Ethnicity", x = "Ethnicity", y = "Percentage", fill = "At Standard") +
  scale_fill_manual(labels = c("Below", "At"), values = c("red3", "blue3")) 
  
# - Counting the size of each gender group
JAM_results %>% count(gender, sort = TRUE)

# - Percentage of gender groups achieving 'At' or 'Above' National Standard
JAM_results %>% group_by(gender) %>% 
  summarise(At = mean(At)) %>% mutate(Below = 1 - At)

# - Plotting gender achievement
ggplot(JAM_results, aes(x = gender)) +
  geom_bar(aes(fill = factor(At)), position = "fill") +
  labs(title = "Achievement by Gender", x = "Gender", y = "Percentage", fill = "At Standard") +
  scale_fill_manual(labels = c("Below", "At"), values = c("red3", "blue3")) 

# - Percentage of stage groups achieving 'At' or 'Above' National Standard in additive domain
JAM_results %>% group_by(additive) %>% 
  summarise(At = mean(At)) %>% mutate(Below = 1 - At)

# - Percentage of stage groups achieving 'At' or 'Above' National Standard in numberID domain
JAM_results %>% group_by(numberID) %>% 
  summarise(At = mean(At)) %>% mutate(Below = 1 - At)

# - Percentage of stage groups achieving 'At' or 'Above' National Standard in forwardSequence domain
JAM_results %>% group_by(forwardSequence) %>% 
  summarise(At = mean(At)) %>% mutate(Below = 1 - At)

# - Percentage of stage groups achieving 'At' or 'Above' National Standard in backwardSequence domain
JAM_results %>% group_by(backwardSequence) %>% 
  summarise(At = mean(At)) %>% mutate(Below = 1 - At)

# - Percentage of stage groups achieving 'At' or 'Above' National Standard in fractions domain
JAM_results %>% group_by(fractions) %>% 
  summarise(At = mean(At)) %>% mutate(Below = 1 - At)

# - Percentage of stage groups achieving 'At' or 'Above' National Standard in placeValue domain
JAM_results %>% group_by(placeValue) %>% 
  summarise(At = mean(At)) %>% mutate(Below = 1 - At)

# - Percentage of stage groups achieving 'At' or 'Above' National Standard in basicFacts domain
JAM_results %>% group_by(basicFacts) %>% 
  summarise(At = mean(At)) %>% mutate(Below = 1 - At)

# - Combined plots to show each domain of the JAM assessment. 
# - Each plot showing the count for the number of students at each stage within in each domain
ggplotGrid(ncol = 2,
           lapply(c("additive", "numberID", "forwardSequence", "backwardSequence", "fractions", 
                    "placeValue", "basicFacts"),
                  function(col) {
                    ggplot(JAM_results, aes_string(col)) + 
                      geom_bar() 
                  }))


# - Each plot showing the percentage of students at each stage of that domain who are 
#   judged 'At' or 'Below' the standard. 
ggplotGrid(ncol = 2,
           lapply(c("additive", "numberID", "forwardSequence", "backwardSequence", "fractions", 
                    "placeValue", "basicFacts"),
                  function(col) {
                    ggplot(JAM_results, aes_string(col)) + 
                      geom_bar(aes(fill = factor(At)), position = "fill") +
                      labs(y = "Percentage") +
                      coord_flip()
                  }))



# - Faceted scatter plots to show three variables and colour to show 'At'.

# - Additive and NumberID domains in facet grid by Place Value 
ggplot(JAM_results, aes(additive, numberID, col = factor(At))) +
  geom_point(position = "jitter") +
  labs(title = "Additive and Number ID", col = "At Standard?") +
  facet_grid(.~placeValue)

# - Forward Sequence and Number ID in facet grid by Place Value 
ggplot(JAM_results, aes(forwardSequence, numberID, col = factor(At))) +
  geom_point(position = "jitter") +
  labs(title = "Forward Sequence and Number ID by Place Value", col = "At Standard?") +
  facet_grid(.~placeValue)

# - Basic Facts and Number ID in facet grid by Place Value 
ggplot(JAM_results, aes(basicFacts, numberID, col = factor(At))) +
  geom_point(position = "jitter") +
  labs(title = "Basic Facts and Number ID by Place Value", col = "At Standard?") +
  facet_grid(.~placeValue)


