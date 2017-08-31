
Targets <- read_csv("targets_tidy.csv")

## Baseline model: 
## If we assumed that all students were achieving standard then we would have an accuracy 
## of 81%. This is based on 119 of the 147 students being judged as not below standard.


Model1 <- glm(Below ~ additive + numberID + placeValue + forwardSequence + backwardSequence, family = binomial, data = Targets)

summary(Model1)

## Model summary indicates that forwardSequence is the only variable of significance

Model2 <- glm(Below ~ additive + numberID + placeValue + forwardSequence, family = binomial, data = Targets)

summary(Model2)

## Model summary again indicates that forwardSequence is the only variable of significance

Model3 <- glm(Below ~ additive + numberID + forwardSequence, family = binomial, data = Targets)

summary(Model3)


Model4 <- glm(Below ~ additive + forwardSequence, family = binomial, data = Targets)

summary(Model4)



Model5 <- glm(Below ~ forwardSequence, family = binomial, data = Targets)

summary(Model5)



Model6 <- glm(Below ~ numberID + forwardSequence, family = binomial, data = Targets)
summary(Model6)


Model7 <- glm(Below ~ forwardSequence + backwardSequence, family = binomial, data = Targets)
summary(Model7)


Model8 <- glm(Below ~ forwardSequence + fractions, family = binomial, data = Targets)
summary(Model8)


Model9 <- glm(Below ~ forwardSequence + placeValue, family = binomial, data = Targets)
summary(Model9)


Model10 <- glm(Below ~ forwardSequence + basicFacts, family = binomial, data = Targets)
summary(Model10)


## Attempts to visualise some of the data:

ggplot(Targets, aes(Below, forwardSequence, col = placeValue)) +
  geom_jitter()

ggplot(Targets, aes(Below, forwardSequence, col = basicFacts)) +
  geom_jitter()
