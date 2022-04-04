library(ggplot2)
library(dplyr)
#
#_______________________________________________________
# Make 3 datasets, one for P1NP Collagen, one for the Resting Metabolic Rate, and one for
# the T3 hormone.
# The data from these datasets was taken from an older project of mine, hence why
# I do not have any reading of csv files in this script specifically
P1NPFuture = subset(P1NPEveryoneFinal, select = c(deidnum, tx, P1NPEveryoneMeasure))

P1NPFuture$BMI <- ifelse (P1NPEveryoneFinal$BinBMI == 0,"Lower BMI","Higher BMI")
P1NPFuture$Gender <- ifelse (P1NPEveryoneFinal$BinGender == 0,"Male","Female")

RMRFuture = subset(RMREveryoneFinal, select = c(deidnum,RMREveryoneMeasure))
T3Future = subset(T3EveryoneFinal, select = c(deidnum,T3EveryoneMeasure))

#Inner join all 3 datasets into one by using the deidnum variable which IDs participants                        
DietFuture = inner_join(P1NPFuture,RMRFuture, by = "deidnum")
DietFuture = inner_join(DietFuture, T3Future, by = "deidnum")

#After joining, remove the deidnum
DietFuture = subset(DietFuture, select = -c(deidnum))

#
#________________________________________________
# Loop and gather data of naivebayes and logit regression:
endVal = 500
bayesCorrectVector = 1:endVal; bayesCorrectVector
logitCorrectVector = 1:endVal; logitCorrectVector
for(iterator in 1:endVal){
  print(iterator)
#Get test set for the data
  indices <- sample(2, nrow(DietFuture), replace = T, prob = c(0.8, 0.2))
  indices
  train <- DietFuture[indices == 1,]
  testing <- DietFuture[indices == 2,]
  train
  testing
#_____________
#Naive Bayes
  library(naivebayes)
  
  head(DietFuture)
  
  model <- naive_bayes(tx ~ ., laplace = 1, data = train, usekernel = T)
#  plot(model)
  
  p<- predict(model, testing)
  naiveTable <- table(p, testing$tx)
  naiveNumerator <- ((naiveTable[1]) + (naiveTable[4]))
  naiveDenom <- ((naiveTable[3]) + (naiveTable[2]))
  naivePercentCorrect <- naiveNumerator/(naiveNumerator+naiveDenom)

#______________
#Logit Regression

  glm.fit <- glm (as.factor(tx)~ P1NPEveryoneMeasure + T3EveryoneMeasure + RMREveryoneMeasure + BMI + Gender, 
                  data = train, family = binomial)
  glm.fit
  summary(glm.fit)
  
  glm.probs <- predict(glm.fit, testing, type = "response")
  glm.probs[1:5]
  
  glm.pred <- ifelse(glm.probs > 0.5, "Control", "Caloric Restriction")

  logitTable <- table(glm.pred, testing$tx)
  logitNumerator <- ((logitTable[1]) + (logitTable[4]))
  logitDenom <- ((logitTable[3]) + (logitTable[2]))
  logitPercentCorrect <- logitNumerator/(logitNumerator+logitDenom)

#_______________
# Save the percent correct data of that iteration  
  bayesCorrectVector[iterator] <- naivePercentCorrect
  logitCorrectVector[iterator] <- logitPercentCorrect
}
# Create box and whisker plots from the lists, labeled and from 0-100%
boxplot(bayesCorrectVector, logitCorrectVector, 
        names = c("Naive Bayes","Logistic Regression"),
        xlab = "Prediction Type", ylab = "Correct Guess Percentages",
        ylim = c(0,1), col = c("pink","light blue"))
#Summary of the box and whisker vectors
summary(bayesCorrectVector)
summary(logitCorrectVector)


