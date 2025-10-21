#======================================================
# BUILDING A LOGISTIC REGRESSION USING TITANIC DATASET
#======================================================

# Load necessary libraries
library(tidyverse)
library(mlr)

# Set working directory
setwd("C:/Users/Ajose Moses/Desktop/My Studies/Titanic")

# Load the training dataset
titanic <- read_csv("train_clean.csv")
titanic

# DATA EXPLORATION
colnames(titanic) # Column names
str(titanic) # Structure of the data
colSums(is.na(titanic)) # Missing values by columns

# Convert category variables to factors
titanicNew <- titanic |> 
  mutate(
    across(c(Survived, Pclass, Sex, Cabin, 
             Embarked, Salutation, Age_Group), as.factor)
  )

#=======================================================
# BUILDING THE MODEL
#=======================================================

# Define the task
titanicTask <- makeClassifTask(data = titanicNew, target = "Survived")
titanicTask

# Define the learner
logreg <- makeLearner("classif.logreg", predict.type = "prob")

# Cross Validate the model process
# LOO
LOO <- makeResampleDesc(method = "LOO")

LOOCV <- resample(learner = logreg,
                  task = titanicTask,
                  resampling = LOO,
                  measures = list(mmce, acc))

calculateConfusionMatrix(LOOCV$pred, relative = TRUE)

# Building the model
logregModel <- train(learner = logreg,
                     task = titanicTask)

# Making predictions
titanic_clean <- read_csv("test_clean.csv")

# DATA EXPLORATION
colnames(titanic_clean) # Column names
str(titanic_clean) # Structure of the data
colSums(is.na(titanic_clean)) # Missing values by columns

# Convert category variables to factors
titanic_testNew <- titanic_clean |> 
  mutate(
    across(c(Pclass, Sex, Cabin, 
             Embarked, Salutation, Age_Group), as.factor)
  )

pred <- predict(object = logregModel,
                newdata = titanic_testNew)

# Extract predicted classes (0 or 1)
predicted_survived <- as.numeric(pred$data$response) - 1  # converts factor levels 1/2 to 0/1

# Create submission dataframe with PassengerId and Survived
submission <- tibble(
  PassengerId = titanic_clean$PassengerId,
  Survived = predicted_survived
)

# Write to CSV file
write_csv(submission, "titanic_logreg.csv")
