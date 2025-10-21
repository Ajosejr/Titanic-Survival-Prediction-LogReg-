#==================================================
# CLEANING OF THE TITANIC TRAINING SETS
#==================================================

# Load the necessary libraries
library(tidyverse)

# Set working directory
setwd("C:/Users/Ajose Moses/Desktop/My Studies/Titanic")

# Load the training dataset
titanic <- read_csv("train.csv")
titanic

# DATA EXPLORATION
colnames(titanic) # Column names
str(titanic) # Structure of the data
colSums(is.na(titanic)) # Missing values by columns

# DATA TRANSFORMATION
# Dealing with missing values in Age variable
titanic$Age <- ifelse(is.na(titanic$Age),
                      mean(titanic$Age, na.rm = TRUE),
                      titanic$Age)

# Dealing with missing values in Fare variable
titanic$Fare <- ifelse(is.na(titanic$Fare),
                      mean(titanic$Fare, na.rm = TRUE),
                      titanic$Fare)

# Dealing with missing values in Embarked variable
titanic$Embarked <- ifelse(is.na(titanic$Embarked),
                           mode(titanic$Embarked),
                           titanic$Embarked)
unique()
# Extracting Salutations from Name variable
surnames <- map_chr(str_split(titanic$Name, "\\."), 1)

salutations <- map_chr(str_split(surnames, ", "), 2)

salutations[!(salutations %in% c("Mr", "Dr", "Master", 
                                 "Miss", "Mrs","Rev"))] <- "Others"

titanicClean <- titanic |> 
  mutate(
    Cabin = case_when(
      is.na(str_sub(Cabin, 1, 1)) ~ "U",
      str_sub(Cabin, 1, 1) == "T" ~ "U",
      TRUE ~ str_sub(Cabin, 1, 1)),
    Salutation = salutations,
    Age_Group = case_when(
      Age < 20 ~ 0,
      Age >= 20 & Age <= 39 ~ 1,
      Age >= 40 & Age <=59 ~ 2,
      TRUE ~ 3
    ),
    FamSize = SibSp + Parch
         ) |> 
  mutate(across(c(Survived, Pclass, Sex, Cabin, 
                  Embarked, Salutation, Age_Group), as.factor)) |> 
  select(Survived, Pclass, Sex, FamSize, Fare, Cabin, Embarked,
         Salutation, Age_Group)
  

colnames(titanicClean)


# Data Scaling
titanic_scaled <- scale(titanicClean[, c(4:5)])   
titanic_scaled_means <- attr(titanic_scaled, "scaled:center")
titanic_scaled_sds   <- attr(titanic_scaled, "scaled:scale")

titanicTibScaled <- as_tibble(titanic_scaled) |> 
  mutate(Survived = titanicClean$Survived,
         Pclass = titanicClean$Pclass,
         Sex = titanicClean$Sex,
         Cabin = titanicClean$Cabin,
         Embarked = titanicClean$Embarked,
         Salutation = titanicClean$Salutation,
         Age_Group = titanicClean$Age_Group
         )

# Scaled Dataset
titanicTibScaled

# Write to CSV file
write_csv(titanicTibScaled, "train_clean.csv")


#==================================================
# CLEANING OF THE TITANIC TEST SETS
#==================================================

# Load the necessary libraries
library(tidyverse)

# Set working directory
setwd("C:/Users/Ajose Moses/Desktop/My Studies/Titanic")

# Load the test dataset
titanic_test <- read_csv("test.csv")
titanic_test

# DATA EXPLORATION
colnames(titanic_test) # Column names
str(titanic_test) # Structure of the data
colSums(is.na(titanic_test)) # Missing values by columns

# DATA TRANSFORMATION
# Dealing with missing values in Age variable
titanic_test$Age <- ifelse(is.na(titanic_test$Age),
                      mean(titanic$Age, na.rm = TRUE),
                      titanic_test$Age)

# Dealing with missing values in Fare variable
titanic_test$Fare <- ifelse(is.na(titanic_test$Fare),
                       mean(titanic$Fare, na.rm = TRUE),
                       titanic_test$Fare)

# Dealing with missing values in Embarked variable
titanic_test$Embarked <- ifelse(is.na(titanic_test$Embarked),
                           mode(titanic$Embarked),
                           titanic_test$Embarked)

# Extracting Salutations from Name variable
surnames_test <- map_chr(str_split(titanic_test$Name, "\\."), 1)

salutations_test <- map_chr(str_split(surnames_test, ", "), 2)

salutations_test[!(salutations_test %in% c("Mr", "Dr", "Master", 
                                 "Miss", "Mrs","Rev"))] <- "Others"

titanic_testClean <- titanic_test |> 
  mutate(
    Cabin = case_when(
      is.na(str_sub(Cabin, 1, 1)) ~ "U",
      str_sub(Cabin, 1, 1) == "T" ~ "U",
      TRUE ~ str_sub(Cabin, 1, 1)),
    Salutation = salutations_test,
    Age_Group = case_when(
      Age < 20 ~ 0,
      Age >= 20 & Age <= 39 ~ 1,
      Age >= 40 & Age <=59 ~ 2,
      TRUE ~ 3
    ),
    FamSize = SibSp + Parch
  ) |> 
  mutate(across(c(Pclass, Sex, Cabin, 
                  Embarked, Salutation, Age_Group), as.factor)) |> 
  select(PassengerId, Pclass, Sex, FamSize, Fare, Cabin, Embarked,
         Salutation, Age_Group)

colnames(titanic_testClean)


# Data Scaling
titanic_test_scaled <- as_tibble(scale(titanic_testClean[, 4:5],
                                    center = titanic_scaled_means,
                                    scale  = titanic_scaled_sds)) |> 
  mutate(PassengerId = titanic_testClean$PassengerId,
         Pclass = titanic_testClean$Pclass,
         Sex = titanic_testClean$Sex,
         Cabin = titanic_testClean$Cabin,
         Embarked = titanic_testClean$Embarked,
         Salutation = titanic_testClean$Salutation,
         Age_Group = titanic_testClean$Age_Group
  )

# Scaled Dataset
titanic_test_scaled

# Write to CSV file
write_csv(titanic_test_scaled, "test_clean.csv")
























