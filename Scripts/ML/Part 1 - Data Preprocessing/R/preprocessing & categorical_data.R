# Data Preprocessing

#Load packages
source("Scripts/General/r prep2.r")

#Importing the dataset

library(tidyverse)


dataset <- read.csv('Data.csv')

data <- read.csv("Data Science/Scripts/ML/Part 1 - Data Preprocessing/R/")

#review the data
colSums(is.na(dataset))   # Check total missing values per column

str(dataset) #Check the structure of the dataset

head(dataset) #Check the first few rows of the dataset

# Taking care of missing data #mean imputation
dataset$Age <- ifelse(is.na(dataset$Age),
                     ave(dataset$Age, FUN = function(x) mean(x, na.rm = TRUE)),
                     dataset$Age)
dataset$Salary <- ifelse(is.na(dataset$Salary),
                        ave(dataset$Salary, FUN = function(x) mean(x, na.rm = TRUE)),
                        dataset$Salary)

# Encoding categorical data
dataset$Country <- factor(dataset$Country,
                         levels = c('France', 'Spain', 'Germany'),
                         labels = c(1, 2, 3))


view(dataset)
glimpse(dataset)

dataset$Purchased <- factor(dataset$Purchased,
                           levels = c('No', 'Yes'),
                           labels = c(0, 1))

view(dataset)

# set RNG seed for reproducibility
# Splitting the dataset into the Training set and Test set
set.seed(123)

# split2 <- sample.split(dataset$Country, SplitRatio = 0.75)
# 
# split2

split <- sample.split(dataset$Purchased, SplitRatio = 0.8)

split

training_set = subset(dataset, split == TRUE)

training_set

test_set = subset(dataset, split == FALSE)

test_set


# Feature Scaling

setNames(seq_along(colnames(dataset)), colnames(dataset)) # View column names with their index positions

training_set[, 2:3] <- scale(training_set[, 2:3])
test_set[, 2:3] <- scale(test_set[, 2:3])

# View the scaled training and test sets
training_set
test_set
