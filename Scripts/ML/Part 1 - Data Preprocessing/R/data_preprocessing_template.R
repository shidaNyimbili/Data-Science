# Data Preprocessing Template

# Importing the dataset

dataset = read.csv('Data.csv')

glimpse(dataset)

str(dataset)

colSums(is.na(dataset))   # Check total missing values per column



colSums(is.na(dataset))
# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$DependentVariable, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)