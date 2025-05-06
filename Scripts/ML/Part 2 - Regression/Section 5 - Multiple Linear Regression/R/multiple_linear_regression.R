# Multiple Linear Regression

# Importing the dataset
dataset = read.csv('50_Startups.csv')

#review the data
colSums(is.na(dataset))   # Check total missing values per column

str(dataset) #Check the structure of the dataset

head(dataset) #Check the first few rows of the dataset

view(dataset)



# Encoding categorical data
dataset$State = factor(dataset$State,
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1, 2, 3))
# set RNG seed for reproducibility
# Splitting the dataset into the Training set and Test set
library(caTools)

set.seed(123)

split = sample.split(dataset$Profit, SplitRatio = 0.8)

training_set = subset(dataset, split == TRUE)
training_set

test_set = subset(dataset, split == FALSE)

test_set

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

#Check sizes of training and test sets
cat("Training set size:", nrow(training_set), "\n")
cat("Testing set size:", nrow(test_set), "\n")


# Feature Scaling

setNames(seq_along(colnames(dataset)), colnames(dataset)) # View column names with their index positions

# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = Profit ~  R.D.Spend + Administration + Marketing.Spend + State,
               data = training_set) #manually adding all the independent variables

regressor = lm(formula = Profit ~ .,
               data = training_set) #using the dot operator to include all independent variables

summary(regressor)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)