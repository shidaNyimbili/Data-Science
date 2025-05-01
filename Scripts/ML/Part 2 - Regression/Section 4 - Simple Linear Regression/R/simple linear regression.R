# Data Preprocessing

# Importing the dataset
dataset <- read.csv('Salary_Data.csv')

#review the data
colSums(is.na(dataset))   # Check total missing values per column

str(dataset) #Check the structure of the dataset

head(dataset) #Check the first few rows of the dataset

view(dataset)

# set RNG seed for reproducibility
# Splitting the dataset into the Training set and Test set
set.seed(123)

# split2 <- sample.split(dataset$Country, SplitRatio = 0.75)
# 
# split2

split <- sample.split(dataset$Salary, SplitRatio = 2/3)

split

training_set = subset(dataset, split == TRUE)

training_set

test_set = subset(dataset, split == FALSE)

test_set

#Check sizes of training and test sets
cat("Training set size:", nrow(training_set), "\n")
cat("Testing set size:", nrow(test_set), "\n")


# Feature Scaling

setNames(seq_along(colnames(dataset)), colnames(dataset)) # View column names with their index positions

# training_set[, 1:2] <- scale(training_set[, 1:2])
# test_set[, 1:2] <- scale(test_set[, 1:2])
# 
# # View the scaled training and test sets
# training_set
# test_set

#Fitting the Simple Linear Regression to the Training set #Training the model
regressor <- lm(formula = Salary ~ YearsExperience,
               data = training_set)

summary(regressor)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)


# Create a comparison df between real vs predicted values #Error Analysis #Model evaluation
comparison_df <- data.frame(
  YearsExperience = test_set$YearsExperience,
  Actual_Salary = test_set$Salary,
  Predicted_Salary = round(y_pred, 0),
  Difference = round(y_pred - test_set$Salary, 0)  # Error = Predicted - Actual
)

# View the table
print(comparison_df)

# Visualising the Training set results

ggplot() +
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Salary vs Experience (Training set)') +
  xlab('Years of experience') +
  ylab('Salary')

# Visualising the Test set results

ggplot() +
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Salary vs Experience (Test set)') +
  xlab('Years of experience') +
  ylab('Salary')