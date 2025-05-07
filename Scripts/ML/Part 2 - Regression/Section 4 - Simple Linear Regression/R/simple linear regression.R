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

y_pred


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

devtools::install_github("tylermorganwall/rayshader")

ggplot() +
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Salary vs Experience (Test set)') +
  xlab('Years of experience') +
  ylab('Salary')

# Compare predicted values with actual values
results = data.frame(Actual = test_set$Salary, Predicted = y_pred)

results

# Create a comparison df between real vs predicted values #Error Analysis #Model evaluation
comparison_df <- data.frame(
  YearsExperience = test_set$YearsExperience,
  Actual_Salary = test_set$Salary,
  Predicted_Salary = round(y_pred, 0),
  Difference = round(y_pred - test_set$Salary, 0)  # Error = Predicted - Actual
)

# View the table
print(comparison_df)

# Reshape data to long format for ggplot
library(tidyverse)
library(ggplot2)

results_long <- results %>%
  mutate(Index = row_number()) %>%
  pivot_longer(cols = c("Actual", "Predicted"),
               names_to = "Type",
               values_to = "Profit")

results_long



# Plot actual vs predicted with different colors
ggplot(results_long, aes(x = Index, y = Profit, color = Type)) +
  geom_point(size = 3) +
  labs(title = "Actual vs Predicted Profits",
       x = "Observation",
       y = "Profit") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "orange"))
#Evaluate the model performance 
library(Metrics)
# install.packages("Metrics")
# install.packages("Metrics", type = "source")
# Load Metrics library for R-squared
library(Metrics)

results_long2 <- data.frame(Actual = test_set$Salary, Predicted = y_pred)

results_long2

results_clean <- na.omit(results_long2)

results_clean

# Calculate Accuracy Metrics
mae_value = mae(results_clean$Actual, y_pred)
mse_value = mse(results_clean$Actual, Predicted)
rmse_value = rmse(results_clean$Actual, Predicted)
r2_value = 1 - sum((results_clean$Actual - Predicted)^2) / sum((results_clean$Actual - mean(results_clean$Actual))^2)
# Print the results
cat("Mean Absolute Error (MAE):", round(mae_value, 2), "\n")
cat("Mean Squared Error (MSE):", round(mse_value, 2), "\n")
cat("Root Mean Squared Error (RMSE):", round(rmse_value, 2), "\n")
cat("R-squared on Test Set:", round(r2_value, 4), "\n")

