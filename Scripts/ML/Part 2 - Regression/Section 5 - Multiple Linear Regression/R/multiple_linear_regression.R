# Multiple Linear Regression

# Importing the dataset
dataset <- read.csv('50_Startups.csv')

#review the data
colSums(is.na(dataset))   # Check total missing values per column

str(dataset) #Check the structure of the dataset

head(dataset) #Check the first few rows of the dataset

# view(dataset)



# Encoding categorical data
dataset$State <- factor(dataset$State,
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1, 2, 3))
# set RNG seed for reproducibility
# Splitting the dataset into the Training set and Test set
library(caTools)

set.seed(123)

split <- sample.split(dataset$Profit, SplitRatio = 0.8)

training_set <- subset(dataset, split == TRUE)
training_set

test_set <- subset(dataset, split == FALSE)

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
regressor <- lm(formula = Profit ~  R.D.Spend + Administration + Marketing.Spend + State,
               data = training_set) #manually adding all the independent variables

regressor <- lm(formula = Profit ~ .,
               data = training_set) #using the dot operator to include all independent variables

summary(regressor)

#From the summary results only one variable has stat significance & influences profits
regressor2 <- lm(formula = Profit ~ R.D.Spend,
               data = training_set) #manually adding the only significant independent variable
summary(regressor2)

# Predicting the Test set results
y_pred <- predict(regressor, newdata = test_set)

y_pred
# Compare predicted values with actual values
results <- data.frame(Actual = test_set$Profit, Predicted = y_pred)

results

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

ggsave("actual_vs_predicted.png",dpi = 300, device = "png",width = 8, height = 6)


ggsave("demo.png",
       device="png",
       type="cairo",
       dpi = 300,
       height = 6.5,
       width = 15)

#Evaluate the model performance
library(Metrics)
# install.packages("Metrics", type = "source")

# Calculate Accuracy Metrics
MAE <- mae(test_set$Profit, y_pred)
MSE <- mse(test_set$Profit, y_pred)
RMSE <- rmse(test_set$Profit, y_pred)
Rsquared <- 1 - sum((test_set$Profit - y_pred)^2) / sum((test_set$Profit - mean(test_set$Profit))^2)

# Print the results

cat("Mean Absolute Error (MAE):", round(MAE, 2), "\n")
cat("Mean Squared Error (MSE):", round(MSE, 2), "\n")
cat("Root Mean Squared Error (RMSE):", round(RMSE, 2), "\n")
cat("R-squared on Test Set:", round(Rsquared, 4), "\n")

# The Mean Absolute Error (MAE) indicates the average absolute difference between the predicted and actual values.
# The R-squared value indicates how well the model explains the variability of the response data around its mean.
# The closer the R-squared value is to 1, the better the model fits the data.
# The RMSE value indicates the average distance between the predicted and actual values.
# A lower RMSE value indicates a better fit.
# The MAE value indicates the average absolute difference between the predicted and actual values.
# A lower MAE value indicates a better fit.
# The MSE value indicates the average squared difference between the predicted and actual values.
# A lower MSE value indicates a better fit.
# The Rsquared value indicates how well the model explains the variability of the response data around its mean.
# The closer the Rsquared value is to 1, the better the model fits the data.
# The MAE, MSE, RMSE, and Rsquared values can be used to compare the performance of different models.



#Building the optimal model using Backward Elimination
regressor <- lm(formula = Profit ~ + R.D.Spend + Administration + Marketing.Spend + State,
               data = dataset) #manually adding all the independent variables

summary(regressor)

#Remove variable state
regressor <- lm(formula = Profit ~ + R.D.Spend + Administration + Marketing.Spend,
               data = dataset) #manually adding all the independent variables

summary(regressor)

#Remove variable Administration
regressor <- lm(formula = Profit ~ + R.D.Spend + Marketing.Spend,
               data = dataset) #manually adding all the independent variables
summary(regressor)
#Remove variable Marketing.Spend
regressor <- lm(formula = Profit ~ + R.D.Spend,
               data = dataset) #manually adding all the independent variables
summary(regressor)

##Automatic backward elimination

backwardElimination <- function(x, sl) {
  numVars = length(x)
  for (i in c(1:numVars)){
    regressor = lm(formula = Profit ~ ., data = x)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > sl){
      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
      x = x[, -j]
    }
    numVars = numVars - 1
  }
  return(summary(regressor))
}

SL = 0.05
dataset <- dataset[, c(1,2,3,4,5)]
backwardElimination(training_set, SL)

# Final model
# Profit ~ R.D.Spend
# The final model after backward elimination is:
# Profit ~ R.D.Spend
# This means that only the R&D Spend variable is statistically significant in predicting Profit.
