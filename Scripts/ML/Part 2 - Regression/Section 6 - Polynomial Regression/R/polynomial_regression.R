# Polynomial Regression

library(tidyverse)

# Importing the dataset
dataset <- read.csv('Position_Salaries.csv')

#review the data
colSums(is.na(dataset))   # Check total missing values per column

str(dataset) #Check the structure of the dataset

head(dataset) #Check the first few rows of the dataset

setNames(seq_along(colnames(dataset)), colnames(dataset)) # View column names with their index positions

dataset5 <- dataset[2:3]

dataset2 <- select(dataset, c(Level, Salary))

dataset3 <- dataset %>%
  select(Level, Salary) %>%
  filter(Salary > 55000)

dataset <- dataset %>%
  select(Level, Salary)

dataset4
dataset3
dataset2
dataset5
dataset


# set RNG seed for reproducibility
# Splitting the dataset into the Training set and Test set


# library(caTools)
# set.seed(123)
# split = sample.split(dataset$Salary, SplitRatio = 2/3)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Linear Regression to the dataset
lin_reg <- lm(formula = Salary ~ .,
             data = dataset)

summary(lin_reg)

# Predicting a new result with Linear Regression

pred <- predict(lin_reg, newdata = dataset)

pred

# Compare predicted values with actual values linear
results <- data.frame(Actual = dataset$Salary, Predicted = pred)

results

# Fitting Polynomial Regression to the dataset
dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3
dataset$Level4 = dataset$Level^4
poly_reg = lm(formula = Salary ~ .,
              data = dataset)
summary(poly_reg)

# Predicting a new result with Polynomial Regression

predpo <- predict(poly_reg, newdata = dataset)

predpo

# Compare predicted values with actual values polynomial
results <- data.frame(Actual = dataset$Salary, Predicted = predpo)

results

# Reshape data to long format for ggplot
results_long <- results %>%
  mutate(Index = row_number()) %>%
  pivot_longer(cols = c("Actual", "Predicted"),
               names_to = "Type",
               values_to = "Salary")


results_long

#write.csv(results_long, "results_long.csv", row.names = FALSE)


# Plot actual vs predicted with different colors polynomial regression
ggplot(results_long, aes(x = Index, y = Salary, color = Type)) +
  geom_point(size = 3) +
  labs(title = "Actual vs Predicted Profits",
       x = "Observation",
       y = "Profit") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "orange"))


# Visualising the Linear Regression results
# install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = pred),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Linear Regression)') +
  xlab('Level') +
  ylab('Salary')

# Visualising the Polynomial Regression results
# install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(poly_reg, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Polynomial Regression)') +
  xlab('Level') +
  ylab('Salary')

# Visualising the Regression Model results (for higher resolution and smoother curve)
# install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(poly_reg,
                                        newdata = data.frame(Level = x_grid,
                                                             Level2 = x_grid^2,
                                                             Level3 = x_grid^3,
                                                             Level4 = x_grid^4))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Polynomial Regression)') +
  xlab('Level') +
  ylab('Salary')

# Predicting a new result with Linear Regression
predict(lin_reg, data.frame(Level = 6.5))

# Predicting a new result with Polynomial Regression
predict(poly_reg, data.frame(Level = 6.5,
                             Level2 = 6.5^2,
                             Level3 = 6.5^3,
                             Level4 = 6.5^4))
