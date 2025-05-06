# Machine Learning Pipeline in R using caret (with Iris dataset)

#Load packages
source("Scripts/Data Science/r prep2.r")

# Load necessary libraries
library(caret)
library(randomForest)
library(pROC)

install.packages('terra')

install.packages("terra")

install.packages("geodata", dependencies = TRUE)


if (!require("devtools")) install.packages("devtools")
devtools::install_github("talgalili/d3heatmap")

# Load dataset
data(iris)

iris

# Binary classification: setosa vs non-setosa
iris_bin <- iris
iris_bin$Species <- ifelse(iris_bin$Species == "setosa", "setosa", "non-setosa")
iris_bin$Species <- factor(iris_bin$Species)

# Split into train and test
set.seed(123)
trainIndex <- createDataPartition(iris_bin$Species, p = 0.8, list = FALSE)
trainData <- iris_bin[trainIndex, ]
testData <- iris_bin[-trainIndex, ]

# Feature selection using RFE
ctrl <- rfeControl(functions = rfFuncs, method = "cv", number = 5)
rfe_results <- rfe(trainData[, -5], trainData$Species, sizes = c(1:4), rfeControl = ctrl)
print(rfe_results)
predictors(rfe_results)

# Train with hyperparameter tuning (Random Forest)
tuneGrid <- expand.grid(.mtry = c(1, 2, 3, 4))
rf_model <- train(Species ~ ., data = trainData,
                  method = "rf",
                  trControl = trainControl(method = "cv", number = 5,
                                           classProbs = TRUE,
                                           summaryFunction = twoClassSummary),
                  metric = "ROC",
                  tuneGrid = tuneGrid)

print(rf_model)

# Variable importance
var_imp <- varImp(rf_model)
print(var_imp)
plot(var_imp, main = "Variable Importance")

# ROC curve and AUC
probs <- predict(rf_model, newdata = testData, type = "prob")
roc_obj <- roc(response = testData$Species,
               predictor = probs$setosa,
               levels = rev(levels(testData$Species)))
plot(roc_obj, col = "blue", main = "ROC Curve")
print(paste("AUC:", auc(roc_obj)))
