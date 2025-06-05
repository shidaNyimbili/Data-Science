# Load required packages
library(openxlsx)
library(ggplot2)
library(pROC)
library(reshape2)
library(caret)
library(cluster)
library(factoextra)
library(iml)
library(randomForest)
library(mlbench)
library(dplyr)
library(ROCR)

install.packages("iml")

# Load the dataset
dataset <- read.xlsx("Data/dummy_sales_data.xlsx")

dataset

str(dataset)

# Convert Date to proper format
# dataset$Date <- as.Date(dataset$Date)
dataset$Date <- as.Date(dataset$Date, format = "%m/%d/%Y")

str(dataset)

colSums(is.na(dataset))
# ----------------------
# 1. KS Plot
# ----------------------
set.seed(123)
pred_probs <- runif(nrow(dataset))  # Dummy predicted probabilities
true_labels <- sample(c(0, 1), nrow(dataset), replace = TRUE)

ks_stat <- ks.test(pred_probs[true_labels == 1], pred_probs[true_labels == 0])
plot(ecdf(pred_probs[true_labels == 1]), col = "blue", main = "KS Plot", xlab = "Predicted Probability", ylab = "Cumulative Probability")
lines(ecdf(pred_probs[true_labels == 0]), col = "red")
legend("bottomright", legend = c("Class 1", "Class 0"), col = c("blue", "red"), lty = 1)

# ----------------------
# 2. SHAP Plot (approximation using Feature Importance)
# ----------------------
rf <- randomForest(Amount ~ Product + Country + Boxes, data = dataset, ntree = 100)
imp <- importance(rf)
barplot(imp[,1], horiz = TRUE, main = "SHAP-like Feature Importance", col = "skyblue", las = 1)

# ----------------------
# 3. ROC Curve
# ----------------------
roc_obj <- roc(true_labels, pred_probs)
plot.roc(roc_obj, main = "ROC Curve", col = "#1c61b6")

# ----------------------
# 4. QQ Plot
# ----------------------
qqnorm(dataset$Amount, main = "QQ Plot of Sales Amount")
qqline(dataset$Amount)

# ----------------------
# 5. Cumulative Explained Variance (PCA)
# ----------------------
num_data <- dataset %>% select_if(is.numeric)
num_data <- scale(num_data)
pca <- prcomp(num_data, scale. = TRUE)
fviz_eig(pca, addlabels = TRUE, main = "Cumulative Explained Variance")

# ----------------------
# 6. Elbow Curve (KMeans)
# ----------------------
fviz_nbclust(num_data, kmeans, method = "wss") + ggtitle("Elbow Curve")

# ----------------------
# 7. Silhouette Curve
# ----------------------
fviz_nbclust(num_data, kmeans, method = "silhouette") + ggtitle("Silhouette Curve")

# ----------------------
# 8. Gini Impurity vs. Entropy
# ----------------------
p <- seq(0.01, 0.99, by = 0.01)
gini <- 2 * p * (1 - p)
entropy <- -p * log2(p) - (1 - p) * log2(1 - p)
plot(p, gini, type = "l", col = "blue", ylim = c(0,1), ylab = "Impurity", main = "Gini vs. Entropy")
lines(p, entropy, col = "red")
legend("topright", legend = c("Gini", "Entropy"), col = c("blue", "red"), lty = 1)

# ----------------------
# 9. Bias-Variance Tradeoff (Simulated)
# ----------------------
complexity <- seq(1, 10)
bias2 <- (10 - complexity)^2 / 100
variance <- complexity^2 / 100
total_error <- bias2 + variance
plot(complexity, total_error, type = "l", col = "black", ylab = "Error", ylim = c(0, max(total_error)), main = "Bias-Variance Tradeoff")
lines(complexity, bias2, col = "red")
lines(complexity, variance, col = "blue")
legend("topright", legend = c("Total Error", "Bias²", "Variance"), col = c("black", "red", "blue"), lty = 1)

# ----------------------
# 10. Partial Dependence Plot (using randomForest)
# ----------------------
partialPlot(rf, pred.data = dataset, x.var = "Boxes", main = "Partial Dependence on Boxes")

# ----------------------
# 11. Precision-Recall Plot
# ----------------------
pred <- prediction(pred_probs, true_labels)
pr <- performance(pred, "prec", "rec")
plot(pr, main = "Precision-Recall Curve", col = "orange", lwd = 2)
