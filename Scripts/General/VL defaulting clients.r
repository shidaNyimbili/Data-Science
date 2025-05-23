# Machine Learning Project: Predicting HIV/AIDS Viral Load Failure in R - Action HIV Project Zambia

install.packages("DALEX")
install.packages("tidymodels")
install.packages("ranger", type = "source")

# # Load required libraries
# library(tidymodels)
# library(ggplot2)
# library(DALEX)
# library(dplyr)
# library(ranger)
source("scripts/General/r prep2.r")


set.seed(123)  # For reproducibility

# Step 1: Create synthetic dataset
# Simulating data for 1000 patients with relevant features

hiv_data <- read.xlsx("Data/hiv_data.xlsx")

# n <- 1000
# hiv_data <- tibble(
#   patient_artnum = 1:n,
#   age = runif(n, 18, 65),  # Age between 18 and 65
#   cd4_count = rnorm(n, 350, 100),  # CD4 count (mean 350, sd 100)
#   adherence_score = runif(n, 0, 1),  # Adherence to ART (0 to 1)
#   time_on_art = runif(n, 0.5, 10),  # Years on ART
#   prior_vl = rnorm(n, 5000, 2000),  # Prior viral load (copies/mL)
#   vl_failure = factor(sample(c("Suppressed", "Unsuppressed"), n, replace = TRUE, prob = c(0.7, 0.3)))  # Target: 30% unsuppressed
# )

# Step 2: Data Preprocessing with tidymodels
# Define recipe for preprocessing
recipe <- recipe(vl_failure ~ age + cd4_count + adherence_score + time_on_art + prior_vl, data = hiv_data) %>%
  step_normalize(all_numeric_predictors()) %>%  # Normalize numeric features
  step_dummy(all_nominal_predictors())  # Encode categorical variables (if any)

# Step 3: Split data into training and testing sets
data_split <- initial_split(hiv_data, prop = 0.8, strata = vl_failure)
train_data <- training(data_split)
test_data <- testing(data_split)

# Step 4: Define and train random forest model
rf_model <- rand_forest(trees = 100, mtry = tune(), min_n = tune()) %>%
  set_engine("ranger") %>%
  set_mode("classification")

# Create workflow
rf_workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(rf_model)

# Tune hyperparameters
rf_grid <- grid_regular(
  mtry(range = c(2, 5)),
  min_n(range = c(5, 20)),
  levels = 3
)
rf_tune <- tune_grid(
  rf_workflow,
  resamples = vfold_cv(train_data, v = 5, strata = vl_failure),
  grid = rf_grid,
  metrics = metric_set(roc_auc, accuracy)
)

# Select best model
best_rf <- select_best(rf_tune, metric = "roc_auc")
final_workflow <- finalize_workflow(rf_workflow, best_rf)

# Fit final model on training data
final_fit <- fit(final_workflow, data = train_data)

# Step 5: Evaluate model on test data
test_predictions <- predict(final_fit, test_data, type = "prob") %>%
  bind_cols(predict(final_fit, test_data), test_data) %>%
  rename(pred_prob = .pred_Unsuppressed, pred_class = .pred_class)

# Calculate performance metrics
metrics <- test_predictions %>%
  metrics(truth = vl_failure, estimate = pred_class, pred_prob)
print(metrics)

# ROC curve
roc_data <- test_predictions %>%
  roc_curve(truth = vl_failure, pred_prob)
roc_plot <- ggplot(roc_data, aes(x = 1 - specificity, y = sensitivity)) +
  geom_path() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "ROC Curve for VL Failure Prediction", x = "False Positive Rate", y = "True Positive Rate") +
  theme_minimal()
print(roc_plot)

# Step 6: Rank patients by risk of unsuppressed VL
ranked_patients <- test_data %>%
  select(patient_artnum, vl_failure) %>%
  bind_cols(predict(final_fit, test_data, type = "prob")) %>%
  arrange(desc(.pred_Unsuppressed)) %>%
  select(patient_artnum, vl_failure, prob_unsuppressed = .pred_Unsuppressed)

# Save ranked patients to CSV
write.csv(ranked_patients, "ranked_patients_vl_failure.csv", row.names = FALSE)
print(head(ranked_patients, 10))  # Display top 10 high-risk patients

# Step 7: Model interpretation with DALEX
explainer <- DALEX::explain(
  model = final_fit,
  data = test_data %>% select(-vl_failure, -patient_artnum),
  y = as.numeric(test_data$vl_failure == "Unsuppressed"),
  label = "Random Forest"
)

# Feature importance
feature_imp <- model_parts(explainer, type = "difference")
plot(feature_imp, max_vars = 5, show_boxplots = FALSE) +
  labs(title = "Feature Importance for VL Failure Prediction")

# Save feature importance plot
ggsave("feature_importance.png", width = 8, height = 6)