library(dplyr); library(sf); library(ggplot2); library(FactoMineR); library(forecast); library(randomForest)

# Load dataset (replace with actual file)
data <- read.csv("healthcare_demand_data.csv")

# Normalize function for scaling indicators
normalize <- function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

# Apply normalization
data <- data %>% mutate(
  pop_density_norm = normalize(pop_density),
  disease_inc_norm = normalize(disease_incidence),
  access_norm = normalize(facility_access),
  resources_norm = normalize(healthcare_resources)
)

# Compute PCA weights
pca_result <- PCA(data %>% select(ends_with("_norm")), scale.unit = TRUE, graph = FALSE)
weights <- abs(pca_result$var$coord[,1]) / sum(abs(pca_result$var$coord[,1]))

# Compute Composite Indicator
data$composite_index <- rowSums(sweep(data %>% select(ends_with("_norm")), 2, weights, `*`))

##Spatial Analysis in R
# Load Zambia shapefile and merge with computed index
zambia_map <- st_read("zambia_admin_boundaries.shp")
spatial_data <- merge(zambia_map, data, by = "district")

# Plot Composite Indicator Map
ggplot(spatial_data) + geom_sf(aes(fill = composite_index)) + scale_fill_viridis_c() + theme_minimal() + labs(title = "Healthcare Demand Composite Index in Zambia")

# Save as shapefile for GIS analysis
st_write(spatial_data, "composite_index_zambia.shp")

# Time Series Analysis using ARIMA
ts_data <- ts(data$composite_index, start = 2015, frequency = 1)
arima_model <- auto.arima(ts_data)
forecast_values <- forecast(arima_model, h = 5)
plot(forecast_values)

# Random Forest Regression for Prediction
set.seed(123)
train_index <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_index, ]; test_data <- data[-train_index, ]
rf_model <- randomForest(composite_index ~ ., data = train_data %>% select(-district))
predictions <- predict(rf_model, test_data)
plot(test_data$composite_index, predictions)
