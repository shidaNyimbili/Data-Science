
#'*Load packages*
source("scripts/r prep2.r")

# Load the datasets (Excel files)
dh_data <- read.xlsx("Data/womens health/Zambia_DHS.xlsx")
health_facility_data <- read.xlsx("Data/womens health/Zambia_MOH_Health_Facilities.xlsx")
economic_data <- read.xlsx("Data/womens health/Zambia_Economic_Data.xlsx")
mental_health_data <- read.xlsx("Data/womens health/Zambia_Mental_Health_GBV.xlsx")
gis_data <- read.xlsx("Data/womens health/Zambia_GIS_Health_Facilities.xlsx")
historical_data <- read.xlsx("Data/womens health/Zambia_Historical_Health_Trend.xlsx")

# Handling missing values more robustly by using imputation
dh_data <- mice(dh_data, method = "pmm", m = 5) %>% complete()
health_facility_data <- mice(health_facility_data, method = "pmm", m = 5) %>% complete()
economic_data <- mice(economic_data, method = "pmm", m = 5) %>% complete()
mental_health_data <- mice(mental_health_data, method = "pmm", m = 5) %>% complete()
gis_data <- mice(gis_data, method = "pmm", m = 5) %>% complete()
historical_data <- mice(historical_data, method = "pmm", m = 5) %>% complete()

#'*Spatial data preparation and analysis*
#'Improving spatial data analysis with advanced handling of coordinates,
#'projection systems, and spatial modeling

# Create spatial data frame for health facility locations
coordinates(health_facility_data) <- ~longitude + latitude
proj4string(health_facility_data) <- CRS("+proj=longlat +datum=WGS84")

# Spatial weights matrix for nearest neighbors using Euclidean distance
gis_coords <- cbind(gis_data$longitude, gis_data$latitude) 
neighbors <- dnearneigh(gis_coords, 0, 100)  # 100 km distance threshold
W <- nb2listw(neighbors, style = "W")

# Visualizing the spatial distribution of health facilities
spplot(health_facility_data, main = "Spatial Distribution of Health Facilities")

#'*Creating the Multidimensional women’s health index*
#'To improve the robustness of the WHI, we scale and normalize more health
#'indicators and apply a weighted aggregation to the final score.

# Normalize the key health indicators
dh_data$normalized_maternal_mortality <- scale(dh_data$maternal_mortality)
dh_data$normalized_contraceptive_use <- scale(dh_data$contraceptive_use)
dh_data$normalized_reproductive_health <- scale(dh_data$reproductive_health)

# Create the Multidimensional Women’s Health Index (WHI)
dh_data$WHI <- rowMeans(dh_data[, c("normalized_maternal_mortality", 
                                    "normalized_contraceptive_use", 
                                    "normalized_reproductive_health")], na.rm = TRUE)

# Visualizing the WHI by province
ggplot(dh_data, aes(x = province, y = WHI)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Women’s Health Index by Province")

#'*Creating the gender sensitive health burden (Adjusted DALY)*
#'*Creating a more advanced model for gender-sensitive health burden,
#'accounting for both physical and mental health dimensions.

# Advanced Gender-sensitive Health Burden Model (adjusted DALY)
mental_health_data$adjusted_health_burden <- (mental_health_data$prevalence_of_depression + 
                                                mental_health_data$prevalence_of_anxiety) * 0.5 + 
  (mental_health_data$prevalence_of_GBV * 0.5)

# Adjust reproductive burden (maternal mortality)
dh_data$adjusted_reproductive_burden <- dh_data$maternal_mortality * 0.7

# Combine both burdens into a comprehensive gender-health-burden index
mental_health_data$gender_health_burden <- mental_health_data$adjusted_health_burden + 
  dh_data$adjusted_reproductive_burden

# Visualizing the adjusted gender-health burden
ggplot(mental_health_data, aes(x = province, y = gender_health_burden)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Gender-Sensitive Health Burden by Province")

#'*Creating the spatial temporal modeling of health disparities*
#'Integrating spatial and temporal models using ARIMA and Moran’s I for
#'forecasting and clustering analysis

# Time-series forecasting using ARIMA for maternal mortality trends
historical_health_ts <- ts(historical_data$maternal_mortality, frequency = 12, start = c(2000, 1))
arima_model <- auto.arima(historical_health_ts)

# Forecast future health trends (next 5 years)
forecast_result <- forecast(arima_model, h = 60)  # Forecasting 5 years ahead
plot(forecast_result, main = "Forecasted Maternal Mortality")

# Spatial clustering analysis for health disparities (using Moran's I)
moran_result <- moran.test(dh_data$maternal_mortality, listw = W)
print(moran_result)

#'*Predictive Analytics for Healthcare Demand
#'Enhance machine learning models (Random Forest and XGBoost) for demand
#'prediction and resource allocation optimization

# Prepare data for predictive model training
predictive_data <- dh_data %>%
  select(province, maternal_mortality, contraceptive_use, reproductive_health, economic_status)

# Split data into training and test sets
set.seed(123)
trainIndex <- createDataPartition(predictive_data$maternal_mortality, p = 0.8, list = FALSE)
train_data <- predictive_data[trainIndex,]
test_data <- predictive_data[-trainIndex,]

# Random Forest model
rf_model <- randomForest(maternal_mortality ~ contraceptive_use + reproductive_health + economic_status, 
                         data = train_data)
predicted_demand <- predict(rf_model, test_data)

# XGBoost model for improved prediction accuracy
xgb_data <- as.matrix(train_data[, c("maternal_mortality", "contraceptive_use", "reproductive_health", "economic_status")])
xgb_model <- xgboost(data = xgb_data, label = train_data$maternal_mortality, nrounds = 100)

# Predict with XGBoost
xgb_predictions <- predict(xgb_model, newdata = xgb_data)

# Visualizing the predicted healthcare demand vs actual demand
ggplot(test_data, aes(x = maternal_mortality, y = predicted_demand)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Predicted Healthcare Demand vs Actual Demand")

#'*Integrated final model for policy framework*
#'Combining all the models into a final integrated framework
#'for decision-making and policy analysis

# Combine all models into a final data frame
final_model_data <- data.frame(province = dh_data$province,
                               WHI = dh_data$WHI,
                               health_burden = mental_health_data$gender_health_burden,
                               predicted_demand = predicted_demand)

# Rank provinces based on the final integrated score
final_model_data$final_score <- rowMeans(final_model_data[, c("WHI", "health_burden", "predicted_demand")], na.rm = TRUE)

# Visualize the integrated model output
ggplot(final_model_data, aes(x = province, y = final_score)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Integrated Health Disparities and Healthcare Demand by Province")

#'*Integrating the spatial-temporal forecasting model with other components*
# Adding Spatial and Temporal Forecasting Results to the Final Model Data
final_model_data <- data.frame(province = dh_data$province,
                               WHI = dh_data$WHI,
                               health_burden = mental_health_data$gender_health_burden,
                               predicted_demand = predicted_demand)

# Spatially adjusted scores based on Moran's I and ARIMA forecast
final_model_data$forecasted_maternal_mortality <- forecast_result$mean  # Add the ARIMA forecast to the final data

# Add the spatial clustering Moran's I result
final_model_data$moran_i_value <- moran_result$estimate[1]  # Moran's I value for spatial autocorrelation

# Combining all elements into one final health disparity score
final_model_data$final_score <- rowMeans(final_model_data[, c("WHI", "health_burden", "predicted_demand", 
                                                              "forecasted_maternal_mortality", "moran_i_value")], na.rm = TRUE)

# Visualization of the integrated model with spatial-temporal components
ggplot(final_model_data, aes(x = province, y = final_score)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Integrated Health Disparities and Healthcare Demand by Province (With Spatial-Temporal Analysis)")

# Identifying provinces with extreme health disparities and high demand
high_disparity_provinces <- final_model_data %>%
  filter(final_score > quantile(final_model_data$final_score, 0.75))

# Visualize provinces with extreme disparities and demand
ggplot(high_disparity_provinces, aes(x = province, y = final_score)) +
  geom_bar(stat = "identity", fill = "red") +
  theme_minimal() +
  labs(title = "Provinces with Extreme Health Disparities and Healthcare Demand")

