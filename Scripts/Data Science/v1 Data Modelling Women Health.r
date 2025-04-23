#'*Load packages*
source("scripts/r prep2.r")

install.packages('mice')

#'*Load the datasets*
# loading xlsx files for DHS, Economic data, and Health Facility Data
dh_data <- read.xlsx("Zambia_DHS.xlsx")  # Maternal mortality, contraceptive use, reproductive health
health_facility_data <- read.xlsx("Zambia_MOH_Health_Facilities.xlsx")  # Facility locations, services, staffing
economic_data <- read.xlsx("Zambia_Economic_Data.xlsx")  # Employment, income inequality, poverty rates
mental_health_data <- read.xlsx("Zambia_Mental_Health_GBV.xlsx")  # Prevalence of depression, anxiety, GBV
gis_data <- read.xlsx("Zambia_GIS_Health_Facilities.xlsx")  # GIS and accessibility data
historical_data <- read.xlsx("Zambia_Historical_Health_Trend.xlsx")  # Disease burden, maternal health trends


#'*Check for missing data*
summary(dh_data)
summary(health_facility_data)
summary(economic_data)
summary(mental_health_data)
summary(gis_data)
summary(historical_data)

#'*Handle missing data if needed*
dh_data <- na.omit(dh_data)
health_facility_data <- na.omit(health_facility_data)
economic_data <- na.omit(economic_data)
mental_health_data <- na.omit(mental_health_data)
gis_data <- na.omit(gis_data)
historical_data <- na.omit(historical_data)

#'*Data Preprocessing for Spatial Model*
#Ensure all datasets are integrated, cleaned, and properly formatted for spatial analysis
# Create Spatial DataFrame for health facility locations

coordinates(health_facility_data) <- ~longitude+latitude
proj4string(health_facility_data) <- CRS("+proj=longlat +datum=WGS84")  # Coordinate reference system

# Spatial weights matrix for the nearest neighbors using Euclidean distance
gis_coords <- cbind(gis_data$longitude, gis_data$latitude)  # Coordinates for accessibility analysis
neighbors <- dnearneigh(gis_coords, 0, 100)  # Distance threshold of 100 km for neighbors
W <- nb2listw(neighbors, style = "W")  # Create a spatial weight matrix

# Visualize the spatial distribution of health facilities
plot(health_facility_data)

#'*Calculations for Health Burdens*
#'

#'* Creating multidimensional women’s health index*
#'Here we are Creating a composite index to combine health data indicators

# Normalizing the data for the composite index
dh_data$normalized_maternal_mortality <- scale(dh_data$maternal_mortality)
dh_data$normalized_contraceptive_use <- scale(dh_data$contraceptive_use)
dh_data$normalized_reproductive_health <- scale(dh_data$reproductive_health)

# Create the Multidimensional Index
dh_data$WHI <- rowMeans(dh_data[, c("normalized_maternal_mortality", 
                                    "normalized_contraceptive_use", 
                                    "normalized_reproductive_health")], na.rm = TRUE)

# Plot the WHI by province
ggplot(dh_data, aes(x = province, y = WHI)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Women’s Health Index by Province")

#'*Creating Gender-sensitive health burden model (Adjusted DALY/QALY)*
#'Here we will incorporate gender-specific health burdens into DALY/QALY
# Gender-Based Violence and Mental Health Impact Calculation

mental_health_data$adjusted_health_burden <- (mental_health_data$prevalence_of_depression + 
                                                mental_health_data$prevalence_of_anxiety) * 0.5 + 
  (mental_health_data$prevalence_of_GBV * 0.5)

# Incorporate reproductive health burden (maternal mortality)
dh_data$adjusted_reproductive_burden <- dh_data$maternal_mortality * 0.7

# Create Gender-Sensitive Health Burden Index
mental_health_data$gender_health_burden <- mental_health_data$adjusted_health_burden + 
  dh_data$adjusted_reproductive_burden

# Visualize the adjusted health burden
ggplot(mental_health_data, aes(x = province, y = gender_health_burden)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Gender-Sensitive Health Burden by Province")


#'*Creating Spatial-temporal modeling of Gender health disparities*
#'We are running all spatial analysis here including health disparities analysis
#Additionally we're conducting spatial-temporal analysis to predict and visualize health disparities over time.

# Fit a time-series ARIMA model to historical health data
historical_health_ts <- ts(historical_data$maternal_mortality, frequency = 12, start = c(2000, 1))
arima_model <- auto.arima(historical_health_ts)

# Forecast future health trends (next 5 years)
forecast_result <- forecast(arima_model, h = 5)
plot(forecast_result, main = "Forecasted Maternal Mortality")

# Spatial clustering analysis for health disparities (using Moran's I)
moran.test(dh_data$maternal_mortality, listw = W)


#'*Creating predictive model analytics for women’s healthcare*
#'Predictive Analytics for Women’s Healthcare Demand
#'Use machine learning(Random Forest algorithm) to predict healthcare demand

# Prepare data for training the predictive model
predictive_data <- dh_data %>%
  select(province, maternal_mortality, contraceptive_use, reproductive_health, economic_status)

# Split the data into training and test sets
set.seed(123)
trainIndex <- createDataPartition(predictive_data$maternal_mortality, p = 0.8, list = FALSE)
train_data <- predictive_data[trainIndex,]
test_data <- predictive_data[-trainIndex,]

# Train a Random Forest model
rf_model <- randomForest(maternal_mortality ~ contraceptive_use + reproductive_health + economic_status, 
                         data = train_data)

# Predict healthcare demand (e.g., maternal health services)
predicted_demand <- predict(rf_model, test_data)

# Visualize predicted healthcare demand
ggplot(test_data, aes(x = maternal_mortality, y = predicted_demand)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Predicted Healthcare Demand vs Actual Demand")


#'*Creating the integrtaed final model*
#'Combining all models into a final integrated framework for policy analysis

# Combine all metrics into a final data frame
final_model_data <- data.frame(province = dh_data$province,
                               WHI = dh_data$WHI,
                               health_burden = mental_health_data$gender_health_burden,
                               predicted_demand = predicted_demand)

# Rank provinces based on combined metrics
final_model_data$final_score <- rowMeans(final_model_data[, c("WHI", "health_burden", "predicted_demand")], na.rm = TRUE)

# Visualize the final integrated model
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


