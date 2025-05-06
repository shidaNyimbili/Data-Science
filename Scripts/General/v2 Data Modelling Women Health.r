
#'*Load packages* 
  # Load the necessary R packages
  source("scripts/r prep2.r")

#'*Load the datasets* 
# Loading xlsx files for DHS, Economic data, and Health Facility Data
dh_data <- read.xlsx("Zambia_DHS.xlsx")  # Maternal mortality, contraceptive use, reproductive health
health_facility_data <- read.xlsx("Zambia_MOH_Health_Facilities.xlsx")  # Facility locations, services, staffing
economic_data <- read.xlsx("Zambia_Economic_Data.xlsx")  # Employment, income inequality, poverty rates
mental_health_data <- read.xlsx("Zambia_Mental_Health_GBV.xlsx")  # Prevalence of depression, anxiety, GBV
gis_data <- read.xlsx("Zambia_GIS_Health_Facilities.xlsx")  # GIS and accessibility data
historical_data <- read.xlsx("Zambia_Historical_Health_Trend.xlsx")  # Disease burden, maternal health trends

#'*Check for missing data* 
# Summarize datasets to check for missing data
summary(dh_data)  # Check for missing data in DHS dataset
summary(health_facility_data)  # Check for missing data in health facility dataset
summary(economic_data)  # Check for missing data in economic dataset
summary(mental_health_data)  # Check for missing data in mental health dataset
summary(gis_data)  # Check for missing data in GIS dataset
summary(historical_data)  # Check for missing data in historical health data

#'*Handle missing data if needed* 
# Remove any rows with missing values
dh_data <- na.omit(dh_data)  # Remove missing data in DHS dataset
health_facility_data <- na.omit(health_facility_data)  # Remove missing data in health facility dataset
economic_data <- na.omit(economic_data)  # Remove missing data in economic dataset
mental_health_data <- na.omit(mental_health_data)  # Remove missing data in mental health dataset
gis_data <- na.omit(gis_data)  # Remove missing data in GIS dataset
historical_data <- na.omit(historical_data)  # Remove missing data in historical data

#'*Data Preprocessing for Spatial Model* 
# Preparing spatial data for health facility analysis
coordinates(health_facility_data) <- ~longitude+latitude  # Set coordinates for health facilities
proj4string(health_facility_data) <- CRS("+proj=longlat +datum=WGS84")  # Define the coordinate reference system

# Spatial weights matrix for the nearest neighbors using Euclidean distance
gis_coords <- cbind(gis_data$longitude, gis_data$latitude)  # Prepare coordinates for accessibility analysis
neighbors <- dnearneigh(gis_coords, 0, 100)  # Define a 100 km threshold for neighbors
W <- nb2listw(neighbors, style = "W")  # Create spatial weight matrix for spatial autocorrelation

# Visualize the spatial distribution of health facilities
plot(health_facility_data)  # Plot health facility locations

#'*Calculations for Health Burdens* 
# Calculating health burdens with multidimensional women’s health index

# Normalizing the data for the composite index
dh_data$normalized_maternal_mortality <- scale(dh_data$maternal_mortality)  # Normalize maternal mortality data
dh_data$normalized_contraceptive_use <- scale(dh_data$contraceptive_use)  # Normalize contraceptive use data
dh_data$normalized_reproductive_health <- scale(dh_data$reproductive_health)  # Normalize reproductive health data

# Create the Multidimensional Health Index (WHI)
dh_data$WHI <- rowMeans(dh_data[, c("normalized_maternal_mortality", 
                                    "normalized_contraceptive_use", 
                                    "normalized_reproductive_health")], na.rm = TRUE)  # Combine indicators into WHI

# Plot the WHI by province
ggplot(dh_data, aes(x = province, y = WHI)) +  # Plot WHI by province
  geom_bar(stat = "identity") +  # Bar plot for each province
  theme_minimal() +  # Apply minimal theme
  labs(title = "Women’s Health Index by Province")  # Add title to the plot

#'*Creating Gender-sensitive health burden model (Adjusted DALY/QALY)* 
# Gender-sensitive health burden model, incorporating GBV, mental health, and reproductive health
mental_health_data$adjusted_health_burden <- (mental_health_data$prevalence_of_depression + 
                                                mental_health_data$prevalence_of_anxiety) * 0.5 + 
  (mental_health_data$prevalence_of_GBV * 0.5)  # Adjust for depression, anxiety, and GBV

# Incorporate reproductive health burden (maternal mortality)
dh_data$adjusted_reproductive_burden <- dh_data$maternal_mortality * 0.7  # Adjust for maternal mortality burden

# Create Gender-Sensitive Health Burden Index
mental_health_data$gender_health_burden <- mental_health_data$adjusted_health_burden + 
  dh_data$adjusted_reproductive_burden  # Combine mental health and reproductive health burdens

# Visualize the adjusted health burden
ggplot(mental_health_data, aes(x = province, y = gender_health_burden)) +  # Plot health burden by province
  geom_bar(stat = "identity") +  # Bar plot for each province
  theme_minimal() +  # Apply minimalist theme
  labs(title = "Gender-Sensitive Health Burden by Province")  # Add title to the plot

#'*Creating Spatial-temporal modeling of Gender health disparities* 
# Performing spatial-temporal analysis, combining health disparities and forecasting

# Fit a time-series ARIMA model to historical health data
historical_health_ts <- ts(historical_data$maternal_mortality, frequency = 12, start = c(2000, 1))  # Create time-series object for maternal mortality data
arima_model <- auto.arima(historical_health_ts)  # Fit ARIMA model to historical data

# Forecast future health trends (next 5 years)
forecast_result <- forecast(arima_model, h = 5)  # Forecast the next 5 years of maternal mortality
plot(forecast_result, main = "Forecasted Maternal Mortality")  # Plot forecasted results

# Spatial clustering analysis for health disparities (using Moran's I)
moran.test(dh_data$maternal_mortality, listw = W)  # Run Moran's I test for spatial autocorrelation

#'*Creating predictive model analytics for women’s healthcare* 
# Predictive analytics for forecasting healthcare demand using machine learning

# Prepare data for training the predictive model
predictive_data <- dh_data %>%
  select(province, maternal_mortality, contraceptive_use, reproductive_health, economic_status)  # Select relevant columns for prediction

# Split the data into training and test sets
set.seed(123)  # Set random seed for reproducibility
trainIndex <- createDataPartition(predictive_data$maternal_mortality, p = 0.8, list = FALSE)  # Split data into training and test sets
train_data <- predictive_data[trainIndex,]  # Training data
test_data <- predictive_data[-trainIndex,]  # Test data

# Train a Random Forest model to predict maternal mortality
rf_model <- randomForest(maternal_mortality ~ contraceptive_use +
                           reproductive_health + economic_status,data = train_data)  # Train model on selected variables

# Predict healthcare demand (e.g., maternal health services)
predicted_demand <- predict(rf_model, test_data)  # Predict maternal health services demand

# Visualize predicted healthcare demand
ggplot(test_data, aes(x = maternal_mortality, y = predicted_demand)) +  # Scatter plot comparing actual vs predicted demand
  geom_point() +  # Plot data points
  geom_smooth(method = "lm") +  # Add linear trend line
  labs(title = "Predicted Healthcare Demand vs Actual Demand")  # Add title to the plot

#'*Creating the integrated final model* 
# Integrating spatial-temporal forecasting,
#predictive analytics, and gender-sensitive health burdens into one model

# Combine all metrics into a final data frame for analysis
final_model_data <- data.frame(province = dh_data$province,  # Province names from DHS data
                               WHI = dh_data$WHI,  # Women’s Health Index from DHS data
                               health_burden = mental_health_data$gender_health_burden,  # Gender-sensitive health burden from mental health data
                               predicted_demand = predicted_demand)  # Predicted healthcare demand from predictive model

#'*Add spatial-temporal components into the final model* 
final_model_data$forecasted_maternal_mortality <- forecast_result$mean  # Adding ARIMA forecasted maternal mortality
final_model_data$moran_i_value <- moran_result$estimate[1]  # Adding Moran's I value for spatial autocorrelation

#'*Create final health disparity and demand scores* 
final_model_data$final_score <- rowMeans(final_model_data[, c("WHI", "health_burden", "predicted_demand", 
                                                              "forecasted_maternal_mortality", "moran_i_value")], na.rm = TRUE)  # Calculate final score

#'*Visualizing integrated results with spatial-temporal components* 
ggplot(final_model_data, aes(x = province, y = final_score)) +  # Plot final integrated score by province
  geom_bar(stat = "identity") +  # Bar plot
  theme_minimal() +  # Minimalist theme
  labs(title = "Integrated Health Disparities and Healthcare Demand by Province (With Spatial-Temporal Analysis)")  # Add title

#'*Identifying provinces with extreme health disparities and high demand* 
high_disparity_provinces <- final_model_data %>%  # Filter provinces with top 25% final score
  filter(final_score > quantile(final_model_data$final_score, 0.75))  # Top 25% threshold

#'*Visualizing provinces with extreme disparities and demand* 
ggplot(high_disparity_provinces, aes(x = province, y = final_score)) +  # Plot high disparity provinces
  geom_bar(stat = "identity", fill = "red") +  # Highlight in red
  theme_minimal() +  # Minimalist theme
  labs(title = "Provinces with Extreme Health Disparities and Healthcare Demand")  # Add title