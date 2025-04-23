#Load packages
source("scripts/r prep2.r")

# Sample data: Maternal mortality rate per year
maternal_data <- ts(c(450, 420, 390, 370, 360, 340, 320, 310, 300, 290), start = 2013, frequency = 2)

# Fit ARIMA model
arima_model <- auto.arima(maternal_data)

# Forecast for the next 5 years
forecast_values <- forecast(arima_model, h = 5)

# Plot the forecast
autoplot(forecast_values) + ggtitle("Maternal Mortality Rate Forecast") + ylab("Deaths per 100,000 births")




# Predictive Model for Maternal Health (Time-Series Forecasting with ARIMA)
# Simulated maternal health data (Monthly maternal deaths over 5 years)
maternal_health <- ts(c(50, 48, 55, 60, 52, 50, 49, 45, 47, 55, 58, 54,
                        53, 50, 57, 65, 61, 59, 56, 50, 48, 52, 58, 60,
                        62, 65, 68, 70, 72, 74, 78, 80, 85, 87, 90, 92,
                        95, 98, 100, 105, 110, 115, 120, 125, 130, 135,
                        140, 145, 150, 155, 160, 165, 170, 175, 180, 185,
                        190, 195, 200, 205), frequency = 12, start = c(2019, 1))

# Fit ARIMA model
model <- auto.arima(maternal_health)

# Forecast next 12 months
forecasted <- forecast(model, h = 12)

# Plot results
autoplot(forecasted) + ggtitle("Maternal Mortality Forecast (Next 12 Months)") + ylab("Maternal Deaths")



# Load necessary libraries
library(randomForest)
library(caret)

installed.packages('caret')

# Sample dataset (age, ANC visits, birth complications, maternal deaths)
set.seed(123)
data <- data.frame(
  Age = runif(100, 18, 40),
  ANC_Visits = sample(1:8, 100, replace = TRUE),
  Complications = sample(0:1, 100, replace = TRUE),
  Maternal_Death = sample(0:1, 100, replace = TRUE)
)

# Train Random Forest model
rf_model <- randomForest(Maternal_Death ~ ., data = data, ntree = 100, importance = TRUE)

# Model summary
print(rf_model)

# Feature importance
importance(rf_model)
varImpPlot(rf_model)


#Spatial Model for Women’s Healthcare Accessibility (Travel Time & Hotspot Analysis)
# Load libraries
library(sf)
library(osrm)

# Load sample data: Health facilities & patient locations
facilities <- st_as_sf(data.frame(id = 1:3, lon = c(28.3, 28.5, 28.7), lat = c(-15.4, -15.5, -15.6)), 
                       coords = c("lon", "lat"), crs = 4326)

patients <- st_as_sf(data.frame(id = 1:5, lon = c(28.35, 28.45, 28.55, 28.65, 28.75), 
                                lat = c(-15.45, -15.55, -15.65, -15.75, -15.85)), 
                     coords = c("lon", "lat"), crs = 4326)

# Compute travel time (in minutes) from patients to nearest facility
travel_time <- osrmTable(src = patients, dst = facilities)

# Display travel time matrix
print(travel_time$durations)

#Hotspot Analysis (High-Risk Maternal Mortality Areas)
library(sp)
library(spatstat)

# Load sample maternal death locations
deaths <- data.frame(lon = c(28.3, 28.5, 28.6, 28.7, 28.8), lat = c(-15.4, -15.5, -15.6, -15.7, -15.8))

# Create SpatialPoints object
sp_deaths <- SpatialPoints(deaths, proj4string = CRS("+proj=longlat +datum=WGS84"))

# Convert SpatialPoints to a point pattern object (ppp)
coords <- coordinates(sp_deaths)
ppp_deaths <- ppp(coords[,1], coords[,2], window = owin(c(min(coords[,1]), max(coords[,1])), 
                                                        c(min(coords[,2]), max(coords[,2]))))

# Perform hotspot analysis
death_density <- density.ppp(ppp_deaths, sigma = 0.1)

# Plot the hotspot map
plot(death_density, main = "Maternal Mortality Hotspot Analysis")



##Multidimensional Health Index for Women
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Define the provinces of Zambia
provinces <- c("Muchinga", "Northern", "Luapula", "Copperbelt", "Central", 
               "Southern", "Eastern", "North-Western", "Western", "Lusaka")

# Generate dummy data for health indicators for each province
data <- data.frame(
  Province = provinces,
  Physical_Health = c(65, 75, 55, 80, 70, 68, 72, 76, 64, 79),  # e.g., maternal mortality rate, life expectancy
  Mental_Health = c(60, 70, 50, 85, 65, 63, 78, 70, 60, 72),    # e.g., depression rate, anxiety
  Social_Wellbeing = c(70, 80, 65, 90, 75, 73, 77, 80, 74, 85), # e.g., education, gender violence
  Economic_Indicators = c(60, 65, 55, 75, 80, 70, 62, 68, 63, 90) # e.g., income inequality, poverty level
)

# Normalize each indicator to a scale of 0 to 100
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)) * 100)
}

data_normalized <- data %>%
  mutate(
    Physical_Health = normalize(Physical_Health),
    Mental_Health = normalize(Mental_Health),
    Social_Wellbeing = normalize(Social_Wellbeing),
    Economic_Indicators = normalize(Economic_Indicators)
  )

# Define weights for each indicator
weights <- c(Physical_Health = 0.3, Mental_Health = 0.2, Social_Wellbeing = 0.25, Economic_Indicators = 0.25)

# Calculate the health index for each province
data_normalized$Health_Index <- with(data_normalized, 
                                     Physical_Health * weights["Physical_Health"] +
                                       Mental_Health * weights["Mental_Health"] +
                                       Social_Wellbeing * weights["Social_Wellbeing"] +
                                       Economic_Indicators * weights["Economic_Indicators"])

# Display the results
print(data_normalized)


##Spatial-Temporal Modeling of Gender-Related Health Disparities
##simulate gender-related health data for Zambia’s provinces over time and apply spatial and time-series analysis

# Load necessary libraries
library(spdep)
library(forecast)
library(spatialreg)

# Set seed for reproducibility
set.seed(456)

# Define the provinces of Zambia
provinces <- c("Muchinga", "Northern", "Luapula", "Copperbelt", "Central", 
               "Southern", "Eastern", "North-Western", "Western", "Lusaka")

# Define time periods (e.g., 10 years of health data)
time_periods <- 10

# Create a spatial coordinates matrix for the provinces (simulated coordinates)
coords <- matrix(c(
  0, 0,  # Muchinga
  1, 0,  # Northern
  2, 0,  # Luapula
  3, 0,  # Copperbelt
  4, 0,  # Central
  5, 0,  # Southern
  6, 0,  # Eastern
  7, 0,  # North-Western
  8, 0,  # Western
  9, 0   # Lusaka
), ncol = 2, byrow = TRUE)

# Create a spatial weights matrix based on Euclidean distance (using 10 provinces)
neighbors <- dnearneigh(coords, 0, 2)  # Define neighbors within a distance of 2
W <- nb2listw(neighbors, style = "W")  # Weight matrix for spatial dependence

# Generate dummy health data (e.g., maternal mortality rate)
health_data <- data.frame(
  Province = rep(provinces, each = time_periods),
  Time = rep(1:time_periods, times = length(provinces)),
  Health_Outcome = c(
    rnorm(time_periods, mean = 50, sd = 5),  # Muchinga
    rnorm(time_periods, mean = 55, sd = 4),  # Northern
    rnorm(time_periods, mean = 60, sd = 6),  # Luapula
    rnorm(time_periods, mean = 48, sd = 5),  # Copperbelt
    rnorm(time_periods, mean = 52, sd = 4),  # Central
    rnorm(time_periods, mean = 50, sd = 5),  # Southern
    rnorm(time_periods, mean = 58, sd = 4),  # Eastern
    rnorm(time_periods, mean = 62, sd = 5),  # North-Western
    rnorm(time_periods, mean = 60, sd = 5),  # Western
    rnorm(time_periods, mean = 67, sd = 5)   # Lusaka
  )
)

# Ensure the dimensions of data match the spatial weights matrix
# Aggregate health data to one value per province (e.g., average over time)
health_data_agg <- aggregate(Health_Outcome ~ Province, data = health_data, FUN = mean)

# Now, fit the spatial lag model using the aggregated health data
slm <- lagsarlm(Health_Outcome ~ 1, data = health_data_agg, listw = W)

# Summary of the model
summary(slm)

# Forecasting with ARIMA (time-series forecasting for each province)
# First, let's create a time series for the health outcomes of Lusaka
lusaka_health <- health_data$Health_Outcome[health_data$Province == "Lusaka"]

# Fit ARIMA model for forecasting
arima_model <- auto.arima(lusaka_health)

# Forecast the next 5 time periods for Lusaka
forecast_result <- forecast(arima_model, h = 5)
print(forecast_result)

# Plot the forecasted values
plot(forecast_result)
