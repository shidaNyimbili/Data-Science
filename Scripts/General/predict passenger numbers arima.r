source("Scripts/General/r prep2.r")

install.packages("tmap")

# Step 2: Load the dataset
ts_data <- read.xslx("Data/Airpassengers.xslx")

data("AirPassengers")
ts_data <- AirPassengers

ts_data

colnames(ts_data)

plot(ts_data, main = "AirPassengers Data", ylab = "Passengers", xlab = "Year")

# Step 3: Check for stationarity using Augmented Dickey-Fuller Test
adf.test(ts_data)

# Step 4: If non-stationary, difference the series
diff_ts <- diff(ts_data)
plot(diff_ts, main = "First Differenced Series")

# Optional: Check stationarity again
adf.test(diff_ts)

# Step 5: Fit ARIMA model automatically
model <- auto.arima(ts_data)
summary(model)

# Step 6: Forecast future values
forecasted <- forecast(model, h = 12)  # Forecasting next 12 months
plot(forecasted, main = "ARIMA Forecast of AirPassengers")

# Step 7: Print forecast values
print(forecasted)
