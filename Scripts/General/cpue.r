# 📌 Load required libraries
library(tidyverse)
library(ggplot2)

# 📊 Simulate Bangweulu Wetlands fishery data
set.seed(42)
cpue_data <- expand.grid(
  district = c("Samfya", "Lunga", "Chifunabuli", "Lavushimanda", "Mpika"),
  year = 2015:2024
) %>%
  mutate(
    effort_days = sample(30:70, n(), replace = TRUE),
    catch_kg = round(effort_days * runif(n(), 6, 12)),  # realistic CPUE ~ 6–12 kg/day
    cpue = catch_kg / effort_days
  )

# 🧮 Inspect the data
head(cpue_data)

# 📈 Plot CPUE trends over time by district
ggplot(cpue_data, aes(x = year, y = cpue, color = district)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(
    title = "CPUE Trends in Bangweulu Wetlands (2015–2024)",
    x = "Year", y = "CPUE (kg/day)"
  ) +
  theme_minimal()

# 🔍 Fit a linear model (LM)
lm_model <- lm(cpue ~ year + district + effort_days, data = cpue_data)
summary(lm_model)

# 🔍 Fit a generalized linear model (GLM)
glm_model <- glm(cpue ~ year + district + effort_days, data = cpue_data, family = gaussian())
summary(glm_model)

# 🧾 Predict future CPUE (e.g., 2025)
future_data <- data.frame(
  year = 2025,
  district = c("Samfya", "Lunga", "Chifunabuli", "Lavushimanda", "Mpika"),
  effort_days = rep(60, 5)
)

future_data$predicted_cpue <- predict(glm_model, newdata = future_data)

print(future_data)
