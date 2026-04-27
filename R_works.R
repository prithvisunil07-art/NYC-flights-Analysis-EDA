# ================================
# 1. Load Libraries
# ================================
install.packages(c("nycflights13", "dplyr", "ggplot2", "skimr"))

library(nycflights13)
library(dplyr)
library(ggplot2)
library(skimr)

# ================================
# 2. Load Data
# ================================
data("flights")

# ================================
# 3. Initial Exploration
# ================================
glimpse(flights)
summary(flights)
skim(flights)

# ================================
# 4. Missing Value Check
# ================================
colSums(is.na(flights))

# ================================
# 5. Data Cleaning
# ================================
flights_clean <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))

# ================================
# 6. Feature Engineering
# ================================
flights_clean <- flights_clean %>%
  mutate(
    speed = distance / air_time * 60,
    delay_status = ifelse(arr_delay > 15, "Delayed", "On Time")
  )

# ================================
# 7. Summary Statistics
# ================================
flights_clean %>%
  summarise(
    avg_dep_delay = mean(dep_delay),
    avg_arr_delay = mean(arr_delay),
    avg_speed = mean(speed)
  )

# ================================
# 8. Airline Performance
# ================================
airline_performance <- flights_clean %>%
  group_by(carrier) %>%
  summarise(
    avg_delay = mean(arr_delay),
    total_flights = n()
  ) %>%
  arrange(avg_delay)

print(airline_performance)

# ================================
# 9. Visualization
# ================================

# Histogram
ggplot(flights_clean, aes(x = arr_delay)) +
  geom_histogram(binwidth = 10, fill = "steelblue") +
  labs(title = "Arrival Delay Distribution")

# Scatter Plot
ggplot(flights_clean, aes(x = distance, y = arr_delay)) +
  geom_point(alpha = 0.3) +
  labs(title = "Distance vs Arrival Delay")

# Boxplot
ggplot(flights_clean, aes(x = carrier, y = arr_delay)) +
  geom_boxplot() +
  labs(title = "Airline vs Arrival Delay")

# ================================
# 10. Correlation Analysis
# ================================
numeric_data <- flights_clean %>%
  select_if(is.numeric)

cor(numeric_data, use = "complete.obs")


