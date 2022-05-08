# File: climatic_data.R
# Analysis of climatic data from July 2021 available on website:
# https://danepubliczne.imgw.pl/data/dane_pomiarowo_obserwacyjne/dane_meteorologiczne/dobowe/klimat/


# Install pacman if not available and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, rio, here, ggplot2, dplyr)


# Data preparation
data_path <- here("project_01", "data", "k_d_07_2021.csv")

data <- rio::import(data_path)
# Status 8 means no measurement, status=9 no phenomenon 
col_names <- c("station_code", "station_name", "year", "month", "day", 
               "max_daily_temp", "status_tmax", "min_daily_temp", "status_tmin",
               "avg_daily_temp", "status_tavg", "ground_min_temp", 
               "status_tminground", "precip_daily_sum", "status_precipsum",
               "precip_direction", "snow_cover_height", "status_snowcover")

names(data) <- col_names
cities <- c("PSZCZYNA", "JAROCIN", "WARSZAWA-BIELANY")

data_cities <- data[data$station_name %in% cities, ]
# Factor station names for categorical data
data_cities$station_name <- as.factor(data_cities$station_name)


# data_city1 <- data[data$station_name == cities[1],]
# data_city2 <- data[data$station_name == cities[2],]
# data_city3 <- data[data$station_name == cities[3],]
# 
# boxplot(x=list(data_city1$max_daily_temp, 
#         data_city2$max_daily_temp, 
#         data_city3$max_daily_temp),
#         names=cities,
#         xlab="City", ylab="Temperature [\u00B0C]",
#         main="Maximum daily temperature by city"
# )

plot(data_cities$station_name, data_cities$max_daily_temp)

# Equivalent to plot
boxplot(data_cities$max_daily_temp ~ data_cities$station_name,
        xlab="City", ylab="Temperature [\u00B0C]",
        main="Maximum daily temperature by city"
)

# Get additional descriptive statistics using dplyr
additional_stats<- data_cities %>% 
  group_by(station_name) %>% 
  summarize(mean_max_daily_temp = mean(max_daily_temp),
            median_max_daily_temp = median(max_daily_temp))

additional_stats

data_cities$daily_temp_fluctuation <- data_cities$max_daily_temp - 
  data_cities$min_daily_temp


boxplot(data_cities$daily_temp_fluctuation ~ data_cities$station_name,
        xlab="City", ylab="Temperature difference [\u00B0C]",
        main="Daily temperature fluctuation by city"
)


?median


# CLEANUP
# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)

# Clear console
cat("\014")
