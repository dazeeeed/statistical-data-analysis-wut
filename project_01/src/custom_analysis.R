# File: custom_analysis.R
# Analysis of climatic data from 2021 available on website:
# https://danepubliczne.imgw.pl/data/dane_pomiarowo_obserwacyjne/dane_meteorologiczne/dobowe/klimat/


# Install pacman if not available and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, rio, here, ggplot2, dplyr, tidyr, stringr, MASS)

here()
# Data preparation
data_path <- here("project_01", "data", "2021", "k_d_2021.csv")

data <- rio::import(data_path)
# Status 8 means no measurement, status=9 no phenomenon 
col_names <- c("station_code", "station_name", "year", "month", "day", 
               "max_daily_temp", "status_tmax", "min_daily_temp", "status_tmin",
               "avg_daily_temp", "status_tavg", "ground_min_temp", 
               "status_tminground", "precip_daily_sum", "status_precipsum",
               "precip_direction", "snow_cover_height", "status_snowcover")

names(data) <- col_names

city <- "PSZCZYNA"

data_city <- data[data$station_name == city, ]

data_city_monthly <- data_city %>% 
  group_by(month) %>% 
  summarize(mean_avg_daily_temp = mean(avg_daily_temp),
            median_avg_daily_temp = median(avg_daily_temp))


plot(data_city_monthly$month, data_city_monthly$mean_avg_daily_temp)


# CLEANUP
# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)

# Clear console
cat("\014")
