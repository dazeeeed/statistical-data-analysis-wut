# File: climatic_data.R
# Analysis of climatic data from July 2021 available on website:
# https://danepubliczne.imgw.pl/data/dane_pomiarowo_obserwacyjne/dane_meteorologiczne/dobowe/klimat/


# Install pacman if not available and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, rio, here)


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

data_pszczyna <- data[data$station_name == "PSZCZYNA",]
data_jarocin <- data[data$station_name == "JAROCIN",]
data_warszawa <- data[data$station_name == "WARSZAWA-BIELANY",]



# CLEANUP
# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)

# Clear console
cat("\014")
