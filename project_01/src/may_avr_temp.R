# File: custom_analysis.R
# Analysis of climatic data from 2021 available on website:
# https://danepubliczne.imgw.pl/data/dane_pomiarowo_obserwacyjne/dane_meteorologiczne/dobowe/klimat/


# Install pacman if not available and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, rio, here, ggplot2, dplyr, tidyr, stringr, MASS)

library('dplyr')
library('here')
library(fitdistrplus)

here()
# Data preparation
data_path <- here("k_d_05.csv")

data <- rio::import(data_path)
# Status 8 means no measurement, status=9 no phenomenon 
col_names <- c("station_code", "station_name", "year", "month", "day", 
               "max_daily_temp", "status_tmax", "min_daily_temp", "status_tmin",
               "avg_daily_temp", "status_tavg", "ground_min_temp", 
               "status_tminground", "precip_daily_sum", "status_precipsum",
               "precip_direction", "snow_cover_height", "status_snowcover")

names(data) <- col_names

city <- "PSZCZYNA"

data_city <- data[data$station_name == city,]


t = seq(0, 25, by = 1)
hist(data_city$avg_daily_temp, breaks = t, col="gray", labels = TRUE,
     main="Średnia dzienna temperatura w maju (2001-2021)",
     xlab="Srednia dzienna temperatura [℃]",
     ylab="Liczba dni o danej temeraturze")


fit_sub <- fitdistr(data_city$avg_daily_temp, "poisson")
lambda_sub <- fit_sub$estimate


t = seq(0, 25, by = 1)
hist(data_city$avg_daily_temp, breaks = t, col="gray", labels = TRUE,
     main="Rozkład prawdopodobieństwa średnich dizennych temperatur w maju (2001-2021)",
     ylab="Prawdopodobieństwo",
     xlab="Srednia dzienna temperatura [℃]",
     prob = TRUE)
lines(t, dpois(t, lambda = lambda_sub), col='red', lwd=3)

print(lambda_sub)


# CLEANUP
# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)

# Clear console
cat("\014")