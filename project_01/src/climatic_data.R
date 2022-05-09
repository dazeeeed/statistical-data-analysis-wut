# File: climatic_data.R
# Analysis of climatic data from July 2021 available on website:
# https://danepubliczne.imgw.pl/data/dane_pomiarowo_obserwacyjne/dane_meteorologiczne/dobowe/klimat/


# Install pacman if not available and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, rio, here, ggplot2, dplyr, tidyr, stringr)

here()
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


###############################################################
# Check differences between max_daily_temp for different cities
###############################################################
#plot(data_cities$station_name, data_cities$max_daily_temp)
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

###############################################################
# Check daily temperature fluctuations (day-night)
###############################################################
data_cities$daily_temp_fluctuation <- data_cities$max_daily_temp - 
  data_cities$min_daily_temp


boxplot(data_cities$daily_temp_fluctuation ~ data_cities$station_name,
        xlab="City", ylab="Temperature difference [\u00B0C]",
        main="Daily temperature fluctuation by city"
)

###############################################################
# Check how does max_daily_temp change day by day
###############################################################
data_cities <- data_cities %>% 
  group_by(station_name) %>%
  mutate(max_daily_temp_day_change = max_daily_temp - lag(max_daily_temp, 1))

data_cities$max_daily_temp_day_change <- tidyr::replace_na(
  data_cities$max_daily_temp_day_change, 0)


boxplot(data_cities$max_daily_temp_day_change ~ data_cities$station_name,
        xlab="City", ylab="Temperature [\u00B0C]",
        main="Day by day max temperature fluctuation by city"
)

# Put graphs in 3 rows and 1 column
par(mfrow = c(3, 1))
x_range <- c(-10, 10)
hist_breaks <- seq(from=-10, to=10, by=1)
colors <- c("red", "purple", "blue")
plot_scale_factor <- 1

# Histograms for each species using options
# ?pdf
# pdf(file="freq_max_daily_temp_change.pdf", 
#     width = 7, 
#     height = 7, 
#     units="in", 
#     noRStudioGD = TRUE
# )

for(i in 1:length(cities)){
  hist(data_cities$max_daily_temp_day_change[data_cities$station_name == 
                                               cities[i]],
       xlim = x_range,
       breaks = hist_breaks,
       main = paste("Frequency of day by day change in max daily temperature", 
                    "in", str_to_sentence(cities[i])),
       xlab = "Temperature difference [\u00B0C]",
       col = colors[i],
       cex.lab=plot_scale_factor, 
       cex.axis=plot_scale_factor, 
       cex.main=plot_scale_factor, 
       cex.sub=plot_scale_factor)
}
dev.off()

# Restore graphic parameter
par(mfrow=c(1, 1))
rm(x_range, hist_breaks)


?median
?rm

# CLEANUP
# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)

# Clear console
cat("\014")
