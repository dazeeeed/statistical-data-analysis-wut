# File: climatic_data.R
# Analysis of climatic data from July 2021 available on website:
# https://danepubliczne.imgw.pl/data/dane_pomiarowo_obserwacyjne/dane_meteorologiczne/dobowe/klimat/


# Install pacman if not available and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, rio, here, ggplot2, dplyr, tidyr, stringr, MASS, pracma)

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


################################################################################
# Check differences between max_daily_temp for different cities
################################################################################

# Plot
# plot(data_cities$station_name, data_cities$max_daily_temp)
pdf(file=here("project_01", "data", "graphics", 
              "max_daily_temp_box.pdf"),
    width = 7,
    height = 5,
    onefile = TRUE
)
par(oma=c(1, 1, 1, 1))

boxplot(data_cities$max_daily_temp ~ data_cities$station_name,
        xlab="City", ylab="Temperature (Tmax) [\u00B0C]",
        main="Maximum daily temperature by city"
)
dev.off()

# Get additional descriptive statistics using dplyr
additional_stats<- data_cities %>% 
  group_by(station_name) %>% 
  summarize(mean_max_daily_temp = mean(max_daily_temp),
            median_max_daily_temp = median(max_daily_temp))

additional_stats

################################################################################
# Check daily temperature fluctuations (day-night)
################################################################################
data_cities$daily_temp_fluctuation <- data_cities$max_daily_temp - 
  data_cities$min_daily_temp

# Plot
pdf(file=here("project_01", "data", "graphics", 
              "max_daily_temp_diff_box.pdf"),
    width = 7,
    height = 5,
    onefile = TRUE
)
par(oma=c(1, 1, 1, 1))

boxplot(data_cities$daily_temp_fluctuation ~ data_cities$station_name,
        xlab="City", ylab="Temperature difference (Tmax-Tmin) [\u00B0C]",
        main="Daily temperature fluctuation by city"
)
dev.off()

################################################################################
# Check how does max_daily_temp change day by day
################################################################################
data_cities <- data_cities %>% 
  group_by(station_name) %>%
  mutate(max_daily_temp_day_change = max_daily_temp - lag(max_daily_temp, 1))

data_cities$max_daily_temp_day_change <- tidyr::replace_na(
  data_cities$max_daily_temp_day_change, 0)

# Plots
pdf(file=here("project_01", "data", "graphics", 
              "max_daily_temp_change_daily_box.pdf"),
    width = 7,
    height = 5,
    onefile = TRUE
)
par(oma=c(1, 1, 1, 1))

boxplot(data_cities$max_daily_temp_day_change ~ data_cities$station_name,
        xlab="City", ylab="Temperature (Tmax_tomorrow-Tmax_today) [\u00B0C]",
        main="Day by day max temperature fluctuation by city"
)
dev.off()

pdf(file=here("project_01", "data", "graphics", 
              "max_daily_temp_change_daily_hist.pdf"),
    width = 7,
    height = 7,
    onefile = TRUE
)

par(mfrow = c(3, 1), oma=c(1, 1, 1, 1))
x_range <- c(-10, 10)
hist_breaks <- seq(from=-10, to=10, by=1)
colors <- c("red", "purple", "blue")
plot_scale_factor <- 1.5

for(i in 1:length(cities)){
  hist(data_cities$max_daily_temp_day_change[data_cities$station_name == 
                                               cities[i]],
       xlim = x_range,
       breaks = hist_breaks,
       main = paste("Frequency of day by day change in max daily temperature", 
                    "in", str_to_sentence(cities[i])),
       xlab = "Temperature (Tmax_tomorrow-Tmax_today) [\u00B0C]",
       col = colors[i],
       cex.lab=plot_scale_factor, 
       cex.axis=plot_scale_factor, 
       cex.main=plot_scale_factor, 
       cex.sub=plot_scale_factor)
}
dev.off()

################################################################################
# More detailed analysis of max daily temp change in Warszawa-Bielany
################################################################################
?hist
data_warsaw = data_cities$max_daily_temp_day_change[data_cities$station_name == 
                                                      "WARSZAWA-BIELANY"]

fit <- MASS::fitdistr(data_warsaw, densfun = "normal")
?MASS::fitdistr

# Plot
pdf(file=here("project_01", "data", "graphics", 
              "max_daily_temp_change_daily_hist_WARSZAWA.pdf"),
    width = 7,
    height = 5,
    onefile = TRUE
)
par(oma=c(1, 1, 1, 1))

hist(data_warsaw,
     xlim = x_range,
     breaks = hist_breaks,
     main = paste("Probability density of day by day change in max daily\n temperature", 
                  "in Warszawa-Bielany"),
     xlab = "Temperature (Tmax_tomorrow-Tmax_today) [\u00B0C]",
     prob = TRUE
)
curve(dnorm(x, fit$estimate[1], fit$estimate[2]), col = "red", add = TRUE)
dev.off()

# Calculate probability of max daily temp change in range <-1, 1>
temp_range <- c(-1, 1)
1/2 * (erf((temp_range[2] - fit$estimate[1]) / (fit$estimate[2] * sqrt(2))) - 
  erf((temp_range[1] - fit$estimate[1]) / (fit$estimate[2] * sqrt(2))))





# Restore graphic parameter
par(mfrow=c(1, 1))
rm(x_range, hist_breaks)


# CLEANUP
# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)

# Clear console
cat("\014")
