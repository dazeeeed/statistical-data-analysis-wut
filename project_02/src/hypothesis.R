# File: estimation.R
# Veritication of statistical hypothesis.

if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, rio, here, ggplot2, dplyr, tidyr, stringr, MASS, pracma)


SAMPLE_LEN <- 50
REALIZATIONS <- 100
alpha <- 0.01

p_value_means <- c()
p_value_means_per_var <- data.frame("0.5" = 1:50, "1" = 1:50, "2"= 1:50)
mu_values <- seq(from=0, to=0.1, length.out=SAMPLE_LEN)

std_dev <- c(0.1, 0.5, 1, 2)
sample_size <- 10000

for(k in 1:length(std_dev)){
  for(i in 1:length(mu_values)){
    p_value_temp <- c()
    
    for(j in 1:REALIZATIONS){
      x <- rnorm(sample_size, mean=mu_values[i], sd=std_dev[k])
      test <- t.test(x, mu=0, conf.level=(1 - alpha))
      p_value_temp[j] <- test$p.value
    }
    p_value_means[i] = mean(p_value_temp)
  }
  p_value_means_per_var[,k] <- p_value_means
}

colors <- c("green", "red", "black", "orange", "magenta", "blue", "forestgreen")
c_i <- 1
plot(mu_values, p_value_means_per_var[,1], xlab="mu_x", ylab="p-value", "l", 
     col=colors[1], lwd=2)
for(i in 2:length(std_dev)){
  c_i = c_i + 1
  lines(mu_values, p_value_means_per_var[,i], "l", col=colors[c_i], lwd=2)
}
lines(c(min(mu_values), max(mu_values)), c(alpha, alpha), col="forestgreen", 
      lty=3, lwd=4)
legend(0.065, 0.4, legend=c("std_dev=0.1", "std_dev=0.5", "std_dev=1", "std_dev=2"),  
       col=colors, lty=c(1,1,1,1))
title(main =list("P-value w zaleznosci od mu_x (n=10000)", cex=0.9), line=0.5)

