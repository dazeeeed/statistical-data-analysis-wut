# File: estimation.R
# Estimation of lambda parameter of exponential distribution. Quality of maximum
# likelihood estimator (MLE).


# Install pacman if not available and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, rio, here, ggplot2, dplyr, tidyr, stringr, MASS, pracma)

lambda <- 13 / 2
#sample_sizes <- c(1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7)

REPETITIONS <- 1000 # how many tries for one sample_size
HIST_LENGTH <- 50 # how many breaks in histogram
SAMPLE_LEN <- 20  # how many different sample_sizes
sample_sizes <- logseq(1e1, 1e5, n=SAMPLE_LEN)
bias <- c()
lambda_hat <- c()
sigma_mle <- c()
fisher_info <- c()
cr_lowerbound <- c()

hist_mids <- matrix(0, nrow=REPETITIONS, ncol=HIST_LENGTH-1)
hist_density <- matrix(0, nrow=REPETITIONS, ncol=HIST_LENGTH-1)


for(i in 1:length(sample_sizes)){
#for(i in length(sample_sizes)){
  sample_size = sample_sizes[i]
  print(sample_size)
        
  j_bias <- c()
  j_lambda_est <- c()
  j_lambda_hat <- c()
  j_sigma_mle <- c()
  j_fisher_info <- c()
  j_cr_lowerbound <- c()
  for(j in 1:REPETITIONS){
    x <- rexp(n=sample_size, rate=lambda)
    j_lambda_hat[j] <- 1 / mean(x)
    j_bias[j] <- j_lambda_hat[j] - lambda
    
    j_fisher_info[j] <- 1 / j_lambda_hat[j]^2
    j_sigma_mle[j] <- mean((j_lambda_hat[j] - lambda)^2)
    j_cr_lowerbound[j] <- lambda^2 / sample_size
    
  }
  lambda_hat[i] <- mean(j_lambda_hat)
  bias[i] <- mean(j_bias)
  fisher_info[i] <- mean(j_fisher_info)
  sigma_mle[i] <- mean(j_sigma_mle)
  cr_lowerbound[i] <- mean(j_cr_lowerbound)
  
  x <- sqrt(sample_size * fisher_info[i]) * (j_lambda_hat - lambda)
  break_seq <- seq(min(x), max(x), length.out = HIST_LENGTH)
  hist_mids[i,] <- hist(x, plot=FALSE, breaks=break_seq)$mids
  hist_density[i,] <- hist(x, plot=FALSE, breaks=break_seq)$density
}
sqrt(sigma_mle[20])
hist(sqrt(1e5 * fisher_info[20]) * (j_lambda_hat-lambda), prob=TRUE)
lines(x_normal, dnorm(x_normal, mean=0, sd=1))
sqrt(sigma_mle[20])

par(oma=c(1, 1, 1, 1))
plot_scale_factor <- 1.2
plot(log(sample_sizes, base=10), bias, type="b", xlab="log10(Liczność próby)",
     ylab="Obciążenie estymatora lambda",
     cex.lab=plot_scale_factor, 
     cex.axis=plot_scale_factor, 
     cex.main=plot_scale_factor, 
     cex.sub=plot_scale_factor
)
#lines(x=c(1,6), y=c(0,0), lty="dashed", col="red")

plot(log(sample_sizes, base=10), sigma_mle, type="b", 
     xlab="log10(Liczność próby)",
     ylab="Wartość",
     cex.lab=plot_scale_factor, 
     cex.axis=plot_scale_factor, 
     cex.main=plot_scale_factor, 
     cex.sub=plot_scale_factor
)
lines(log(sample_sizes, base=10), cr_lowerbound, type="b", col="red")
legend(3.75, 6, legend=c("Wariancja", "Kres Cramera-Rao"), 
       col=c("black", "red"), lty=c(1,1), cex=plot_scale_factor)


colors <- c("green", "red", "black", "orange", "magenta", "blue", "forestgreen")
c_i <- 1

lo <- loess(hist_density[1,] ~ hist_mids[1,])
plot(hist_mids[1,], predict(lo), type='l', xlim=c(-5, 5), ylim=c(0, 0.4), 
     col=colors[1], lwd=2, xlab="sqrt(n*I(lambda)) * (hat{lambda} - lambda)",
     ylab="Wartość")


for(i in c(6, 9, 14, 18, 20)){
  c_i = c_i + 1
  sample_size = sample_sizes[i]
  print(sample_size)
  
  lo <- loess(hist_density[i,] ~ hist_mids[i,])
  lines(hist_mids[i,], predict(lo), col=colors[c_i], lwd=2)
}

x_normal <- seq(from=-20, to=20, by=0.1)
lines(x_normal, dnorm(x_normal, mean=0, sd=1), col=colors[7], lty=3, lwd=4)
legend(3, 0.35, 
       legend=c("n=10", "n=110", "n=480", "n=5500", "n=38000", "n=100000", 
                "Normal(0,1)"),  col=colors, lty=c(1,1), cex=plot_scale_factor)
