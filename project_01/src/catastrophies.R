library(fitdistrplus)

dataset <- read.csv(
  file = "katastrofy.csv",
  header = TRUE,
  sep = ",",
  dec = ".",
  encoding="UTF-8",
  stringsAsFactors=FALSE
)
head(dataset[c("Date", "Time", "Location", "Type")])


dataset$Date <- as.Date(dataset$Date, format = "%m/%d/%Y")

dataset$Quarters = paste(format(dataset$Date, format = "%Y"),quarters(dataset$Date), sep="_")
head(dataset[c("Quarters", "Date", "Time", "Location", "Type")])


occur = table(dataset$Quarters)
occur_df = as.data.frame(occur) 
colnames(occur_df) <- c('Quarter','Crash_cnt')
occur_df

plot(occur_df)
t = seq(0, 40, by = 1)
hist(occur_df$Crash_cnt, breaks = t, col="gray", labels = TRUE,
     main="liczba katastrof na kwartał",
     ylab="Ilość wystąpień",
     xlab="Liczba katastrof w kwartale"
  )


all_q = data.frame(expand.grid(Year = seq(
  as.integer(min(format(dataset$Date, format = "%Y"))),
  as.integer(max(format(dataset$Date, format = "%Y"))),
  1), 
  Quart = c("Q4","Q3","Q2","Q1")),
  Count = 0
)

all_q$Quarter = paste(all_q$Year, all_q$Quart, sep="_")

all_m = merge(all_q, occur_df, by = "Quarter", all.x = TRUE)


all_m$Sum_cnt =  rowSums(cbind(all_m$Count, all_m$Crash_cnt), na.rm = TRUE)

t = seq(0, 40, by = 1)
hist(all_m$Sum_cnt, breaks = t, col="gray", labels = TRUE,
     main="Liczba katastrof na kwartał(z uwzględnieniem kwartałów bez katastrof)",
     ylab="Ilość wystąpień",
     xlab="Liczba katastrof w kwartale")
plot(all_m$Sum_cnt)


#rozklad Poisson
fit <- fitdistr(all_m$Sum_cnt, "poisson")
lambda <- fit$estimate
print(lambda)

t = seq(0, 40, by = 1)
hist(
  all_m$Sum_cnt,
  breaks = t,
  prob = TRUE,
  labels = TRUE,
  main="Prawdopodobieństwo katastrofy na tle rozkładu Poissona",
  xlab="Liczba katastrof na kwartał",
  ylab="Prawdopodobieństwo wystąpienia"
)
lines(t, dpois(t, lambda = lambda), col='red', lwd=3)


all_m_sub <- subset(all_m, Year >= 1930)
fit_sub <- fitdistr(all_m_sub$Sum_cnt, "poisson")
lambda_sub <- fit_sub$estimate
print(lambda_sub)
hist(
  all_m_sub$Sum_cnt,
  breaks = t,
  labels = TRUE,
  prob = TRUE,
  main="Prawdopodobieństwo katastrofy na tle rozkładu Poissona(zbiór ograniczony)",
  xlab="Liczba wypadków na kwartał",
  ylab="Prawdopodobieństwo wystąpienia",
  ylim= c(0,0.1)
)
lines(t, dpois(t, lambda = lambda_sub), col='red', lwd=3)


#Jak zmieniała się liczba wypadków w kolejnych latach? Czy latanie staje się bardziej bezpieczne? 

dataset_nof <- read.csv(
  file = "number-of-flights-from-2.csv",
  header = TRUE,
  sep = ";",
  dec = ".",
  encoding="UTF-8",
  stringsAsFactors=FALSE
)
head(dataset_nof[c("Year", "NumOfFl")])


dataset$Year = format(dataset$Date, format = "%Y")

occur_all = table(dataset$Year)
occur_all_df = as.data.frame(occur_all) 
colnames(occur_all_df) <- c('Year','Crash_cnt')
occur_all_df$Year <- as.numeric(as.character(occur_all_df$Year))


y_o <- c(occur_all_df$Crash_cnt)
x_o <- (1:length(y_o))
plot(
  x_o,
  y_o,
  #occur_all_df,
  main="Liczba katastrofy w latach",
  xlab="Rok",
  ylab="Liczba wypadków",
  pch = 19,
  xaxt = "n"
)
axis(1, at=1:length(y_o), labels=occur_all_df$Year)
lines(predict(lm(y_o~ poly(x_o,4))),col='red')


#x_l <-dataset_nof$Year
#y_l <-dataset_nof$NumOfFl
y_l <- c(dataset_nof$NumOfFl)
x_l <- (1:length(y_l))
plot(  x_l,
       y_l,
       main="Liczba lotów w latach",
       xlab="Rok",
       ylab="Liczba lotów [1e6]",
       pch = 19, xaxt = "n")
axis(1, at=1:length(y_l), labels=seq(2004, 2021, 1))
lines(predict(lm(y_l~ poly(x_l,3))),col='green')


dataset_non_covid <- subset(dataset_nof, Year < 2020)
#x_l <-dataset_nof$Year
#y_l <-dataset_nof$NumOfFl
y_l <- c(dataset_non_covid$NumOfFl)
x_l <- (1:length(y_l))
plot(  x_l,
       y_l,
       main="Liczba lotów w latach",
       xlab="Rok",
       ylab="Liczba lotów [1e6]",
       pch = 19, xaxt = "n",
       ylim=c(15,40))
axis(1, at=1:length(y_l), labels=seq(2004, 2019, 1))
lines(predict(lm(y_l~x_l)),col='red')



dataset_04_09 <- subset(dataset, Year >= 2004)
dataset_nof_04_09 <-subset(dataset_nof, Year <= 2009)





occur_04_09 = table(dataset_04_09$Year)
occur_04_09_df = as.data.frame(occur_04_09) 

colnames(occur_04_09_df) <- c('Year','Crash_cnt')
plot(occur_04_09_df)
occur_04_09_df
dataset_nof_04_09

safty = merge(occur_04_09_df, dataset_nof_04_09, by = "Year", all.x = TRUE) 
safty$Crash_prob = safty$Crash_cnt / (safty$NumOfFl)
safty
safty$Year <- as.numeric(as.character(safty$Year))
plot(  safty$Year,safty$Crash_prob
     )

plot(  safty$Year,safty$Crash_prob
)


plot(
  x=safty$Year,
  y=safty$Crash_prob,
  #xlim = c(2004, 2009)
  main = "Prawdopodobieństwo wypadku w latach 2004-2009",
  xlab = "Rok",
  ylab = "Prawdopodobieństwo wypadku [1E-6]",
  pch = 19
)






