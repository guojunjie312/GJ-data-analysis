##Bootstrapping notes

library("boot")
library("ggplot2")
library("tidyverse")

# x1<-c(35,37,43,41,37,32,45,36,44,31,38,31)
# x2<-c(32,33,36,35,36,31,34,32,39,38,39,34,33)

df1 <- rnorm(1000, 100, 10)

#Use sample() and for-loop to do bootstrapping
x1_s <- sample(df1,100,replace = TRUE)

mean1 <- mean(x1_s)

for (i in 2:999){
  a = mean(sample(df1,10,replace = TRUE))
  mean1 = c(mean1, a)
}

sort(mean1)
quantile(mean1,0.975)
quantile(mean1,0.025)

figure_s1 <- hist(mean1, breaks = 20)
figure_df <- hist(df1, breaks = 20)


##Bootstrap the mean of a vector
s_mean <- function(data, i){
  data <- data %>% as.data.frame
  d <- data[i, ]   #select the sampled row
  mean <- mean(d)
  return(mean)
}

x3_s <- boot(df1, s_mean, R = 999)
#An alternative
x3_s <- boot(df1, function(i, d) mean(i[d]), R = 999)

boot.ci(boot.out = x3_s, type = c("norm", "basic", "perc"))

##Bootstrap the correlation coefficient 
hsb2 <- read.table("https://stats.idre.ucla.edu/stat/data/hsb2.csv", sep=",", header=T)

fc <- function(d, i){
  d2 <- d[i,]
  return(cor(d2$write, d2$math))
}

set.seed(626)
bootcorr <- boot(hsb2, fc, R=500)

boot.ci(boot.out = bootcorr, type = c("norm", "basic", "perc", "bca"))

