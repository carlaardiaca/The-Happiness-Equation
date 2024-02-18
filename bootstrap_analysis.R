# Bootstrap analysis for linear models and correlations
#Generate the parametric bootstrap replicates and calculate the statistics of interest.
iter<-10000 ; #number of simulations 
n<-length(data_hap$Score) #number of countries

slope_gdp<-numeric(iter); 
slope_social<-numeric(iter); 
slope_healthy<-numeric(iter); 
slope_freedom<-numeric(iter); 
slope_corruption<-numeric(iter); 
slope_generosity<-numeric(iter); 
interc<-numeric(iter)

corr_gdp<-numeric(iter); 
corr_social<-numeric(iter); 
corr_healthy<-numeric(iter); 
corr_freedom<-numeric(iter); 
corr_corruption<-numeric(iter); 
#corr_generosity<-numeric(iter); 

rsales<-numeric(n)
a <- lm(Score~1+GDP_per_capita+Social_support+Healthy_life_expectancy+Freedom_to_make_life_choices+Perceptions_of_corruption, data = data_hap)
b=summary(a)

for(i in 1:10000){
  error<-rnorm(n,0,b$sigma)
  rscore <-a$coefficient[1] + a$coefficient[2]*data_hap$GDP_per_capita + 
    a$coefficient[3]*data_hap$Social_support + a$coefficient[4]*data_hap$Healthy_life_expectancy +
    a$coefficient[5]*data_hap$Freedom_to_make_life_choices + a$coefficient[6]*data_hap$Perceptions_of_corruption + 
    error
  
  nl_nova<-lm(rscore~GDP_per_capita+Social_support+Healthy_life_expectancy+Freedom_to_make_life_choices+Perceptions_of_corruption, data = data_hap)
  
  interc[i]<-nl_nova$coefficient[1]
  slope_gdp[i]<-nl_nova$coefficient[2] ;
  slope_social[i]<-nl_nova$coefficient[3] ;
  slope_healthy[i]<-nl_nova$coefficient[4] ;
  slope_freedom[i]<-nl_nova$coefficient[5] ;
  slope_corruption[i]<-nl_nova$coefficient[6] ;
  #slope_generosity[i]<-nl_nova$coefficient[6] ;
  
  
  corr_gdp[i]<-cor(rscore,data_hap$GDP_per_capita)
  corr_social[i]<-cor(rscore,data_hap$Social_support)
  corr_healthy[i]<-cor(rscore,data_hap$Healthy_life_expectancy)
  corr_freedom[i]<-cor(rscore,data_hap$Freedom_to_make_life_choices)
  corr_corruption[i]<-cor(rscore,data_hap$Perceptions_of_corruption)
  #corr_generosity[i]<-cor(rscore,data_hap$Generosity)
}

#corruption#Calculate confidence intervals
#gdp: variable and correlation
IC=quantile(slope_gdp, probs = c(0.025, 0.975))
IC
hist(slope_gdp, xlab="GDP per capita", ylab="Frequency", main="GDP per capita Histogram")
abline(v = IC[1], col = c("red"), lwd = 2, lty = 2:3)
abline(v = IC[2], col = c("red"), lwd = 2, lty = 2:3)

moda <- getmode(slope_gdp)
meand_GDP<-mean(slope_gdp)
abline(v = meand_GDP, col = c("blue"), lwd = 2, lty = 2:3)
abline(v = moda, col = c("purple"), lwd = 2, lty = 2:3)
legend(x = "topright", legend = c("Mean", "Mode", "95% IC"), fill = c("blue", "purple", "red"))

IC_c=quantile(corr_gdp, probs = c(0.025, 0.975)) 
IC_c
hist(corr_gdp, xlab="Correlation GDP & Score", ylab="Frequency", main="Correlation GDP & Score Histogram")
abline(v = IC_c[1], col = c("red"), lwd = 2, lty = 2:3)
abline(v = IC_c[2], col = c("red"), lwd = 2, lty = 2:3)

moda <- getmode(corr_gdp)
meand_GDP<-mean(corr_gdp)
abline(v = meand_GDP, col = c("blue"), lwd = 2, lty = 2:3)
abline(v = moda, col = c("purple"), lwd = 2, lty = 2:3)
legend(x = "topright", legend = c("Mean", "Mode", "95% IC"), fill = c("blue", "purple", "red"))

#social support: variable and correlation
IC=quantile(slope_social, probs = c(0.025, 0.975))
IC
hist(slope_social, xlab="Social Support", ylab="Frequency", main="Social Support per capita Histogram")
abline(v = IC[1], col = c("red"), lwd = 2, lty = 2:3)
abline(v = IC[2], col = c("red"), lwd = 2, lty = 2:3)

moda <- getmode(slope_social)
meand_social<-mean(slope_social)
abline(v = meand_social, col = c("blue"), lwd = 2, lty = 2:3)
abline(v = moda, col = c("purple"), lwd = 2, lty = 2:3)
legend(x = "topright", legend = c("Mean", "Mode", "95% IC"), fill = c("blue", "purple", "red"))

IC_c=quantile(corr_social, probs = c(0.025, 0.975)) 
IC_c
hist(corr_social, xlab="Correlation Social Support & Score", ylab="Frequency", main="Correlation Social Support & Score Histogram")
abline(v = IC_c[1], col = c("red"), lwd = 2, lty = 2:3)
abline(v = IC_c[2], col = c("red"), lwd = 2, lty = 2:3)

moda <- getmode(corr_social)
meand_GDP<-mean(corr_social)
abline(v = meand_GDP, col = c("blue"), lwd = 2, lty = 2:3)
abline(v = moda, col = c("purple"), lwd = 2, lty = 2:3)
legend(x = "topright", legend = c("Mean", "Mode", "95% IC"), fill = c("blue", "purple", "red"))

#healthy support: variable and correlation
IC=quantile(slope_healthy, probs = c(0.025, 0.975))
IC
hist(slope_healthy, xlab="Healthy life expectancy", ylab="Frequency", main="Healthy life expectancy Histogram")
abline(v = IC[1], col = c("red"), lwd = 2, lty = 2:3)
abline(v = IC[2], col = c("red"), lwd = 2, lty = 2:3)

moda <- getmode(slope_healthy)
mean<-mean(slope_healthy)
abline(v = mean, col = c("blue"), lwd = 2, lty = 2:3)
abline(v = moda, col = c("purple"), lwd = 2, lty = 2:3)
legend(x = "topright", legend = c("Mean", "Mode", "95% IC"), fill = c("blue", "purple", "red"))

IC_c=quantile(corr_healthy, probs = c(0.025, 0.975)) 
IC_c
hist(corr_healthy, xlab="Correlation Healthy life expectancy & Score", ylab="Frequency", main="Correlation Healthy life & Score Histogram")
abline(v = IC_c[1], col = c("red"), lwd = 2, lty = 2:3)
abline(v = IC_c[2], col = c("red"), lwd = 2, lty = 2:3)

moda <- getmode(corr_healthy)
mean<-mean(corr_healthy)
abline(v = mean, col = c("blue"), lwd = 2, lty = 2:3)
abline(v = moda, col = c("purple"), lwd = 2, lty = 2:3)
legend(x = "topright", legend = c("Mean", "Mode", "95% IC"), fill = c("blue", "purple", "red"))

#freedom: variable and correlation
IC=quantile(slope_freedom, probs = c(0.025, 0.975))
IC
hist(slope_freedom, xlab="Freedom", ylab="Frequency", main="Freedom Histogram")
abline(v = IC[1], col = c("red"), lwd = 2, lty = 2:3)
abline(v = IC[2], col = c("red"), lwd = 2, lty = 2:3)

moda <- getmode(slope_freedom)
mean<-mean(slope_freedom)
abline(v = mean, col = c("blue"), lwd = 2, lty = 2:3)
abline(v = moda, col = c("purple"), lwd = 2, lty = 2:3)
legend(x = "topright", legend = c("Mean", "Mode", "95% IC"), fill = c("blue", "purple", "red"))

IC_c=quantile(corr_freedom, probs = c(0.025, 0.975)) 
IC_c
hist(corr_freedom, xlab="Correlation Freedom & Score", ylab="Frequency", main="Correlation Freedom & Score Histogram")
abline(v = IC_c[1], col = c("red"), lwd = 2, lty = 2:3)
abline(v = IC_c[2], col = c("red"), lwd = 2, lty = 2:3)

moda <- getmode(corr_freedom)
meand_GDP<-mean(corr_freedom)
abline(v = meand_GDP, col = c("blue"), lwd = 2, lty = 2:3)
abline(v = moda, col = c("purple"), lwd = 2, lty = 2:3)
legend(x = "topright", legend = c("Mean", "Mode", "95% IC"), fill = c("blue", "purple", "red"))

IC=quantile(slope_corruption, probs = c(0.025, 0.975))
IC
hist(slope_corruption, xlab="Perceptions of corruption", ylab="Frequency", main="Perceptions of corruption Histogram")
abline(v = IC[1], col = c("red"), lwd = 2, lty = 2:3)
abline(v = IC[2], col = c("red"), lwd = 2, lty = 2:3)

moda <- getmode(slope_corruption)
meand_GDP<-mean(slope_corruption)
abline(v = meand_GDP, col = c("blue"), lwd = 2, lty = 2:3)
abline(v = moda, col = c("purple"), lwd = 2, lty = 2:3)
legend(x = "topright", legend = c("Mean", "Mode", "95% IC"), fill = c("blue", "purple", "red"))

IC_c=quantile(corr_corruption, probs = c(0.025, 0.975)) 
IC_c
hist(corr_corruption, xlab="Correlation Corruption & Score", ylab="Frequency", main="Correlation Corruption & Score Histogram")
abline(v = IC_c[1], col = c("red"), lwd = 2, lty = 2:3)
abline(v = IC_c[2], col = c("red"), lwd = 2, lty = 2:3)

moda <- getmode(corr_corruption)
meand_GDP<-mean(corr_corruption)
abline(v = meand_GDP, col = c("blue"), lwd = 2, lty = 2:3)
abline(v = moda, col = c("purple"), lwd = 2, lty = 2:3)
legend(x = "topright", legend = c("Mean", "Mode", "95% IC"), fill = c("blue", "purple", "red"))

#non-parametric boostrap
n<-length(data_hap$Score) #number of countries
iter=10000

slope_gdp<-numeric(iter); 
slope_social<-numeric(iter); 
slope_healthy<-numeric(iter); 
slope_freedom<-numeric(iter); 
slope_corruption<-numeric(iter); 
slope_generosity<-numeric(iter);
interc<-numeric(iter)

corr_gdp<-numeric(iter); 
corr_social<-numeric(iter); 
corr_healthy<-numeric(iter); 
corr_freedom<-numeric(iter); 
corr_corruption<-numeric(iter); 

hap<- select(data_hap,Score, GDP_per_capita, Social_support,Healthy_life_expectancy, Freedom_to_make_life_choices,Perceptions_of_corruption  )

for(i in 1:10000){
  yb <- hap[sample(n, n, replace = TRUE ), ]
  fitb<-lm(Score~GDP_per_capita+Social_support+Healthy_life_expectancy+Freedom_to_make_life_choices+Perceptions_of_corruption, data = yb)
  interc[i] <- summary(fitb)$coefficients[1]
  slope_gdp[i]<-summary(fitb)$coefficients[2]
  slope_social[i]<-summary(fitb)$coefficients[3]
  slope_healthy[i]<-summary(fitb)$coefficients[4]
  slope_freedom[i]<-summary(fitb)$coefficients[5]
  slope_corruption[i]<-summary(fitb)$coefficients[6]
  #slope_generosity[i]<-summary(fitb)$coefficients[6]
  
  corr_gdp[i]<-cor(yb$Score , yb$GDP_per_capita)
  corr_social[i]<-cor(yb$Score , yb$Social_support)
  corr_healthy[i]<-cor(yb$Score , yb$Healthy_life_expectancy)
  corr_freedom[i]<-cor(yb$Score , yb$Freedom_to_make_life_choices)
  corr_corruption[i]<-cor(yb$Score , yb$Perceptions_of_corruption)
  #corr_generosity[i]<-cor(rscore,data_hap$Generosity)
}
#Calculate confidence intervals
#gdp: variable and correlation
IC=quantile(slope_gdp, probs = c(0.025, 0.975))
IC
hist(slope_gdp, xlab="GDP per capita", ylab="Frequency", main="GDP per capita Histogram")
abline(v = IC[1], col = c("red"), lwd = 2, lty = 2:3)
abline(v = IC[2], col = c("red"), lwd = 2, lty = 2:3)
moda <- getmode(slope_gdp)
meand_GDP<-mean(slope_gdp)
abline(v = meand_GDP, col = c("blue"), lwd = 2, lty = 2:3)
abline(v = moda, col = c("purple"), lwd = 2, lty = 2:3)
legend(x = "topright", legend = c("Mean", "Mode", "95% IC"), fill = c("blue", "purple", "red"))

IC_c=quantile(corr_gdp, probs = c(0.025, 0.975)) 
IC_c
hist(corr_gdp, xlab="Correlation GDP & Score", ylab="Frequency", main="Correlation GDP & Score Histogram")
abline(v = IC_c[1], col = c("red"), lwd = 2, lty = 2:3)
abline(v = IC_c[2], col = c("red"), lwd = 2, lty = 2:3)

moda <- getmode(corr_gdp)
meand_GDP<-mean(corr_gdp)
abline(v = meand_GDP, col = c("blue"), lwd = 2, lty = 2:3)
abline(v = moda, col = c("purple"), lwd = 2, lty = 2:3)
legend(x = "topright", legend = c("Mean", "Mode", "95% IC"), fill = c("blue", "purple", "red"))

#social support: variable and correlation
IC=quantile(slope_social, probs = c(0.025, 0.975))
IC
hist(slope_social, xlab="Social Support", ylab="Frequency", main="Social Support per capita Histogram")
abline(v = IC[1], col = c("red"), lwd = 2, lty = 2:3)
abline(v = IC[2], col = c("red"), lwd = 2, lty = 2:3)

moda <- getmode(slope_social)
meand_social<-mean(slope_social)
abline(v = meand_social, col = c("blue"), lwd = 2, lty = 2:3)
abline(v = moda, col = c("purple"), lwd = 2, lty = 2:3)
legend(x = "topright", legend = c("Mean", "Mode", "95% IC"), fill = c("blue", "purple", "red"))

IC_c=quantile(corr_social, probs = c(0.025, 0.975)) 
IC_c
hist(corr_social, xlab="Correlation Social Support & Score", ylab="Frequency", main="Correlation Social Support & Score Histogram")
abline(v = IC_c[1], col = c("red"), lwd = 2, lty = 2:3)
abline(v = IC_c[2], col = c("red"), lwd = 2, lty = 2:3)

moda <- getmode(corr_social)
meand_GDP<-mean(corr_social)
abline(v = meand_GDP, col = c("blue"), lwd = 2, lty = 2:3)
abline(v = moda, col = c("purple"), lwd = 2, lty = 2:3)
legend(x = "topright", legend = c("Mean", "Mode", "95% IC"), fill = c("blue", "purple", "red"))

#healthy support: variable and correlation
IC=quantile(slope_healthy, probs = c(0.025, 0.975))
IC
hist(slope_healthy, xlab="Healthy life expectancy", ylab="Frequency", main="Healthy life expectancy Histogram")
abline(v = IC[1], col = c("red"), lwd = 2, lty = 2:3)
abline(v = IC[2], col = c("red"), lwd = 2, lty = 2:3)

moda <- getmode(slope_healthy)
mean<-mean(slope_healthy)
abline(v = mean, col = c("blue"), lwd = 2, lty = 2:3)
abline(v = moda, col = c("purple"), lwd = 2, lty = 2:3)
legend(x = "topright", legend = c("Mean", "Mode", "95% IC"), fill = c("blue", "purple", "red"))

IC_c=quantile(corr_healthy, probs = c(0.025, 0.975)) 
IC_c
hist(corr_healthy, xlab="Correlation Healthy life expectancy & Score", ylab="Frequency", main="Correlation Healthy life & Score Histogram")
abline(v = IC_c[1], col = c("red"), lwd = 2, lty = 2:3)
abline(v = IC_c[2], col = c("red"), lwd = 2, lty = 2:3)

moda <- getmode(corr_healthy)
mean<-mean(corr_healthy)
abline(v = mean, col = c("blue"), lwd = 2, lty = 2:3)
abline(v = moda, col = c("purple"), lwd = 2, lty = 2:3)
legend(x = "topright", legend = c("Mean", "Mode", "95% IC"), fill = c("blue", "purple", "red"))

#freedom: variable and correlation
IC=quantile(slope_freedom, probs = c(0.025, 0.975))
IC
hist(slope_freedom, xlab="Freedom", ylab="Frequency", main="Freedom Histogram")
abline(v = IC[1], col = c("red"), lwd = 2, lty = 2:3)
abline(v = IC[2], col = c("red"), lwd = 2, lty = 2:3)

moda <- getmode(slope_freedom)
mean<-mean(slope_freedom)
abline(v = mean, col = c("blue"), lwd = 2, lty = 2:3)
abline(v = moda, col = c("purple"), lwd = 2, lty = 2:3)
legend(x = "topright", legend = c("Mean", "Mode", "95% IC"), fill = c("blue", "purple", "red"))

IC_c=quantile(corr_freedom, probs = c(0.025, 0.975)) 
IC_c
hist(corr_freedom, xlab="Correlation Freedom & Score", ylab="Frequency", main="Correlation Freedom & Score Histogram")
abline(v = IC_c[1], col = c("red"), lwd = 2, lty = 2:3)
abline(v = IC_c[2], col = c("red"), lwd = 2, lty = 2:3)

moda <- getmode(corr_freedom)
meand_GDP<-mean(corr_freedom)
abline(v = meand_GDP, col = c("blue"), lwd = 2, lty = 2:3)
abline(v = moda, col = c("purple"), lwd = 2, lty = 2:3)
legend(x = "topright", legend = c("Mean", "Mode", "95% IC"), fill = c("blue", "purple", "red"))

IC=quantile(slope_corruption, probs = c(0.025, 0.975))
IC
hist(slope_corruption, xlab="Perceptions of corruption", ylab="Frequency", main="Perceptions of corruption Histogram")
abline(v = IC[1], col = c("red"), lwd = 2, lty = 2:3)
abline(v = IC[2], col = c("red"), lwd = 2, lty = 2:3)

moda <- getmode(slope_corruption)
meand_GDP<-mean(slope_corruption)
abline(v = meand_GDP, col = c("blue"), lwd = 2, lty = 2:3)
abline(v = moda, col = c("purple"), lwd = 2, lty = 2:3)
legend(x = "topright", legend = c("Mean", "Mode", "95% IC"), fill = c("blue", "purple", "red"))

IC_c=quantile(corr_corruption, probs = c(0.025, 0.975)) 
IC_c
hist(corr_corruption, xlab="Correlation Corruption & Score", ylab="Frequency", main="Correlation Corruption & Score Histogram")
abline(v = IC_c[1], col = c("red"), lwd = 2, lty = 2:3)
abline(v = IC_c[2], col = c("red"), lwd = 2, lty = 2:3)

moda <- getmode(corr_corruption)
meand_GDP<-mean(corr_corruption)
abline(v = meand_GDP, col = c("blue"), lwd = 2, lty = 2:3)
abline(v = moda, col = c("purple"), lwd = 2, lty = 2:3)
legend(x = "topright", legend = c("Mean", "Mode", "95% IC"), fill = c("blue", "purple", "red"))

