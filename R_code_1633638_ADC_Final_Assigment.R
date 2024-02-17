#FINAL ASSIGMENT - HAPPINESS WORLD REPORT 2019 - ANÀLISI DE DADES COMPLEXES
#CARLA ARDIACA ROMERO: 163368
library(reshape2)
library(ggplot2)
library(dplyr)
library(purrr)
library(ggcorrplot) # for correlation plots
world <- map_data('world')

#Read Datset
data_hap <-read.csv ("C:/Users/Carla/OneDrive/Escritorio/MatCAD/2n/2n semestre/Anàlisi de Dades Complexa/treball final/archive/2019.csv")
summary(data_hap)

shapiro.test(data_hap$Score)
#World happines map
hap<- select(data_hap,Country_or_region, Score)
world <- world %>% filter(region != "Antarctica")
world <- fortify(world)

ggplot() +  geom_map(data=world, map=world, aes(x=long, y=lat, group=group, map_id=region),
                     fill="white", colour="black") +  geom_map(data=hap, map=world,
           aes(fill=Score, map_id=Country_or_region),colour="black") + 
  scale_fill_continuous(low="blue", high="yellow", guide="colorbar") + 
  labs(title = "World Happiness Score")


#Correlation Matrix
numeric_data=select(data_hap,Score,GDP_per_capita,Social_support,Healthy_life_expectancy,Freedom_to_make_life_choices,Generosity,Perceptions_of_corruption)
cormat <- round(x = cor(numeric_data), digits = 2)

melted_corr_mat <- melt(cormat)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,fill=value)) +
geom_tile() +
scale_fill_gradient(low = "blue",
                    high = "yellow",
                    guide = "colorbar")

#Lineal Model lm function
fit <- lm(Score~1+GDP_per_capita+Social_support+Healthy_life_expectancy+Freedom_to_make_life_choices+Generosity+Perceptions_of_corruption, data = data_hap)
summary(fit)
confint(fit) #confidence intervals for the lineal model

#we apply backward selection and delete "generosity" of our model
fit1 <- lm(Score~1+GDP_per_capita+Social_support+Healthy_life_expectancy+Freedom_to_make_life_choices+Perceptions_of_corruption, data = data_hap)
summary(fit1) #we'll work with this model
confint(fit1) #confidence intervals for the lineal model

#gdp
model <- lm(data_hap$Score~ data_hap$GDP_per_capita, data = data_hap)  # Fit linear regression model
cor_coef<-cor(data_hap$Score,data_hap$GDP_per_capita)
p<- ggplot(data_hap, aes(x=GDP_per_capita, y=Score)) + geom_point()+ theme_bw() + xlab("GDP per capita") + ggtitle("GDP vs Score - Correlation: ", round(cor_coef,2))
p=p + geom_abline(intercept = coef(model)[1], slope = coef(model)[2], color="blue")
p

#social support
model <- lm(data_hap$Score~ data_hap$Social_support, data = data_hap)  # Fit linear regression model
cor_coef<-cor(data_hap$Score,data_hap$Social_support)
p<- ggplot(data_hap, aes(x=Social_support, y=Score)) + geom_point()+ theme_bw() + xlab("Social support") + ggtitle("Social Support vs Score - Correlation: ", round(cor_coef,2))
p=p + geom_abline(intercept = coef(model)[1], slope = coef(model)[2], color="blue")
p

#Healthy_life_expectancy
model <- lm(data_hap$Score~ data_hap$Healthy_life_expectancy, data = data_hap)  # Fit linear regression model
cor_coef<-cor(data_hap$Score,data_hap$Healthy_life_expectancy)
p<- ggplot(data_hap, aes(x=Healthy_life_expectancy, y=Score)) + geom_point()+ theme_bw() + xlab("Healthy life expectancy") + ggtitle("Healthy life vs Score - Correlation: ", round(cor_coef,2))
p=p + geom_abline(intercept = coef(model)[1], slope = coef(model)[2], color="blue")
p

#Freedom_to_make_life_choices
model <- lm(data_hap$Score~ data_hap$Freedom_to_make_life_choices, data = data_hap)  # Fit linear regression model
cor_coef<-cor(data_hap$Score,data_hap$Freedom_to_make_life_choices)
p<- ggplot(data_hap, aes(x=Freedom_to_make_life_choices, y=Score)) + geom_point()+ theme_bw() + xlab("Freedom to make life choices") + ggtitle("Freddom vs Score - Correlation: ", round(cor_coef,2))
p=p + geom_abline(intercept = coef(model)[1], slope = coef(model)[2], color="blue")
p

#Generosity
model <- lm(data_hap$Score~ data_hap$Generosity, data = data_hap)  # Fit linear regression model
cor_coef<-cor(data_hap$Score,data_hap$Generosity)
p<- ggplot(data_hap, aes(x=Generosity, y=Score)) + geom_point()+ theme_bw() + xlab("Generosity") + ggtitle("Generosity vs Score - Correlation: ", round(cor_coef,2))
p=p + geom_abline(intercept = coef(model)[1], slope = coef(model)[2], color="blue")
p

#Perceptions_of_corruption
model <- lm(data_hap$Score~ data_hap$Perceptions_of_corruption, data = data_hap)  # Fit linear regression model
cor_coef<-cor(data_hap$Score,data_hap$Perceptions_of_corruption)
p<- ggplot(data_hap, aes(x=Perceptions_of_corruption, y=Score)) + geom_point()+ theme_bw() + xlab("Perceptions of corruption") + ggtitle("Corruption vs Score - Correlation: ", round(cor_coef,2))
p=p + geom_abline(intercept = coef(model)[1], slope = coef(model)[2], color="blue")
p

#GDP vs Health
model <- lm(data_hap$GDP_per_capita ~ data_hap$Healthy_life_expectancy, data = data_hap)  # Fit linear regression model
cor_coef<-cor(data_hap$GDP_per_capita,data_hap$Healthy_life_expectancy)
p<- ggplot(data_hap, aes(x=Healthy_life_expectancy, y=GDP_per_capita)) + geom_point()+ theme_bw() + xlab("Healthy life expectancy") + ggtitle("GDP vs Healthy life - Correlation: ", round(cor_coef,2))
p=p + geom_abline(intercept = coef(model)[1], slope = coef(model)[2], color="blue")
p

#GDP vs Social Support
model <- lm(data_hap$GDP_per_capita ~ data_hap$Social_support, data = data_hap)  # Fit linear regression model
cor_coef<-cor(data_hap$GDP_per_capita,data_hap$Social_support)
p<- ggplot(data_hap, aes(x=Social_support, y=GDP_per_capita)) + geom_point()+ theme_bw() + xlab("Social Support") + ggtitle("GDP vs Social Support - Correlation: ", round(cor_coef,2))
p=p + geom_abline(intercept = coef(model)[1], slope = coef(model)[2], color="blue")
p

#Health vs Social
model <- lm(data_hap$Social_support ~ data_hap$Healthy_life_expectancy, data = data_hap)  # Fit linear regression model
cor_coef<-cor(data_hap$Social_support,data_hap$Healthy_life_expectancy)
p<- ggplot(data_hap, aes(x=Healthy_life_expectancy, y=Social_support)) + geom_point()+ theme_bw() + xlab("Healthy life expectancy") + ggtitle("Social Support vs Healthy life - Correlation: ", round(cor_coef,2))
p=p + geom_abline(intercept = coef(model)[1], slope = coef(model)[2], color="blue")
p

mean_score<-mean(data_hap$Score);
meand_GDP<-mean(data_hap$GDP_per_capita)
mean_gene = mean(data_hap$Generosity)
sd_score<-sd(data_hap$Score);
sd_score_GDP<-sd(data_hap$GDP_per_capita); 
sd_generosity = sd(data_hap$Generosity)

getmode <- function(v) {uniqv <- unique(v); uniqv[which.max(tabulate(match(v, uniqv)))]}

moda <- getmode(data_hap$Score)
hist(data_hap$Score, xlab="Happiness Score", ylab="Frequency", main="Distribution of Happiness score")
abline(v = mean_score, col = c("blue"), lwd = 2, lty = 2:3)
abline(v = moda, col = c("purple"), lwd = 2, lty = 2:3)
legend(x = "topright", legend = c("Mean", "Mode"), fill = c("blue", "purple"))

#correlation coefficient between the variable scorre and gdp per capita
cor_coef<-cor(data_hap$Score,data_hap$GDP_per_capita)

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

