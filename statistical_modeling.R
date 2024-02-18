# Linear model with all predictors
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