# Load necessary library for data visualization
library(ggplot2)
library(ggcorrplot)

# Basic exploratory data analysis
# Dataset structure and summary
print(str(data_hap))
print(summary(data_hap))

# Shapiro test for normality
shapiro.test(data_hap$Score)

# World happiness map visualization
world <- map_data('world')
hap<- select(data_hap,Country_or_region, Score)
world <- world %>% filter(region != "Antarctica")
world <- fortify(world)

ggplot() + 
  geom_map(data = world, map = world, aes(group = group, map_id = region), fill = "white", colour = "black") + 
  geom_map(data = hap, map = world, aes(fill = Score, map_id = Country_or_region), colour = "black") + 
  scale_fill_continuous(low = "blue", high = "yellow", guide = "colorbar") + 
  labs(title = "World Happiness Score")

# Correlation matrix visualization
numeric_data=select(data_hap,Score,GDP_per_capita,Social_support,Healthy_life_expectancy,Freedom_to_make_life_choices,Generosity,Perceptions_of_corruption)
cormat <- round(x = cor(numeric_data), digits = 2)

melted_corr_mat <- melt(cormat)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,fill=value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue",
                      high = "yellow",
                      guide = "colorbar")

