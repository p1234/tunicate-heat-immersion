##Tunicate analysis

setwd("~/OneDrive - UBC/Year 5/Directed Studies - Tunicate")
tunicate <- read.csv("tunicate_data_nov_1.csv")
tunicate_weight <- read.csv("tunicate_weight.csv")
head(tunicate_weight)

mean(tunicate$initial_weight_g) #7 g
mean(tunicate$post_acclimation_weight_g) ## 9.661 g
mean(tunicate$X48hours_weight_g) # 9.241625 g


library(ggplot2)
tunicate_weight$timestep_days <- as.factor(tunicate_weight$timestep_days)
tunicate_weight$temperature_c <- as.factor(tunicate_weight$temperature_c)
ggplot(data = tunicate_weight, aes(x = timestep_days , y = weight_g, group = temperature_c, colour = temperature_c))+
  geom_line()


library(tidyverse)
gd <- aggregate(tunicate_weight$weight_g, by = list(tunicate_weight$timestep_days, tunicate_weight$temperature_c), FUN = mean)
colnames(gd) <- c("timestep", "temp", "meanweight")

ggplot(data = gd, aes(x =timestep, y = meanweight, colour = temp))+
  geom_line() + theme_classic()

gd1 <- aggregate(tunicate_weight$weight_g, by = list(tunicate_weight$timestep_days, tunicate_weight$water_type), FUN = mean)
colnames(gd1) <- c("timestep","watertype", "meanweight")

ggplot(data = gd1, aes(x =timestep, y = meanweight, colour = watertype))+
  geom_line() + theme_classic()
