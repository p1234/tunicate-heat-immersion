
test <- read_delim("tunicatemain.csv", delim = ',')
library(tidyverse)
test1 <- test %>% mutate(percentchangergb = `48hr_rgb`/initial_rgb)
test1 <- test1 %>% mutate(percentchangered = `48hr_red`/initial_red)

test1$temperature_c <- as.factor(test1$temperature_c)
testboxplot <- ggplot(test1, aes(x=temperature_c, y=percentchangered)) + 
  geom_boxplot() + ylim(0.5,1.5) +
  xlab("temperature") + ylab("percentchangered") +
  theme_classic() + geom_jitter()
testboxplot

test1$temperature_c <- as.factor(test1$temperature_c)
testboxplot <- ggplot(test1, aes(x=water_type, y=percentchangergb)) + 
  geom_boxplot() + ylim(0.5,1.5) +
  xlab("water type") + ylab("percentchangergb") +
  theme_classic() + geom_jitter()
testboxplot

test1$exposure_time_s <- as.factor(test1$exposure_time_s)
testboxplot <- ggplot(test1, aes(x=exposure_time_s, y=percentchangered)) + 
  geom_boxplot() + ylim(0.5,1.5) +
  xlab("exposuretime") + ylab("percentchangergb") +
  theme_classic() + geom_jitter()
testboxplot

tuniaov <- aov(percentchangered ~ temperature_c*exposure_time_s*water_type, data = test1)
summary(tuniaov)

weight <- test %>% mutate(percentchangeweight = `48hr_weight`/post_acclimation_weight_g)
tuniaov <- aov(percentchangeweight ~ temperature_c*exposure_time_s*water_type, data = weight)
summary(tuniaov)
TukeyHSD(tuniaov)

agg <- aggregate(test1$percentchange, by= list(test1$water_type, test1$temperature_c, test1$exposure_time_s), FUN = mean)
colnames(agg) <- c("water", "temp", "exposure", "percentchange")

agg$temp <- as.factor(agg$temp)
testboxplot <- ggplot(test1, aes(x=temperature_c, y=percentchange)) + 
  geom_boxplot() + ylim(0.5,1.5) +
  xlab("temperature") + ylab("percentchangergb") +
  theme_classic() + geom_jitter()
testboxplot