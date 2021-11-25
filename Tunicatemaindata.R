
###making some new columns
test <- read_delim("tunicatemain.csv", delim = ',')
library(tidyverse)
test1 <- test %>% mutate(percentchangergb = `48hr_rgb`/initial_rgb)
test1 <- test1 %>% mutate(percentchangered = `48hr_red`/initial_red)
test1 <- test1 %>% mutate(deltachangergb = `48hr_rgb`-initial_rgb)
test1 <- test1 %>% mutate(deltaweight = `48hr_weight`-post_acclimation_weight_g)

test1$temperature_c <- as.factor(test1$temperature_c)
test1$exposure_time_s <- replace(test1$exposure_time_s, test1$exposure_time_s== "30", "2")
test1$exposure_time_s <- replace(test1$exposure_time_s, test1$exposure_time_s== "60", "1")

test1$exposure_time_s <- as.factor(test1$exposure_time_s)
##delta RGB
position_dodge(width = 0.2)
testboxplot <- ggplot(test1, aes(x=water_type, y=deltachangergb)) + 
  geom_boxplot(outlier.shape = NA, alpha = 0.8) + #ylim(-1,1.5) +
  geom_jitter(position = position_dodge(width=0.75))+
  xlab("Water Type") + ylab("Change RGB Value") +
  theme_bw() + facet_grid(. ~ temperature_c)
testboxplot


test1boxplot <- ggplot(test1, aes(x=temperature_c, y=deltachangergb,
                                 fill = water_type, color = water_type, group = interaction(temperature_c, water_type))) + 
  geom_boxplot(outlier.shape = NA, alpha = 0.1, width = 0.75) + #ylim(-1,1.5) +
  geom_jitter(position = position_dodge(width=0.75))+
  xlab("Temperature (°C)") + ylab("Change in Mean RGB") +
  theme_classic() + labs(colour = "Water Type")
test1boxplot

####doing one for delta weight
position_dodge(width = 0.2)
testboxplot <- ggplot(test1, aes(x=temperature_c, y=deltaweight,
                                 fill = water_type, color = water_type, group = interaction(temperature_c, water_type))) + 
  geom_boxplot(outlier.shape = NA, alpha = 0.1, width = 0.75) + #ylim(-1,1.5) +
  geom_jitter(position = position_dodge(width=0.75))+
  xlab("Temperature (*C)") + ylab("Change Weight") +
  theme_classic() 
testboxplot

ggplot(df, aes(y = y, x = x, fill = g, color = g, group = interaction(x, g))) +
  geom_boxplot(alpha = 0.1, width=0.75) +
  geom_point(position = position_dodge(width=0.75))




test1$temperature_c <- as.factor(test1$temperature_c)
testboxplot <- ggplot(test1, aes(x=temperature_c, y=percentchangergb)) + 
  geom_boxplot() + ylim(0.5,1.5) +
  xlab("temp") + ylab("percentchangergb") +
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

###is it normal?
shapiro.test(test1$`48hr_rgb`)

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

##bar chart
bar <- ggplot(test1, aes(x=temperature_c, y=`48hr_rgb`, fill = water_type)) + 
  geom_bar(position="dodge", stat='identity') + theme_classic()
bar  

testboxplot <- ggplot(test1, aes(x=water_type, y=deltachangergb)) + 
  geom_boxplot() + #ylim(-1,1.5) +
  xlab("temperature") + ylab("deltachangered") +
  theme_classic() + geom_jitter()
testboxplot

###Doing a little correlation moment
tunibby <- read_delim("tunibby.csv", delim = ',')
res <- cor.test(tunibby$`48hr_rgb`, tunibby$`survival`, 
                method = "pearson")
res

#Now doing it on delta change
tunibby <- tunibby %>% mutate(deltachangergb = `48hr_rgb`-initial_rgb)
yuh <- cor.test(tunibby$deltachangergb, tunibby$`survival`, 
                method = "pearson")
yuh

library("ggpubr")
ggscatter(tunibby, x = "survival", y = "48hr_rgb", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "survival", ylab = "rgb")

####THESE ARE THE PROPORTION GRAPHS
###survival graph by temperature

ew <- group_by(tunibby,temperature_c)
View(ew)
ew <- ew[-c(81, 82, 83, 84), ]
blu<- summarise(ew, prop_surv=mean(survival, na.rm=TRUE), sd=sd(survival, na.rm=TRUE), total=n(), SE=sd(survival, na.rm = TRUE)/sqrt(20))

##making a survival proportion graph
library(ggplot2)
blu$temperature_c <- as.factor(blu$temperature_c)
survivalgraph <- ggplot(blu, aes(x=temperature_c, y=prop_surv)) + geom_point() +ylim(0,1) +
  xlab("Temperature (°C)") + ylab("Survival Proportion")+
  geom_errorbar(aes(ymin=prop_surv-SE, ymax=prop_surv+SE, width=.2))+
  theme_classic()+
  geom_point(size=2)
survivalgraph


###survival graph by water type
ew1 <- group_by(tunibby,water_type)
ew1 <- ew1[-c(81, 82, 83, 84), ]
blu1<- summarise(ew1, prop_surv=mean(survival, na.rm=TRUE), sd=sd(survival, na.rm=TRUE), total=n(), SE=sd(survival, na.rm = TRUE)/sqrt(40))

##making the graph
library(ggplot2)
watergraph <- ggplot(blu1, aes(x=water_type, y=prop_surv)) + geom_point() +ylim(0,0.54) +
  xlab("Temperature") + ylab("Survival Proportion")+
  geom_errorbar(aes(ymin=prop_surv-SE, ymax=prop_surv+SE, width=.2))+
  theme_classic()+
  geom_point(size=2)
watergraph


##survival graph by exposure time
ew2 <- group_by(tunibby,exposure_time_s)
ew2 <- ew2[-c(81, 82, 83, 84), ]
blu2<- summarise(ew2, prop_surv=mean(survival, na.rm=TRUE), sd=sd(survival, na.rm=TRUE), total=n(), SE=sd(survival, na.rm = TRUE)/sqrt(40))

##making the graph
watergraph <- ggplot(blu2, aes(x=exposure_time_s, y=prop_surv)) + geom_point() +ylim(0,1) +
  xlab("Temperature") + ylab("Survival Proportion")+
  geom_errorbar(aes(ymin=prop_surv-SE, ymax=prop_surv+SE, width=.2))+
  theme_classic()+
  geom_point(size=2)
watergraph



##Playing with weight graph
ggplot(data=tunibby, 
       aes(x=factor(), y=proportion.tasty, 
           group=group,
           shape=group,
           color=group)) + 
  geom_line() + 
  geom_point() +
  opts(title = 
         "Proportion Tasty by Year, Quality, and Group") +
  scale_x_discrete("Year") +
  scale_y_continuous("Proportion Tasty") + 
  facet_grid(.~quality )



###attachment graph
at$temperature_c <- as.factor(at$temperature_c)
at <- group_by(tunibby,temperature_c)
at <- at[-c(81, 82, 83, 84), ]
attach_t<- summarise(at, prop_attach=mean(attachment, na.rm=TRUE), sd=sd(attachment, na.rm=TRUE), total=n(), SE=sd(attachment, na.rm = TRUE)/sqrt(40))

##making the graph
attach_t$temperature_c <- as.factor(attach_t$temperature_c)
attachgraph <- ggplot(attach_t, aes(x=temperature_c, y=prop_attach)) + geom_point() +ylim(0,1) +
  xlab("Temperature (°C)") + ylab("Attachment Proportion")+
  geom_errorbar(aes(ymin=prop_attach-SE, ymax=prop_attach+SE, width=.2))+
  theme_classic()+
  geom_point(size=2)
attachgraph
