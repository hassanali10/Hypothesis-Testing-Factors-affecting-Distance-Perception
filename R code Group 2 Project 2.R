library(lubridate)
library(googlesheets4)
library(gargle)
library(dplyr)
library(reshape2)
library(condformat)
library(tidyr)
library(ggplot2)

# load data set

load("C:/Users/Hassan Ali/Desktop/ICS/Project 2/linelength.RData")

# make a boxplot of relative deviation by person
data3<- mutate(data_all, relative_dev = (Measured - Length)/Length)
boxplot(relative_dev ~ id, 
        data=data3,
        xlab = "Student ID",
        ylab = "Relative deviation")

# -----------------------------------------------------------------------------------
# TASK 1
# -----------------------------------------------------------------------------------


data<- mutate(data_all, relative_dev = (Measured - Length)/Length)
drop<-c("Length", "Eyes closed", "Measured", "rand")
data_point_1 = data[,!names(data)%in%drop]
data_point_1 = 
  data%>%group_by(id, Horizontal)%>%summarise(avg_rel_dev = mean(relative_dev))

Horizontal_lines<-data_point_1[data_point_1$Horizontal == "1",]
Vertical_lines<-data_point_1[data_point_1$Horizontal == "0",]

hist(Vertical_lines$avg_rel_dev)
barplot(Horizontal_lines$avg_rel_dev,  main = "Relative deviations (horizontal)", ylab = "Relative Deviation")  
barplot(Vertical_lines$avg_rel_dev, main = "Relative deviations (vertical)",  ylab = "Relative Deviation",
        xlab = "Index number")
boxplot(Horizontal_lines$avg_rel_dev, Vertical_lines$avg_rel_dev,
        ylab = "Relative deviation", 
        names = c("Horizontal lines", "Vertical lines"))

boxplot((Horizontal_lines$avg_rel_dev - Vertical_lines$avg_rel_dev),
        ylab = "Relative deviation", 
        xlab = ("Horizontal - Vertical"))

# for the t-test, the distributions must be approximately normally distributed
# check how normally distributed the deviations are using a q-q plot
par(mfrow = c(1,1))

qqnorm((Horizontal_lines$avg_rel_dev - Vertical_lines$avg_rel_dev), 
       xlab = "Normal theoretical quantiles",
       ylab = "Sample quantiles (horizontal - vertical)",
       main = "")
qqline(Horizontal_lines$avg_rel_dev - Vertical_lines$avg_rel_dev)

qqnorm(Horizontal_lines$avg_rel_dev, 
       xlab = "Normal theoretical quantiles",
       ylab = "Sample quantiles (horizontal lines)",
       main = "")
qqline(Horizontal_lines$avg_rel_dev)
qqnorm(Vertical_lines$avg_rel_dev,
       xlab = "Normal theoretical quantiles",
       ylab = "Sample quantiles (vertical Lines)",
       main = "")
qqline(Vertical_lines$avg_rel_dev)

# Print Summary

summary(Horizontal_lines$avg_rel_dev)
summary(Vertical_lines$avg_rel_dev)
var(Horizontal_lines$avg_rel_dev)
var(Vertical_lines$avg_rel_dev)


## assumptions for paired T test
## samples paired? yes, as vertical and horizontal line come from same individual

t.test(Horizontal_lines$avg_rel_dev,Vertical_lines$avg_rel_dev, paired = TRUE, conf.level = 0.95)


# -----------------------------------------------------------------------------------
# TASK 2
# -----------------------------------------------------------------------------------

drop<-c("Length", "Horizontal", "Measured", "rand")
data_point_1 = data[,!names(data)%in%drop]
data_point_1 = 
  data%>%group_by(id, `Eyes closed`)%>%summarise(avg_rel_dev = mean(relative_dev))

closed_lines<-data_point_1[data_point_1$`Eyes closed` == "1",]
open_lines<-data_point_1[data_point_1$`Eyes closed` == "0",]


boxplot((closed_lines$avg_rel_dev - open_lines$avg_rel_dev),
        ylab = "Relative deviation", 
        xlab = ("Closed eyes - open eyes"))

boxplot(closed_lines$avg_rel_dev, open_lines$avg_rel_dev,
        ylab = "Relative deviation", 
        names = c("Eyes closed", "Eyes open"))

# check whether the data points are normally distributed 

qqnorm((closed_lines$avg_rel_dev - open_lines$avg_rel_dev), 
       xlab = "Normal theoretical quantiles",
       ylab = "Sample quantiles (closed eyes - open eyes)",
       main = "")
qqline(closed_lines$avg_rel_dev - open_lines$avg_rel_dev)


qqnorm(closed_lines$avg_rel_dev,
       xlab = "Normal theoretical quantiles",
       ylab = "Sample quantiles (eyes closed)",
       main = "")
qqline(closed_lines$avg_rel_dev)
qqnorm(open_lines$avg_rel_dev,
       xlab = "Normal theoretical quantiles",
       ylab = "Sample quantiles (eyes open)",
       main = "")
qqline(open_lines$avg_rel_dev)

# Print Summary

summary(closed_lines$avg_rel_dev)
summary(open_lines$avg_rel_dev)
var(closed_lines$avg_rel_dev)
var(open_lines$avg_rel_dev)

## samples paired? yes, as both sets of lines come from the same individual

t.test(closed_lines$avg_rel_dev,open_lines$avg_rel_dev, paired = TRUE, conf.level = 0.95)


# -----------------------------------------------------------------------------------
# TASK 3
# -----------------------------------------------------------------------------------

data3<- mutate(data_all, relative_dev = (Measured - Length)/Length)

len4<-data3[data3$Length == '4',]
len8<-data3[data3$Length == '8',]
len12<-data3[data3$Length == '12',]
len16<-data3[data3$Length == '16',]


summary(len4$relative_dev)
summary(len8$relative_dev)
summary(len12$relative_dev)
summary(len16$relative_dev)

boxplot(len4$relative_dev, len8$relative_dev, 
        len12$relative_dev, len16$relative_dev,
        ylab = "Relative deviation", 
        names = c("4 cm", "8 cm",
                  "12 cm", "16 cm"),
        xlab = "Target length")

#par(mfrow = c(2,2))
qqnorm(len4$relative_dev,
       xlab = "Normal theoretical quantiles",
       ylab = "Sample quantiles (target length 4 cm)",
       main = "")
qqline(len4$relative_dev)
qqnorm(len8$relative_dev,
       xlab = "Normal theoretical quantiles",
       ylab = "Sample quantiles (target length 8 cm)",
       main = "")
qqline(len8$relative_dev)
qqnorm(len12$relative_dev, 
       xlab = "Normal theoretical quantiles",
       ylab = "Sample quantiles (target length 12 cm)",
       main = "")
qqline(len12$relative_dev)
qqnorm(len16$relative_dev,
       xlab = "Normal theoretical quantiles",
       ylab = "Sample quantiles (target length 16 cm)",
       main = "")
qqline(len16$relative_dev)

var(len4$relative_dev)
var(len8$relative_dev)
var(len12$relative_dev)
var(len16$relative_dev)

model = aov(relative_dev~as.factor(Length), data = data3)
summary(model)

# -----------------------------------------------------------------------------------
# TASK 4
# -----------------------------------------------------------------------------------

# perform Tukey's test

TukeyHSD(model, conf.level=.95) 
plot(TukeyHSD(model, conf.level=.95))

