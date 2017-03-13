rm(list = ls)

dir.create("ReproducableResearch")

setwd("ReproducableResearch")

download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",destfile = "factivity.zip")

unzip("factivity.zip")


activity<-read.csv("activity.csv",na.strings = "NA")

activity$date<-as.Date(activity$date,"%Y-%m-%d")

##part 1
sumperday<-tapply(activity$steps,activity$date,sum)

mean(sumperday)##a
median(sumperday)##c
summary(sumperday)

sumperday

hist(sumperday)##b

head(activity)

comp<-complete.cases(activity)
completeAct<-activity[comp,]

meanperint<-tapply(completeAct$steps,completeAct$interval,mean)
medperint<-tapply(completeAct$steps,completeAct$interval,median)
head(meanperint)
head(medperint)

plot(names(meanperint),meanperint,type="l")
lines(names(medperint),medperint,col = "red")##d

max(sumperday)##e

##part 2

activity2<-activity
medianAct<-cbind(names(medperint),medperint)
names(medianAct)<-c("interval","medSteps")
head(medianAct)
activity2$steps[is.na(activity$steps)==TRUE]<- median(activity$steps[is.na(activity$steps)==FALSE])


activity3<-activity
meanAct<-cbind(names(meanperint),meanperint)
names(meanAct)<-c("interval","meanSteps")
head(meanAct)
activity3$steps[is.na(activity$steps)==TRUE]<- mean(activity$steps[is.na(activity$steps)==FALSE])


##part 3

##1.
a<-nrow(activity)
c<-nrow(completeAct)
missingcnt<-a-c
missingcnt ##returns 2304 as an int value

##2.
#head(activity2$weekday)

activity2$daycat<-ifelse(wday(activity2$date) %in% c(1,7),"weekend","weekday")
activity2$daycat<-as.factor(activity2$daycat)
str(activity2)
summary(activity2)

sumperday2<-tapply(activity2$steps,activity2$date,sum)
sumperday3<-tapply(activity3$steps,activity3$date,sum)

mean(sumperday2)##a
median(sumperday2)##c
summary(sumperday2)

sumperday2
hist(sumperday)
hist(sumperday2)##more skewed.  Maybe should use mean instead of median.
hist(sumperday3)##use this one

install.packages("lubridate")
library(lubridate)



activity2$daycat<-ifelse(wday(activity2$date) %in% c(1,7),"weekend","weekday")
activity2$daycat<-as.factor(activity2$daycat)

weekendact<-activity2[activity2$daycat=="weekend",]

str(activity2)

par(mfrow = c(1,2))

meanweekday<-tapply(activity2$steps[activity2$daycat=="weekday"],activity2$interval[activity2$daycat=="weekday"],mean)
meanweekend<-tapply(activity2$steps[activity2$daycat=="weekend"],activity2$interval[activity2$daycat=="weekend"],mean)
head(meanweekday)
head(meanweekend)

plot(names(meanweekday),meanweekday,xlab="5 Minute Interval",ylab = "Mean Steps",main="Weekday Activity",type="l")

plot(names(meanweekend),meanweekend,xlab="5 Minute Interval",ylab = "Mean Steps",main="Weekend Activity",type="l")

