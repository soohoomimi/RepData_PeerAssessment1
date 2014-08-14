### Navigate to working directory containing forked repo files
setwd("~/Coursera/Reproducible Research/RepData_PeerAssessment1")

### Load the data

act<-read.csv("activity.csv")

## What is the mean total number of steps taken per day?

### Remove NA's from dataset

act.na<-na.omit(act) 

# 1. Make a histogram of the total number of steps taken each day

library(reshape2)
totalsteps<-tapply(act.na$steps, act.na$date, sum)
totalsteps<-melt(totalsteps, id.vars="date")
names(totalsteps)<-c("date","total.steps")

library(ggplot2)
qplot(total.steps, data=totalsteps)

# 2. Calculate and report the mean and median total number of steps taken per day

mean(totalsteps$total.steps, na.rm=TRUE)
median(totalsteps$total.steps,na.rm=TRUE)

## What is the average daily activity pattern?

# 1. Make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days
library(plyr)
avginterval<-ddply(act.na, "interval", summarise, avg.steps=mean(steps))
qplot(interval, avg.steps, data=avginterval, geom="line")

# 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max<-max(avginterval$avg.steps)
avginterval[avginterval$avg.steps==max,]

## Imputing missing values

# 1. Calculate and report the total number of missing values in the dataset

nrow(act[act$steps=="NA",])

# 2. Devise strategy for filling in NA's (average steps for each interval, averaged across all days, e.g. output from daily activity pattern question 1)

library(plyr)
impute.avg<- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

# 3. Create a new dataset that is equal to the original but with missing values filled in

act2 <- ddply(act, ~ interval, transform, steps = impute.avg(steps))
act2<-act2[order(act2$date), ]

# 4. Make a histogram of the total number of steps taken each day, calculate and report mean and median total steps taken per day

totalsteps2<-tapply(act2$steps, act2$date, sum)
totalsteps2<-melt(totalsteps2, id.vars="date")
names(totalsteps2)<-c("date","total.steps")
qplot(total.steps, data=totalsteps2)

mean(totalsteps2$total.steps)
median(totalsteps2$total.steps)

# Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps? 
# Mean is not changed by inputting NA values with interval average (averaged across all days), but does shift the median to match the mean exactly. Because we are looking at totals,
# an imputing of (positive) values in place of NAs will increase the average total and median total. 

## Are there differences in activity patterns between weekdays and weekends?

act2$date<-as.character(act2$date) # convert date column to character, in order to feed into as.Date() function
act2$date<-as.Date(act2$date) # convert date column to date class
act3<-ddply(act2, ~ date, transform, weekday = weekdays(date)) # identify day of week

# 1. Create a new factor variable in the dataset with two levels, weekday and weekend

weekday<-c("Monday","Tuesday","Wednesday","Thursday","Friday")

# ifelse similar to if() command in Excel

act3["daytype"]<-ifelse(act3$weekday %in% weekday==TRUE, "weekday","weekend")
act3$daytype<-as.factor(act3$daytype)

# 2. Make a panel plot containing a time series plot (type "l") of the intervals (x-axis) and the average number of steps taken,
#    averaged across all weekday days or weekend days (y-axis). Plot should match plot created from simulated data.

avgdaytype<-ddply(act3, .(interval,daytype), summarise, avg.steps=mean(steps))

library(lattice)
xyplot(avg.steps ~ interval | daytype, data=avgdaytype,layout=c(1,2), type="l", xlab="Interval", ylab="Number of steps")

