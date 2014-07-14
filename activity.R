library (ggplot2)
## Loading and preprocessing the data
getwd()

##sourcedata <- read.csv(file="activity.csv", sep=",", header=TRUE)

sourcedata$steps <- as.numeric(sourcedata$steps)
sourcedata$date <- as.Date(sourcedata$date)

## What is mean total number of steps taken per day?
## 1. Make a histogram of the total number of steps taken each day

meanStepData <- aggregate(steps~date, data=sourcedata, FUN=sum, na.rm=TRUE)
hist(meanStepData$steps, main="Total Steps Taken per Day", xlab="Steps", ylab="Total number of steps")

##2. Calculate and report the mean and median total number of steps taken per day
mean(meanStepData$steps)
median(meanStepData$steps)
summary(meanStepData$steps, na.rm=TRUE, digits = 10)

## What is the average daily activity pattern?
## 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
## and the average number of steps taken, averaged across all days (y-axis)

avgStepsPerInterval <- aggregate(steps~interval, data=sourcedata, FUN=mean, na.rm=TRUE)
plot(avgStepsPerInterval$interval, avgStepsPerInterval$steps, type="l", 
     main="The average number of steps \n taken in 5-minutes interval across all days",
     xlab="Interval", ylab="Average number of steps")

## 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
which.max(avgStepsPerInterval$steps)
avgStepsPerInterval[104,]


## Imputing missing values
## 1. Calculate and report the total number of missing values 
## in the dataset (i.e. the total number of rows with NAs)