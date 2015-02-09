---
title: "Reproducible Research: Peer Assessment 1"
output: 
html_document:
keep_md: true
---



```r
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.1.1
```

```r
library(lattice)
library(xtable)
```

```
## Warning: package 'xtable' was built under R version 3.1.2
```


## Loading and preprocessing the data

```r
#Assuming the data is in the following directory
activity <- read.csv("d://RWorkingDir//repdata-data-activity//activity.csv",header=TRUE)
```

## What is mean total number of steps taken per day?

```r
#group by date , calculating total number of steps taken per day
total_steps_by_date <- ddply(activity,.(date),summarize,totalSteps = sum(steps))

#histogram of total number of steps taken each day
hist(total_steps_by_date$totalSteps,xlab="Total Steps On Each Day",col="blue",ylim=c(0,30),main="Histogram of the total number \n of steps taken each day\n (has NAs)")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

### Mean and Median of total number of steps taken per day

```r
#group by date, calculating mean and median of steps taken per day
summary_steps_by_date <- ddply(activity,.(date),summarize,mean=mean(steps),median=median(steps))
#reporting the mean and median of total number of steps taken per day
print(summary_steps_by_date,type="html")
```

```
##          date       mean median
## 1  2012-10-01         NA     NA
## 2  2012-10-02  0.4375000      0
## 3  2012-10-03 39.4166667      0
## 4  2012-10-04 42.0694444      0
## 5  2012-10-05 46.1597222      0
## 6  2012-10-06 53.5416667      0
## 7  2012-10-07 38.2465278      0
## 8  2012-10-08         NA     NA
## 9  2012-10-09 44.4826389      0
## 10 2012-10-10 34.3750000      0
## 11 2012-10-11 35.7777778      0
## 12 2012-10-12 60.3541667      0
## 13 2012-10-13 43.1458333      0
## 14 2012-10-14 52.4236111      0
## 15 2012-10-15 35.2048611      0
## 16 2012-10-16 52.3750000      0
## 17 2012-10-17 46.7083333      0
## 18 2012-10-18 34.9166667      0
## 19 2012-10-19 41.0729167      0
## 20 2012-10-20 36.0937500      0
## 21 2012-10-21 30.6284722      0
## 22 2012-10-22 46.7361111      0
## 23 2012-10-23 30.9652778      0
## 24 2012-10-24 29.0104167      0
## 25 2012-10-25  8.6527778      0
## 26 2012-10-26 23.5347222      0
## 27 2012-10-27 35.1354167      0
## 28 2012-10-28 39.7847222      0
## 29 2012-10-29 17.4236111      0
## 30 2012-10-30 34.0937500      0
## 31 2012-10-31 53.5208333      0
## 32 2012-11-01         NA     NA
## 33 2012-11-02 36.8055556      0
## 34 2012-11-03 36.7048611      0
## 35 2012-11-04         NA     NA
## 36 2012-11-05 36.2465278      0
## 37 2012-11-06 28.9375000      0
## 38 2012-11-07 44.7326389      0
## 39 2012-11-08 11.1770833      0
## 40 2012-11-09         NA     NA
## 41 2012-11-10         NA     NA
## 42 2012-11-11 43.7777778      0
## 43 2012-11-12 37.3784722      0
## 44 2012-11-13 25.4722222      0
## 45 2012-11-14         NA     NA
## 46 2012-11-15  0.1423611      0
## 47 2012-11-16 18.8923611      0
## 48 2012-11-17 49.7881944      0
## 49 2012-11-18 52.4652778      0
## 50 2012-11-19 30.6979167      0
## 51 2012-11-20 15.5277778      0
## 52 2012-11-21 44.3993056      0
## 53 2012-11-22 70.9270833      0
## 54 2012-11-23 73.5902778      0
## 55 2012-11-24 50.2708333      0
## 56 2012-11-25 41.0902778      0
## 57 2012-11-26 38.7569444      0
## 58 2012-11-27 47.3819444      0
## 59 2012-11-28 35.3576389      0
## 60 2012-11-29 24.4687500      0
## 61 2012-11-30         NA     NA
```

## What is the average daily activity pattern?

```r
#group by interval, calculating average number of steps taken, averaged across all days
by_interval <- ddply(activity,.(interval),summarize,meanSteps = mean(steps,na.rm=TRUE))

#plotting time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
plot(by_interval$interval,by_interval$meanSteps,type="l",xlab="5-minute interval",ylab="average number of steps taken averaged across all days")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

### On average across all the days in the dataset, 835 is the 5-minute interval that contains the maximum number of steps



## Imputing missing values


### The total number of missing values in the dataset are 2304
### Strategy for filling in all of the missing values =  mean for that 5-minute interval


```r
#Creating a new dataset named activity_impute that is equal to the original dataset but with the missing data filled in with mentioned strategy
activity_impute <- activity
for (i in which(is.na(activity$steps))) {
        activity_impute[i,]$steps <- by_interval$meanSteps[by_interval$interval==activity[i,]$interval]
        }

#group by date , calculating total number of steps taken per day
total_steps_by_date_impute <- ddply(activity_impute,.(date),summarize,totalSteps = sum(steps))

#histogram of total number of steps taken each day
hist(total_steps_by_date_impute$totalSteps,xlab="Total Steps On Each Day",col="blue",ylim=c(0,35),main="Histogram of the total number \n of steps taken each day\n (imputed NAs)")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

### Mean and Median of total number of steps taken per day for imputed NA's

```r
#group by date, calculating mean and median of steps taken per day
summary_steps_by_date_impute <- ddply(activity_impute,.(date),summarize,mean=mean(steps),median=median(steps))

#reporting the mean and median of total number of steps taken per day
print(summary_steps_by_date_impute,type="html")
```

```
##          date       mean   median
## 1  2012-10-01 37.3825996 34.11321
## 2  2012-10-02  0.4375000  0.00000
## 3  2012-10-03 39.4166667  0.00000
## 4  2012-10-04 42.0694444  0.00000
## 5  2012-10-05 46.1597222  0.00000
## 6  2012-10-06 53.5416667  0.00000
## 7  2012-10-07 38.2465278  0.00000
## 8  2012-10-08 37.3825996 34.11321
## 9  2012-10-09 44.4826389  0.00000
## 10 2012-10-10 34.3750000  0.00000
## 11 2012-10-11 35.7777778  0.00000
## 12 2012-10-12 60.3541667  0.00000
## 13 2012-10-13 43.1458333  0.00000
## 14 2012-10-14 52.4236111  0.00000
## 15 2012-10-15 35.2048611  0.00000
## 16 2012-10-16 52.3750000  0.00000
## 17 2012-10-17 46.7083333  0.00000
## 18 2012-10-18 34.9166667  0.00000
## 19 2012-10-19 41.0729167  0.00000
## 20 2012-10-20 36.0937500  0.00000
## 21 2012-10-21 30.6284722  0.00000
## 22 2012-10-22 46.7361111  0.00000
## 23 2012-10-23 30.9652778  0.00000
## 24 2012-10-24 29.0104167  0.00000
## 25 2012-10-25  8.6527778  0.00000
## 26 2012-10-26 23.5347222  0.00000
## 27 2012-10-27 35.1354167  0.00000
## 28 2012-10-28 39.7847222  0.00000
## 29 2012-10-29 17.4236111  0.00000
## 30 2012-10-30 34.0937500  0.00000
## 31 2012-10-31 53.5208333  0.00000
## 32 2012-11-01 37.3825996 34.11321
## 33 2012-11-02 36.8055556  0.00000
## 34 2012-11-03 36.7048611  0.00000
## 35 2012-11-04 37.3825996 34.11321
## 36 2012-11-05 36.2465278  0.00000
## 37 2012-11-06 28.9375000  0.00000
## 38 2012-11-07 44.7326389  0.00000
## 39 2012-11-08 11.1770833  0.00000
## 40 2012-11-09 37.3825996 34.11321
## 41 2012-11-10 37.3825996 34.11321
## 42 2012-11-11 43.7777778  0.00000
## 43 2012-11-12 37.3784722  0.00000
## 44 2012-11-13 25.4722222  0.00000
## 45 2012-11-14 37.3825996 34.11321
## 46 2012-11-15  0.1423611  0.00000
## 47 2012-11-16 18.8923611  0.00000
## 48 2012-11-17 49.7881944  0.00000
## 49 2012-11-18 52.4652778  0.00000
## 50 2012-11-19 30.6979167  0.00000
## 51 2012-11-20 15.5277778  0.00000
## 52 2012-11-21 44.3993056  0.00000
## 53 2012-11-22 70.9270833  0.00000
## 54 2012-11-23 73.5902778  0.00000
## 55 2012-11-24 50.2708333  0.00000
## 56 2012-11-25 41.0902778  0.00000
## 57 2012-11-26 38.7569444  0.00000
## 58 2012-11-27 47.3819444  0.00000
## 59 2012-11-28 35.3576389  0.00000
## 60 2012-11-29 24.4687500  0.00000
## 61 2012-11-30 37.3825996 34.11321
```

## Are there differences in activity patterns between weekdays and weekends?

```r
#Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
activity_impute$dayType <- weekdays(as.Date(activity_impute$date))
activity_impute$dayType[!(activity_impute$dayType %in% c("Saturday","Sunday"))] = "weekday"
activity_impute$dayType[activity_impute$dayType %in% c("Saturday","Sunday")] = "weekend"
activity_impute$dayType <- as.factor(activity_impute$dayType)

#group by date and interval, calculating average number of steps taken, averaged across all weekday days or weekend days
by_interval_dayType <- ddply(activity_impute,.(interval,dayType),summarize,steps = mean(steps))

#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
xyplot(steps ~ interval| dayType,data=by_interval_dayType,type="l",xlab="Interval",ylab="Number of Steps",layout=c(1,2))
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 
