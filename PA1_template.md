---
title: "Peer grading assignment"
output: html_document
---

## Loading and preprocessing the data
* Load the data (i.e. read.csv())

```r
# Import the data
filename <- "activity.csv"
data <- read.csv(filename)
```
   
## What is mean total number of steps taken per day?
* Calculate the total number of steps taken per day

```r
days <- unique(data$date)
steps_per_day <- matrix(c(NA),length(days),2)
for (i in 1:length(days)) {
  index <- which(data$date == days[i])
	steps_per_day[i,1] <- days[i]
	steps_per_day[i,2] <- sum ( data$steps[index], na.rm=T )
}
colnames(steps_per_day) <- c("date","stepsPerDay")
```

* Make a histogram of the total number of steps taken each day

```r
hist(steps_per_day[,2],main="steps histogram",xlab="number of steps per day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 


* Calculate and report the mean and median of the total number of steps taken per day

```r
paste("the mean is",mean(steps_per_day[,2], na.rm=T))
```

```
## [1] "the mean is 9354.22950819672"
```

```r
paste("the median is",median(steps_per_day[,2], na.rm=T))
```

```
## [1] "the median is 10395"
```

## What is the average daily activity pattern?

* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
mean_timeseries <- matrix(c(0),length(unique(data$interval)),2)
for (i in 1:length(unique(data$interval))) {
  index <- which(data$interval == unique(data$interval)[i])
  mean_timeseries[i,1] <- unique(data$interval)[i]
	mean_timeseries[i,2] <- mean(data$steps[index],na.rm=T)
}
plot(mean_timeseries[,1],mean_timeseries[,2],
xlim=c(min(unique(data$interval)),max(unique(data$interval))),ylim=c(0,250),
type="l",xlab="time interval",ylab="mean steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
index <- which(mean_timeseries[,2] == max(mean_timeseries[,2]))
mean_timeseries[index,1]
```

```
## [1] 835
```

## Imputing missing values

* Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
index <- which(is.na(data$steps)==T)
length(index)
```

```
## [1] 2304
```

* Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
newdata <- data
for (i in 1:length(days)) {
  index_day <- which(data$date == days[i])
  index_NA <- which(is.na(data$steps[index_day])==T)
	if (length(index_NA)>0) {
		newdata$steps[index_day][index_NA] <- median(steps_per_day[,2], na.rm=T)
	}
}
```

* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
imputed_steps_per_day <- matrix(c(NA),length(days),2)
for (i in 1:length(days)) {
  index <- which(newdata$date == days[i])
	imputed_steps_per_day[i,1] <- days[i]
	imputed_steps_per_day[i,2] <- sum ( newdata$steps[index], na.rm=T )
}

hist(imputed_steps_per_day[,2])
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

```r
mean(imputed_steps_per_day[,2], na.rm=T)
```

```
## [1] 401978.5
```

```r
median(imputed_steps_per_day[,2], na.rm=T)
```

```
## [1] 11458
```

## Are there differences in activity patterns between weekdays and weekends?

* Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
dates <- as.Date(data$date,format="%Y-%m-%d")
day_of_week <- weekdays(dates,abbr=T)
index_weekends <- which(grepl("Sat|Sun",day_of_week)==T)
weekdata <- cbind(data,c("weekday","weekend"))
names(weekdata)[4] <- "day"
weekdata$day[-index_weekends] <- "weekday"
weekdata$day[index_weekends] <- "weekend"
```

* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
interval_weekday <- matrix(c(0),length(unique(weekdata$interval)),2)
interval_weekend <- matrix(c(0),length(unique(weekdata$interval)),2)
index_weekday <- which(weekdata$day=="weekday")
for (i in 1:length(unique(weekdata$interval))) {
  interval_weekday[i,1] <- unique(weekdata$interval)[i]
  interval_weekend[i,1] <- unique(weekdata$interval)[i]
	index_interval <- which(weekdata$interval == unique(weekdata$interval)[i])
	interval_weekday[i,2] <- mean(weekdata$steps[intersect(index_weekday,index_interval)],na.rm=T)
	interval_weekend[i,2] <- mean(weekdata$steps[intersect(index_weekends,index_interval)],na.rm=T)
}

# generate plots
plot(interval_weekday[,1],interval_weekday[,2],
xlim=c(min(unique(data$interval)),max(unique(data$interval))),ylim=c(0,250),
type="l",xlab="time interval",ylab="mean steps",col="black")
lines(interval_weekend[,1],interval_weekend[,2],type="l",col="red")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 
red-weekends, black-weekdays

