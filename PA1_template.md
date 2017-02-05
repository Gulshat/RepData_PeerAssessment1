# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
unzip("activity.zip")

activity <- read.csv("activity.csv")
dim(activity)
```

```
## [1] 17568     3
```

```r
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```


## What is mean total number of steps taken per day?

1) Histogram with total number of steps taken per day


```r
activity.data <- aggregate(steps ~ date, data=activity, FUN = sum)

barplot(activity.data$steps, names.arg = activity.data$date, xlab = "Date",
        ylab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

2) Calculate and report mean and median total number of steps taken per day?


```r
print("Mean total number of steps taken per day:")
```

```
## [1] "Mean total number of steps taken per day:"
```

```r
mean(activity.data$steps)
```

```
## [1] 10766.19
```

```r
print("Median total number of steps taken per day:")
```

```
## [1] "Median total number of steps taken per day:"
```

```r
median(activity.data$steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?
1) Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
library(ggplot2)
activity.interval <- aggregate(steps ~ interval, data=activity, FUN = mean)
ggplot(activity.interval, aes(x=activity.interval$interval, y=activity.interval$steps)) + geom_line()+
  ylab("Steps") + xlab("Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

2) Which 5 minutes intervals contains the maximum number of steps?


```r
activity.interval$interval[which.max(activity.interval$steps)]
```

```
## [1] 835
```

## Imputing missing values

1) Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

2) Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
I will use the means 5 minute interval as fillers for missing value

3) Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activity <- merge(activity, activity.interval, by="interval",
                  suffixes = c('','.y'))
nas <- is.na(activity$steps)
activity$steps[nas] <- activity$steps.y[nas]  
activity <- activity[,c(1:3)]
```

4) Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
activity.data <- aggregate(steps ~ date, data = activity, FUN = sum)
barplot(activity.data$steps, names.arg = activity.data$date, xlab = "Data",
     ylab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


```r
print("Mean total number of steps taken per day:")
```

```
## [1] "Mean total number of steps taken per day:"
```

```r
mean(activity.data$steps)
```

```
## [1] 10766.19
```

```r
print("Median total number of steps taken per day:")
```

```
## [1] "Median total number of steps taken per day:"
```

```r
median(activity.data$steps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?
1) Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
daytype <- function(date){
  if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
    "weekend"
  } else {
    "weekday"
  }
}
activity$daytype <- as.factor(sapply(activity$date, daytype))
```

2) Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
par(mfrow=c(2,1))
for (type in c("weekend", 'weekday')){
  step.type <- aggregate(steps ~ interval, 
                         data = activity,
                         subset = activity$daytype==type,
                         FUN = mean)
  plot(step.type, type="l", main = type)
}
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->



