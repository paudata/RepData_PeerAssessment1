# Reproducible Research: Peer Assessment 1

Loading needed libraries.

```r
require(plyr)
```

```
## Loading required package: plyr
```

```r
require(ggplot2)
```

```
## Loading required package: ggplot2
```

## Loading and preprocessing the data

```r
unzip("activity.zip")
activity <- read.csv("activity.csv", header = TRUE)
activity <- transform(activity, date = as.Date(date))
```

## What is mean total number of steps taken per day?
Find the total number of steps taken per day.

```r
steps_day <- ddply(activity, ~date, summarise, steps = sum(steps))
```

Plot a histogram of the total number of steps taken per day.

```r
p <- ggplot(steps_day, aes(steps))
p <- p + geom_histogram(fill = "red", color = "black", binwidth = 500)
p <- p + ggtitle("Total number of steps per day")
p + xlab("Steps per day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

Compute the `mean` and `median` total number of steps taken per day.

```r
mean_steps_day <- mean(steps_day$steps, na.rm = TRUE)
median_steps_day <- median(steps_day$steps, na.rm = TRUE)
```

- The mean of total number of steps per day is 10766
- The median of total number of steps per day is 10765
## What is the average daily activity pattern?
Find the average number of steps taken per 5 minute interval.

```r
avg_steps_interval <- ddply(activity, ~interval, summarise, mean = mean(steps, 
    na.rm = T))
```

Make a time series plot of the 5-minute interval and the average number of steps taken and averaged across all days.

```r
p <- ggplot(avg_steps_interval, aes(interval, mean)) + geom_line()
p <- p + ggtitle("The average daily activity pattern")
p + xlab("Interval") + ylab("Number of steps")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

Find the 5-minute interval that contains the maximum number of steps on average across all the days in the dataset.

```r
max_id <- which.max(avg_steps_interval$mean)
max_interval <- avg_steps_interval$interval[max_id]
```

- The 5-minute interval, on average across all days, that contains the maximum number of steps is 835
## Imputing missing values
Calculate the total number of missing values in the dataset.

```r
number_NA <- sum(apply(is.na(activity), 1, any))
```

- The total number of missing values in the dataset is 2304
Create a function replacing the NA's step by the mean of 5-minute interval averaged across all days.

```r
na.replace <- function(act) {
    ddply(act, ~interval, function(dd) {
        steps <- dd$steps
        dd$steps[is.na(steps)] <- mean(steps, na.rm = TRUE)
        return(dd)
    })
}
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
imputed_activity <- na.replace(activity)
```

Find the total number of steps taken each day.

```r
imputed_steps_day <- ddply(imputed_activity, ~date, summarise, steps = sum(steps))
```

Make a histogram of the total number of steps taken each day.

```r
p <- ggplot(imputed_steps_day, aes(steps))
p <- p + geom_histogram(fill = "red", color = "black", binwidth = 500)
p <- p + ggtitle("Total number of steps per day")
p + xlab("Steps per day")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 

Calculate `mean` and `median` total number of steps taken per day.

```r
imputed_mean_steps_day <- mean(imputed_steps_day$steps)
imputed_median_steps_day <- median(imputed_steps_day$steps)
```

- The mean of total number steps per day is
10766
- The median of total number steps per day is
10766
The imputation slightly impacted on the median total number of steps taken per day. It was changed from 10765 to 10766. The mean total number of steps taken per day remained the same. Usually the imputing of missing values can introduce bias in an estimates but in our case impact of it on the estimates of the total daily number of steps is negligible.
## Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable `weekpart` in the dataset with two levels "weekday" and "weekend".

```r
week_parts <- c("Weekday", "Weekend")
date_weekpart <- function(date) {
    day <- weekdays(date)
    part <- factor("Weekday", week_parts)
    if (day %in% c("Saturday", "Sunday")) 
        part <- factor("Weekend", week_parts)
    return(part)
}
imputed_activity$weekpart <- sapply(imputed_activity$date, date_weekpart)
```

Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days.

```r
avg_steps <- ddply(imputed_activity, .(interval, weekpart), summarise, mean = mean(steps))
p <- ggplot(avg_steps, aes(x = interval, y = mean))
p <- p + geom_line() + facet_grid(. ~ weekpart, )
p <- p + ggtitle("Activity patterns on weekends and weekdays")
p + xlab("Interval") + ylab("Number of steps")
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16.png) 

