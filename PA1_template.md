# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
unzip("activity.zip")
data <-read.csv("./activity.csv")
data2<-na.omit(data) 
library(lattice)
```


## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day

```r
numpasos<-setNames(with(data2,aggregate(steps,list(date),sum)),c("dates","numsteps"))
head(numpasos)
```

```
##        dates numsteps
## 1 2012-10-02      126
## 2 2012-10-03    11352
## 3 2012-10-04    12116
## 4 2012-10-05    13294
## 5 2012-10-06    15420
## 6 2012-10-07    11015
```
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
with(numpasos,hist(numsteps,main="Histogram of Number of Steps taken per Day",
col="steelblue",xlab="Number of Steps"))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day


```r
mean(numpasos$numsteps)
```

```
## [1] 10766.19
```

```r
median(numpasos$numsteps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
AvgStep<-setNames(with(data2,aggregate(steps,list(interval),mean)),c("interval","Avgstep"))
with(AvgStep,plot(interval,Avgstep,type="l",xlab="5-Minute Interval",ylab="Average Number of Steps taken",col="steelblue",main="Time Series Plot of the 5-Minute Interval"))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 


2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
AvgStep[AvgStep$Avgstep==max(AvgStep$Avgstep),1]
```

```
## [1] 835
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(data))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I will use the average of the intervals for replacing missing data (NA) on the steps column.


```r
data3<-data
for (i in 1:nrow(data3)){
        if(is.na(data3$steps[i])){
                data3$steps[i]<-AvgStep[which(data3$interval[i] == AvgStep$interval), ]$Avgstep
        }
}
```


3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
head(data3)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```



4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
numpasos2<-setNames(with(data3,aggregate(steps,list(date),sum)),c("dates","numsteps"))
with(numpasos2,hist(numsteps,main="Histogram of Number of Steps taken per Day (Reemplacing NA)",
col="steelblue",xlab="Number of Steps"))
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 


```r
mean(numpasos2$numsteps)
```

```
## [1] 10766.19
```

```r
median(numpasos2$numsteps)
```

```
## [1] 10766.19
```

No major differences in replacing the NA from the average and the median. If you win on the amount of data used.

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
data3$weekday<-weekdays(as.Date(data3$date))
data3$weekend<-c("Weekday")
for(i in 1:nrow(data3)){
        if (data3$weekday[i]=="Saturday" || data3$weekday[i]=="Sunday"){
                data3$weekend[i]<-"Weekend"
        }
}
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
avgSteps2 <- aggregate(data3$steps, list(interval = as.numeric(as.character(data3$interval)), weekend = data3$weekend),"mean")
names(avgSteps2)[3] <- "meanSteps"
xyplot(avgSteps2$meanSteps ~ avgSteps2$interval | avgSteps2$weekend, layout = c(1, 2), type = "l", xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 




