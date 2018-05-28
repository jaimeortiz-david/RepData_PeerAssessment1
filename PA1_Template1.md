---
title: "PA1_template"
output: 
  html_document:
      keep_md: TRUE
---

Read in the data in the activity.csv file.


```r
activityData <- read.csv(file="activity.csv", header=TRUE)
```

##What is mean total number of steps taken per day?


```r
# Calculate total steps per day
totalSteps <- aggregate(steps ~ date, activityData, FUN=sum)

# Make a histogram 
hist(totalSteps$steps,
     main = "Total Steps per Day",
     xlab = "Number of Steps")
```

![](PA1_Template1_files/figure-html/unnamed-chunk-2-1.png)<!-- -->



```r
# Calculate and report the mean and median of total steps taken per day
meanSteps <- mean(totalSteps$steps, na.rm = TRUE)
medSteps <- median(totalSteps$steps, na.rm = TRUE)
```


Mean Number of Steps Taken per Day is $$1.076618910 * 10^{4}$$   
Median Number of Steps Taken per Day is $$10765$$

##What is the average daily activity pattern?


```r
# Make a time-series plot of the 5-minute interval and the average number of
# steps taken, averaged acoss all days.
library(ggplot2)
meanStepsByInt <- aggregate(steps ~ interval, activityData, mean)
ggplot(data = meanStepsByInt, aes(x = interval, y = steps)) +
  geom_line() +
  ggtitle("Average Daily Activity Pattern") +
  xlab("5-minute Interval") +
  ylab("Average Number of Steps") +
  theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_Template1_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


### Which 5-minute interval across all days contain the maximum number of steps

```r
(maxInt <- meanStepsByInt[which.max(meanStepsByInt$steps),])
```

```
##     interval    steps
## 104      835 206.1698
```

The 5-minute interval that contains the maximum of steps, on average across all days, is 835.

##Missing Values


```r
# Calculate and report the total number of missing values in the dataset
(missingVals <- sum(is.na(activityData$steps)))
```

```
## [1] 2304
```

### Devise a strategy for filling in all of the missing values

There are 2304 missing values. I will replace these missing values with hte average of the steps attribute.


```r
# Find the NA positions
na_pos <- which(is.na(activityData$steps))

# Create a vector of means
mean_vec <- rep(mean(activityData$steps, na.rm=TRUE), times=length(na_pos))

# Replace the NAs by the means
activityData[na_pos, "steps"] <- mean_vec

head(activityData)
```

```
##     steps       date interval
## 1 37.3826 2012-10-01        0
## 2 37.3826 2012-10-01        5
## 3 37.3826 2012-10-01       10
## 4 37.3826 2012-10-01       15
## 5 37.3826 2012-10-01       20
## 6 37.3826 2012-10-01       25
```

##New Histogram with replaced NAs values


```r
# Compute the total number of steps each day (NA values removed)
sum_data <- aggregate(activityData$steps, by=list(activityData$date), FUN=sum)

# Rename the attributes
names(sum_data) <- c("date", "total")

# Compute the histogram of the total number of steps each day
hist(sum_data$total, 
     breaks=seq(from=0, to=25000, by=2500),
     xlab="Total number of steps", 
     ylim=c(0, 30), 
     main="Histogram of the total number of steps taken each day\n(NA replaced by mean value)")
```

![](PA1_Template1_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


```r
mean(sum_data$total)
```

```
## [1] 10766.19
```

```r
median(sum_data$total)
```

```
## [1] 10766.19
```

These formulas gives a mean and median of 10766 and 10766 respectively.

These values differ greatly from the estimates from the first part of the assignment. The impact of imputing the missing values is to have more data, hence to obtain a bigger mean and median value.

##Are there differences in activity patterns between weekdays and weekends?


```r
# Transform the date attribute to an actual date format
activityData$date <- as.POSIXct(activityData$date, format="%Y-%m-%d")

# Compute the weekdays from the date attribute
activityData <- data.frame(date=activityData$date, 
                           weekday=tolower(weekdays(activityData$date)), 
                           steps=activityData$steps, 
                           interval=activityData$interval)

# Compute the day type (weekend or weekday)
activityData <- cbind(activityData, 
                      daytype=ifelse(activityData$weekday == "saturday" | 
                                     activityData$weekday == "sunday", "weekend", 
                                     "weekday"))

# Create the final data.frame
activityData <- data.frame(date=activityData$date, 
                       weekday=activityData$weekday, 
                       daytype=activityData$daytype, 
                       interval=activityData$interval,
                       steps=activityData$steps)

head(activityData)
```

```
##         date weekday daytype interval   steps
## 1 2012-10-01  monday weekday        0 37.3826
## 2 2012-10-01  monday weekday        5 37.3826
## 3 2012-10-01  monday weekday       10 37.3826
## 4 2012-10-01  monday weekday       15 37.3826
## 5 2012-10-01  monday weekday       20 37.3826
## 6 2012-10-01  monday weekday       25 37.3826
```

```r
# Load the lattice graphical library
library(lattice)

# Compute the average number of steps taken, averaged across all daytype variable
mean_data <- aggregate(activityData$steps, 
                       by=list(activityData$daytype, 
                               activityData$weekday, activityData$interval), mean)

# Rename the attributes
names(mean_data) <- c("daytype", "weekday", "interval", "mean")

head(mean_data)
```

```
##   daytype  weekday interval     mean
## 1 weekday   friday        0 8.307244
## 2 weekday   monday        0 9.418355
## 3 weekend saturday        0 4.672825
## 4 weekend   sunday        0 4.672825
## 5 weekday thursday        0 9.375844
## 6 weekday  tuesday        0 0.000000
```

```r
# Compute the time serie plot
xyplot(mean ~ interval | daytype, mean_data, 
       type="l", 
       lwd=1, 
       xlab="Interval", 
       ylab="Number of steps", 
       layout=c(1,2))
```

![](PA1_Template1_files/figure-html/unnamed-chunk-10-1.png)<!-- -->











