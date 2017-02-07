# Reproducible Research: Peer Assessment 1
#### Memduh Er

## Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

- Dataset: [Activity monitoring data][1] [52K]

The variables included in this dataset are:

- **steps:** Number of steps taking in a 5-minute interval (missing values are coded as **NA**)
- **date:** The date on which the measurement was taken in YYYY-MM-DD format
- **interval:** Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.


## Loading and preprocessing the data

Loading *knitr* package and setting global *echo* parameter as TRUE in order to be sure the R codes will be displayed through entire document.


```r
library(knitr)
opts_chunk$set(echo = TRUE)
```

1. Load the data


```r
data <- read.csv("activity.csv")
```

2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

There is no need to process data. Its structure is just how I need...

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day


```r
TotalStepsPerDay <- tapply(data$steps, data$date, sum)
```

2. Make a histogram of the total number of steps taken each day


```r
hist(TotalStepsPerDay, main = "Total Number of Steps (per Day)",
     xlab = "Total Steps",
     ylab = "Frequency",
     breaks = 20,
     col = "blue",
     xlim = c(0,25000),
     panel.first = grid())
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day


```r
mean_TotalStepsPerDay   <- mean(TotalStepsPerDay, na.rm = TRUE)
median_TotalStepsPerDay <- median(TotalStepsPerDay, na.rm = TRUE)
```



The mean of the total number of steps taken per day is **10766.19**  
The median of the total number of steps taken per day is **10765**

## What is the average daily activity pattern?

1. Make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days.


```r
AvgStepsByInterval <- setNames(aggregate(steps ~ interval, data, mean, na.rm = TRUE), c("interval", "AvgSteps"))
with(AvgStepsByInterval, plot(interval, 
                              AvgSteps,
                              main = "Average Steps By Intervals",
                              xlab = "Intervals",
                              ylab = "Average Steps",
                              col = "blue", 
                              type = "l",
                              panel.first = grid()
                              )
    )
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
MaxAvgSteps <- AvgStepsByInterval[which.max(AvgStepsByInterval$AvgSteps),]
```



5-minute interval, on average across all the days in the dataset, contains the maximum number of steps is **835th** and the number is **206.17**

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset 


```r
NumberOfMissingValues <- sum(is.na(data$steps))
```

Number of Missing Values in the dataset is **2304**.

2. Devise a strategy for filling in all of the missing values in the dataset.

My strategy will be fillinng missing values out with the average steps per intervals.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
MergedData <- merge(data, AvgStepsByInterval, by = "interval")
MergedData$RoundAvgSteps <- as.integer(round(MergedData$AvgSteps, digits = 0))
isNA <- is.na(MergedData$steps)
MergedData[isNA,2] <- MergedData[isNA,5]
MergedData <- MergedData[, 1:3]
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
TotalStepsPerDay2 <- tapply(MergedData$steps, MergedData$date, sum)
hist(TotalStepsPerDay2, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="red")
hist(TotalStepsPerDay, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="blue", add = T )
legend("topright", c("Imputed Data", "Original Data"), fill=c("red", "blue") )
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->


```r
mean_TotalStepsPerDay2   <- mean(TotalStepsPerDay2)
median_TotalStepsPerDay2 <- median(TotalStepsPerDay2)
```



The means are  **10766.19** and **10765.64**  
The medians are **10765** and **10762**  
It seems both medians and means are slightly different between the original and imputed data.

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
MergedData$DayTypeFactor <- ifelse(weekdays(as.Date(MergedData$date)) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
MergedDataWeekDay <- MergedData[MergedData$DayTypeFactor == "Weekday",][,1:3]
MergedDataWeekEnd <- MergedData[MergedData$DayTypeFactor == "Weekend",][,1:3]



AvgStepsByInterval_MergedDataWeekDay <- setNames(aggregate(steps ~ interval, MergedDataWeekDay, mean), c("interval", "AvgSteps"))
AvgStepsByInterval_MergedDataWeekEnd <- setNames(aggregate(steps ~ interval, MergedDataWeekEnd, mean), c("interval", "AvgSteps"))

#m <- matrix(c(1,1,2,2),2, 2, byrow = TRUE)
#layout(m, heights = c(50,50))

par(mfrow = c(2,1), mar = c(2,5,5,0))
with(AvgStepsByInterval_MergedDataWeekDay, plot(interval, 
                                                AvgSteps, 
                                                main = "WeekDays",
                                                xlab = "",
                                                ylab = "Steps",
                                                col = "red",
                                                type = "l",
                                                panel.first = grid()
                                                ))

with(AvgStepsByInterval_MergedDataWeekEnd, plot(interval, 
                                                AvgSteps, 
                                                main = "Weekends",
                                                col = "blue",
                                                xlab = "Intervals",
                                                ylab = "Steps",
                                                type = "l",
                                                panel.first = grid()
                                                ))
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png)<!-- -->



[1]: https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip
