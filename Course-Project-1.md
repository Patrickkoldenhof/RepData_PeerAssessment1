---
title: "Course Project 1"
author: "Patrick Koldenhof"
date: "23-11-2020"
output:
  html_document: 
    keep_md: true
  pdf_document: default
---



## Load Data

```r
unzip("Activity.zip")
Activity <- read_csv("activity.zip")
```

## Steps per day
Calculate the total number of steps taken per day. This is plotted in the histogram. Mean and median for the total number of steps per day is calculated 

```r
stepsperday <- Activity %>% group_by(date) %>% summarise(steps = sum(steps))
hist(stepsperday$steps, breaks = 20, main = paste("Total Steps Each Day"), col="red",xlab="Number of Steps")
```

![](Course-Project-1_files/figure-html/pressure-1.png)<!-- -->

```r
dev.copy(png,"plot1.png", width=480, height=480)
dev.off()

meanstepsperday <- mean(stepsperday$steps, na.rm = TRUE)
meanstepsperday
medianstepsperday <- median(stepsperday$steps, na.rm = TRUE)
medianstepsperday
```

```
## png 
##   3 
## png 
##   2 
## [1] 10766.19
## [1] 10765
```

## Steps per interval
Plotting the 5-minute interval against the average number of steps taken in that interval. Consequently calculating the interval containing the most steps

```r
stepsperinterval <- Activity %>% group_by(interval) %>% summarise (steps = mean(steps, na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
plot(stepsperinterval$interval, stepsperinterval$steps, type = "l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Day by Interval")
```

![](Course-Project-1_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
dev.copy(png,"plot2.png", width=480, height=480)
dev.off()

maxinterval <- stepsperinterval[which.max(stepsperinterval$steps), 1]
maxinterval
```

```
## png 
##   3 
## png 
##   2 
## # A tibble: 1 x 1
##   interval
##      <dbl>
## 1      835
```

## Imputing missing values
First calculating how many rows have missing values. Subsequently the NAs are replaced by the mean step values for each interval. Over this dataset the mean/median of total steps per day is calculated and compared to the first derived average steps per day. 


```r
isNA <- sum(is.na(Activity$steps))
isNA

MeanStepsPerInterval<-function(interval){
    stepsperinterval[stepsperinterval$interval==interval,]$steps
}

ImputedActivity<-Activity
for(i in 1:nrow(ImputedActivity)){
    if(is.na(ImputedActivity[i,]$steps)){
        ImputedActivity[i,]$steps <- MeanStepsPerInterval(ImputedActivity[i,]$interval)
    }
}

totalStepsPerDayImputedActivity <- aggregate(steps ~ date, data=ImputedActivity, sum)
hist(totalStepsPerDayImputedActivity$steps, main = paste("Total Steps Each Day"), col="red",xlab="Number of Steps") 
```

![](Course-Project-1_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
dev.copy(png,"plot3.png", width=480, height=480)
dev.off()

meantotalstepsperdayimputed <- mean(totalStepsPerDayImputedActivity$steps)
meantotalstepsperdayimputed

mediantotalstepsperdayimputed <- median(totalStepsPerDayImputedActivity$steps)
mediantotalstepsperdayimputed
```

```
## [1] 2304
## png 
##   3 
## png 
##   2 
## [1] 10766.19
## [1] 10766.19
```
The mean did not change, which is logical. We replaced the NA values with the mean values for each interval, so it is likely the mean per day does not change. The median did change by a value of 1.18. 


## Weekdays vs Weekenddays

```r
library(lubridate)
ImputedActivity$date <- ymd(Activity$date)
ImputedActivity$weekdays <- wday(Activity$date, label = TRUE)

ImputedActivity$DayType <- ifelse(ImputedActivity$weekdays=='za' | ImputedActivity$weekdays=='zo', 'weekend','weekday')
perintervalanddaytype <- ImputedActivity %>% group_by(interval, DayType) %>% summarise (steps = mean(steps, na.rm = TRUE))

library(ggplot2)
Graph <- ggplot(perintervalanddaytype, aes(x = interval , y = steps, color = DayType)) +
       geom_line() +
       labs(title = "Mean daily steps by type of date", x = "Interval", y = "Average number of steps") +
       facet_wrap(~DayType, ncol = 1, nrow=2)
print(Graph)
```

![](Course-Project-1_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
dev.copy(png,"plot4.png", width=480, height=480)
dev.off()
```

```
## png 
##   3 
## png 
##   2
```


