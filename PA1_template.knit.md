---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
keep_md: true
---
Activity Monotiring Assignment
==========

**This assignment aims to invesigate the activity patters. Average number of steps is 
shown per day and per interval**


Code for reading in the dataset and/or processing the data

```r
library(ggplot2)
setwd(getwd())
data<-read.csv("activity.csv", header = TRUE, sep = ",")
data$date<-as.Date(data$date)
head(data)
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

Calculate mean steps per day


```r
steps.per.day <- aggregate(steps ~ date, 
                           data = data, sum)
```

Histogram of the total number of steps taken each day


```r
ggplot(data=steps.per.day, aes(steps.per.day$steps)) + 
  geom_histogram(bins = 5, color = "#00AFBB")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-3-1.png" width="672" />

Mean and median number of steps taken each day


```r
sum.stat<-summary(steps.per.day$steps)
sum.stat[c(3,4)]
```

```
##   Median     Mean 
## 10765.00 10766.19
```

Calculate mean steps per interval


```r
steps.per.interval <- aggregate(steps ~ data$interval, 
                                data = data, mean)
```

Time series plot of the average number of steps taken


```r
ggplot(data=steps.per.interval, aes(steps.per.interval$`data$interval`, 
                                    steps.per.interval$steps)) + 
  geom_line()
```

<img src="PA1_template_files/figure-html/unnamed-chunk-6-1.png" width="672" />

The 5-minute interval that, on average, contains the maximum number of steps


```r
steps.per.interval$`data$interval`[which.max(steps.per.interval$steps)]
```

```
## [1] 835
```

Code to describe data


```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
sum(is.na(data$date))
```

```
## [1] 0
```

```r
sum(is.na(data$interval))
```

```
## [1] 0
```

Impute NAs with the average of that interval
If the value is NA, impute, otherwice keep original


```r
data.imputed<-data

for (i in 1:nrow(data.imputed)){
  if(is.na(data.imputed$steps[i])){
    data.imputed$steps[i]<- steps.per.interval$steps[
      steps.per.interval$`data$interval`==data.imputed$interval[i]]
  }else{
    data.imputed$steps[i]<-data.imputed$steps[i]}
}

head(data.imputed)
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

Calculate mean steps per day for imputed data


```r
steps.per.day.imp <- aggregate(steps ~ date, 
                           data = data.imputed, sum)
```

Histogram of the total number of steps taken each day after missing values are imputed


```r
ggplot(data=steps.per.day.imp, aes(steps.per.day.imp$steps)) + 
  geom_histogram(bins = 5, color = "#00AFBB")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-11-1.png" width="672" />

Calculating summary statistics for imputed data and comparing to non-imputed data 


```r
sum.stat.imp<-summary(steps.per.day.imp$steps)
rbind(sum.stat.imp[c(3,4)], sum.stat[c(3,4)])
```

```
##        Median     Mean
## [1,] 10766.19 10766.19
## [2,] 10765.00 10766.19
```

Create a new variable "week.days" which would record weekends and weekdays


```r
for (i in 1:nrow(data.imputed)){
  if(weekdays(data.imputed$date[i])=="Samstag"){
      data.imputed$week.days[i]<- "Weekend"
  }else if(weekdays(data.imputed$date[i])=="Sonntag"){
      data.imputed$week.days[i]<- "Weekend"
  }else{
      data.imputed$week.days[i]<- "Weekday"
  }
}
```

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
indicating whether a given date is a weekday or weekend day.


```r
data.imputed$week.days<-factor(data.imputed$week.days)
```

Calculate average steps per interval based on imputed data


```r
steps.per.interval.imp <- aggregate(steps ~ data.imputed$interval + data.imputed$week.days, 
                                data = data.imputed, mean)
```

Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends


```r
ggplot(data=steps.per.interval.imp, 
        aes(`data.imputed$interval`, steps)) + 
                                 geom_line() + 
                     facet_grid(`data.imputed$week.days`~.)
```

<img src="PA1_template_files/figure-html/unnamed-chunk-16-1.png" width="672" />
