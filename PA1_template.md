# PA1_template
Warren Chanzit  
April 3, 2016  

## Pre-Analysis set-up.
### Import packages and set working directory.

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
library(reshape2)
```

### Read and process data.

```r
data <- read.csv("activity.csv")
data$date <- as.Date(data$date)
data2 <- na.omit(data)
```

## Calculate and plot total steps per day.

```r
daySteps <- with(data2, tapply(steps, date, sum))
hist(daySteps,
     breaks = 10,
     xlab = "Steps",
     main = "Daily Steps Taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)


```r
mean(daySteps)
```

```
## [1] 10766.19
```

```r
median(daySteps)
```

```
## [1] 10765
```

## Investigate average daily activity patterns.

```r
intervals <- with(data2, tapply(steps, interval, mean))
plot(names(intervals),intervals,type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)

```r
intervals[intervals == max(intervals)]
```

```
##      835 
## 206.1698
```

## Imput missing values.

```r
sum(is.na(data))
```

```
## [1] 2304
```


```r
int_df <- as.data.frame(intervals)
names(int_df) <- "ave_steps"
int_df$interval <- as.integer(dimnames(int_df)[[1]])

imputed <- inner_join(data, int_df)
```

```
## Joining by: "interval"
```

```r
nas <- is.na(imputed$steps)
imputed$steps[nas] <- imputed$ave_steps[nas]

dayStepsIm <- with(imputed, tapply(steps, date, sum))
hist(dayStepsIm,
      breaks = 10,
     xlab = "Steps",
     main = "Daily Steps Taken, Using Imputed Values")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)


```r
mean(dayStepsIm)
```

```
## [1] 10766.19
```

```r
median(dayStepsIm)
```

```
## [1] 10766.19
```

## Weekdays vs weekends.

```r
weekdays <- weekdays(imputed$date)
weekdays <- ifelse(weekdays %in% c("Saturday","Sunday"),
                   "Weekend",
                   "Weekday")
imputed$weekday <- as.factor(weekdays)

wkAvgs <- with(imputed, tapply(steps, list(interval, weekday), mean))
wkAvgs <- as.data.frame(wkAvgs)
wkAvgs$interval <- dimnames(wkAvgs)[[1]]
wkAvgs <- wkAvgs %>%
  melt(id = "interval", value.name = "steps")
names(wkAvgs)[2] <- "weekday"
wkAvgs$interval <- as.numeric(wkAvgs$interval)
```


```r
g <- ggplot(wkAvgs, aes(interval,steps))
g + geom_line() +
  facet_grid(. ~ weekday) +
  labs(x = "5-Minute Interval",
       y = "Steps",
       title = "Daily Average Step Count")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)

Weekend step activity seems somewhat higher with the glaring exception of weekday rush hour.


