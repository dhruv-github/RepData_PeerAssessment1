### 5/13/14 ###
# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
input <- read.table("activity.csv", sep = ",", header = T)
head(input)
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

```r
table(input$date)
```

```
## 
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##        288        288        288        288        288        288 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##        288        288        288        288        288        288 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##        288        288        288        288        288        288 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##        288        288        288        288        288        288 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##        288        288        288        288        288        288 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##        288        288        288        288        288        288 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##        288        288        288        288        288        288 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##        288        288        288        288        288        288 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##        288        288        288        288        288        288 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##        288        288        288        288        288        288 
## 2012-11-30 
##        288
```


## What is mean total number of steps taken per day?

```r
hist(input$steps, col = "red", xlab = "Number of steps", main = "Histogram of steps taken")
```

![plot of chunk mean_total_steps](figure/mean_total_steps.png) 

```r
require(plyr)
results <- ddply(input, "date", function(x) {
    data.frame(mean.steps = mean(x$steps, na.rm = T), median.steps = median(x$steps, 
        na.rm = T))
})
head(results)
```

```
##         date mean.steps median.steps
## 1 2012-10-01        NaN           NA
## 2 2012-10-02     0.4375            0
## 3 2012-10-03    39.4167            0
## 4 2012-10-04    42.0694            0
## 5 2012-10-05    46.1597            0
## 6 2012-10-06    53.5417            0
```


## What is the average daily activity pattern?

```r
out <- ddply(input, "interval", function(x) {
    data.frame(mean.steps = mean(x$steps, na.rm = T))
})
head(out)
```

```
##   interval mean.steps
## 1        0    1.71698
## 2        5    0.33962
## 3       10    0.13208
## 4       15    0.15094
## 5       20    0.07547
## 6       25    2.09434
```

```r
plot(out$interval, out$mean.steps, type = "l")
```

![plot of chunk average_daily_activity](figure/average_daily_activity.png) 

```r
subset(out, mean.steps == max(mean.steps))
```

```
##     interval mean.steps
## 104      835      206.2
```


## Imputing missing values

```r
sum(!complete.cases(input))
```

```
## [1] 2304
```

```r
# library(reshape2) input2 <- dcast(input,interval~date,value.var='steps')
# rownames(input2) <- input2$interval input2
# <-subset(input2,select=-interval) head(input2) input2.imputed <-
# data.frame(impute.knn(as.matrix(input2),k=20,colmax=1)$data,stringsAsFactors=F)
input2 <- head(merge(input, out, by = "interval"), n = 20)
for (i in 1:dim(input2)[1]) {
    input2[i, "steps"] = ifelse(is.na(input2[i, "steps"]), input2[i, "mean.steps"], 
        input2[i, "steps"])
}
input3 <- data.frame(t(apply(as.matrix(input2), 1, function(x) {
    x[2] = ifelse(is.na(x[2]), x[4], x[2])
    return(x)
})), check.names = F)
```


## Are there differences in activity patterns between weekdays and weekends?
