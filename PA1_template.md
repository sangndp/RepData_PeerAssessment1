# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

```r
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}
data <- read.csv("activity.csv", header = TRUE)
```

## What is mean total number of steps taken per day?
```{r}
data_date <- ddply(data, c("date"), summarize, steps = sum(steps))
plot1 <- function(){
  ggplot(data_date, aes(steps)) + geom_histogram(fill = "green", colour = "green") +
    xlab("Steps") + ylab("Frequency") +
    ggtitle("The histogram of the total number of steps taken each day")
}
plot1()
```
![plot of chunk unnamed-chunk-1](figures/plot1_assign1.png)

The mean and median of the total number of steps taken per day:
```{r}
mean_steps <- mean(data_date$steps, na.rm = TRUE)
median_steps <- median(data_date$steps, na.rm = TRUE)
```

```{r}
mean_steps
```

```
## [1] 10766.19
```
```{r}
median_steps
```

```
## [1] 10765
```

## What is the average daily activity pattern?
```{r}
data_remove_na <- data[complete.cases(data),]
data_interval <- ddply(data_remove_na, c("interval"), summarize, steps = sum(steps) / 61)

plot2 <- function(){
  ggplot(data_interval, aes(interval, steps)) + geom_line(colour = "red") + xlab("5-min Interval") + ylab("Steps") +
    ggtitle("The average number of steps taken by 5-minutes interval")
}
plot2()
```
![plot of chunk unnamed-chunk-2](figures/plot2_assign2.png)

On average across all the days in the dataset, the 5-minute interval contains
the maximum number of steps?
```{r}
data_interval[which.max(data_interval$steps),]
```
## Imputing missing values
The number of missing values
```{r}
sum(!complete.cases(data))
```

```{r}
data_new <- data
data_new$steps[is.na(data_new$steps)] <- mean(data_new$steps, na.rm = TRUE) # na = average steps of interval
data_new_date <- ddply(data_new, c("date"), summarize, steps = sum(steps))
plot3 <- function(){
  ggplot(data_new_date, aes(steps)) + geom_histogram(fill = "red", colour = "red") + xlab("Steps") + ylab("Frequency") +
    ggtitle("The histogram of the total number of steps taken each day")
}
plot3()
```
![plot of chunk unnamed-chunk-3](figures/plot3_assign3.png) 
```{r}
mean_new_steps <- mean(data_new_date$steps, na.rm = TRUE)
median_new_steps <- median(data_new_date$steps, na.rm = TRUE)
```