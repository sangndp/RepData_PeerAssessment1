```{r}
library(dplyr)
library(plyr)
library(ggplot2)
library(lattice)

data <- read.csv("activity.csv", header = TRUE) ## loading data

## Q1: WHAT IS MEAN TOTAL NUMBER OF STEPS TAKEN PER DAY?
data_date <- ddply(data, c("date"), summarize, steps = sum(steps))

plot1 <- function(){
  ggplot(data_date, aes(steps)) + geom_histogram(fill = "green", colour = "green") +
    xlab("Steps") + ylab("Frequency") +
    ggtitle("The histogram of the total number of steps taken each day")
}
plot1()

mean_steps <- mean(data_date$steps, na.rm = TRUE)
median_steps <- median(data_date$steps, na.rm = TRUE)

## Q2: WHAT IS THE AVERAGE DAILY ACTIVITY PATTERN?
data_remove_na <- data[complete.cases(data),]
data_interval <- ddply(data_remove_na, c("interval"), summarize, steps = sum(steps) / 61)

plot2 <- function(){
  ggplot(data_interval, aes(interval, steps)) + geom_line(colour = "red") + xlab("5-min Interval") + ylab("Steps") +
    ggtitle("The average number of steps taken by 5-minutes interval")
}
plot2()

data_interval[which.max(data_interval$steps),]
## Q3: IMPUTTING MISSING VALUE
sum(!complete.cases(data))
data_new <- data
data_new$steps[is.na(data_new$steps)] <- mean(data_new$steps, na.rm = TRUE) # na = average steps of interval

data_new_date <- ddply(data_new, c("date"), summarize, steps = sum(steps))
plot3 <- function(){
  ggplot(data_new_date, aes(steps)) + geom_histogram(fill = "red", colour = "red") + xlab("Steps") + ylab("Frequency") +
    ggtitle("The histogram of the total number of steps taken each day")
}
plot3()

mean_new_steps <- mean(data_new_date$steps, na.rm = TRUE)
median_new_steps <- median(data_new_date$steps, na.rm = TRUE)

## Q4: ARE THERE DIFFERENCES IN ACTIVITY PATTERNS BETWEEN WEEKDAYS AND WEEKENDS?
data_new <- data
data_new$steps[is.na(data_new$steps)] <- mean(data_new$steps, na.rm = TRUE)
data_interval <- ddply(data_remove_na, c("interval"), summarize, steps = sum(steps) / 61)
data_new$date <- as.Date(data_new_date$date)

data_new$Weekday <- weekdays(data_new$date)
weekday_list <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
data_new$Weekday <- factor(data_new$Weekday %in% weekday_list, 
                                levels = c(TRUE, FALSE), labels = c("Weekday", "Weekend"))

data_new_interval <- ddply(data_new, c("Weekday","interval"), summarize, steps = sum(steps) / 61)
plot4 <- function(){
  xyplot(steps ~interval | factor(Weekday), data = data_new_interval, type = "l",
         main = "The average number of steps taken by 5-minutes interval",
         xlab = "Number of Steps", ylab = "Interval")
}
plot4()
```
