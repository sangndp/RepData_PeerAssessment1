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
