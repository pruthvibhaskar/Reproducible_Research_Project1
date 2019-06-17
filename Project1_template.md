# Reproducible Research Project 1

## Loading and preprocessing the data
unzip(zipfile="activity.zip")
data <- read.csv("activity.csv")

## What is mean total number of steps taken per day?

library(ggplot2)
total.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
mean(total.steps, na.rm=TRUE)
median(total.steps, na.rm=TRUE)

![alt text](https://github.com/pruthvibhaskar/Reproducible_Research_Project1/blob/master/figure/Total_Number_of_steps.png "Steps taken per day")

## What is the average daily activity pattern?

library(ggplot2)
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken")
    
  ![alt text](https://github.com/pruthvibhaskar/Reproducible_Research_Project1/blob/master/figure/5%20minute%20Interval.png "Average Daily Activity pattern")
  
  ### On average across all the days in the dataset, the 5-minute interval contains the maximum number of steps?
  averages[which.max(averages$steps),]
  
  ## Imputing missing values
  missing <- is.na(data$steps)
table(missing)

###  Replace each missing value with the mean value of its 5-minute interval
fill.value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (averages[averages$interval==interval, "steps"])
    return(filled)
}
filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)

### histogram of the total number of steps taken each day and calculate the mean and median total number of steps.
total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
mean(total.steps)
median(total.steps)

![alt text](https://github.com/pruthvibhaskar/Reproducible_Research_Project1/blob/master/figure/Total%20Number%20of%20steps%20(replacing%20mssing%20with%20mean%20value).png "Steps taken after handling Missing Value")

## Are there differences in activity patterns between weekdays and weekends?
weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=weekday.or.weekend)

![alt text](https://github.com/pruthvibhaskar/Reproducible_Research_Project1/blob/master/figure/steps%20taken%20on%20weekdays%20and%20weekends.png "Difference between Weekdays and Weekends")
