

```r
### Peer-graded Assignment: Reproducible Research Course Project 1
### Objective: Collect, work with, and analyze the given dataset
### Astudent44
  
  
## Loading and pre-processing the data
#### Load in libraries
library(ggplot2)
library(plyr)
library(lubridate)

#### Assume that raw data file is in the working directory
dataset <- read.csv("activity.csv")

#### Clean data by formatting date and removing na
dataset$date <- ymd(dataset$date)
cleandata <- dataset[!is.na(dataset$steps),]
  


## What is mean total number of steps taken per day?
#### Create a summary table of dates and steps
stepsPerDay <- aggregate(cleandata$steps ~ cleandata$date, FUN = sum,)
colnames(stepsPerDay) <- c("Date", "Steps")

#### Graph steps per day, total steps by frequency
png("plot1.png", height = 480, width = 480)
hist(as.numeric(stepsPerDay$Steps), 
     main = "Total Number of Steps Taken Each Day", 
     xlab = "Steps", col = "Blue")
dev.off()
```

```
## png 
##   2
```

```r
#### Calculate mean and median steps taken per day
print(paste("Mean Number of Steps Taken Each Day = ",
            mean(stepsPerDay$Steps)))
```

```
## [1] "Mean Number of Steps Taken Each Day =  10766.1886792453"
```

```r
print(paste("Median Number of Steps Taken Each Day = ",
            median(stepsPerDay$Steps)))
```

```
## [1] "Median Number of Steps Taken Each Day =  10765"
```

```r
## What is the average daily activity pattern?
#### Make a time series plot of the 5-minute interval and the average steps
avgDailyActivity <- aggregate(cleandata$steps, 
                              by = list(cleandata$interval), FUN = mean)
colnames(avgDailyActivity) <- c("Interval", "Mean")

#### Plot average daily activity as a line graph
png("plot2.png", height = 480, width = 640)
plot(avgDailyActivity$Interval, avgDailyActivity$Mean, type = "l", 
     col = "Blue", lwd = 2, xlab = "Interval (by 5-minute)", 
     ylab = "Average Number of Steps Taken Averaged Across All Days",
     main = "Average Daily Activity Pattern")
dev.off()
```

```
## png 
##   2
```

```r
#### Determine which interval has the greatest number of steps
intervalSteps <- aggregate(cleandata$steps ~ cleandata$interval, FUN = sum,)
colnames(intervalSteps) <- c("Interval", "Steps")
print(paste("The 5-minute with the Maximum Number of Steps = ",
            intervalSteps[which.max(intervalSteps$Steps),]$Interval))
```

```
## [1] "The 5-minute with the Maximum Number of Steps =  835"
```

```r
## Imputing missing values
#### Calculate the number of NA values 
print(paste("The Total Number of Missing Values in the Dataset = ",
            sum(is.na(dataset$steps))))
```

```
## [1] "The Total Number of Missing Values in the Dataset =  2304"
```

```r
#### Create new data set with NA values filled in
#### Strategy used is to fill in with mean for that day
avgSteps <- aggregate(dataset$steps ~ dataset$date, FUN = mean,)
colnames(avgSteps) <- c("Date", "Steps")
filleddata <- transform(dataset, steps = ifelse(is.na(dataset$steps), 
                                                yes = avgSteps$Steps,
                                                no = dataset$steps))

#### Redo part 1 with the new data set
filledstepsPerDay <- aggregate(filleddata$steps ~ filleddata$date, FUN = sum,)
colnames(filledstepsPerDay) <- c("Date", "Steps")

#### Graph steps per day, total steps by frequency
png("plot3.png", height = 480, width = 480)
hist(as.numeric(filledstepsPerDay$Steps), 
     main = "Filled Total Number of Steps Taken Each Day", 
     xlab = "Steps", col = "Blue")
dev.off()
```

```
## png 
##   2
```

```r
#### Calculate mean and median steps taken per day
print(paste("Filled Mean Number of Steps Taken Each Day = ",
            mean(filledstepsPerDay$Steps)))
```

```
## [1] "Filled Mean Number of Steps Taken Each Day =  10768.1680327869"
```

```r
print(paste("Filled Median Number of Steps Taken Each Day = ",
            median(filledstepsPerDay$Steps)))
```

```
## [1] "Filled Median Number of Steps Taken Each Day =  10789.1840277778"
```

```r
print(paste("These values DO differ from those of part 1.  By replacing",
            " the NA values with the average number of steps taken",
            " per day the mean and median increase because the average",
            " is greater than the lowest number of steps per day."))
```

```
## [1] "These values DO differ from those of part 1.  By replacing  the NA values with the average number of steps taken  per day the mean and median increase because the average  is greater than the lowest number of steps per day."
```

```r
## Are there differences in activity patterns between weekdays and weekends?
#### Add new variable column to the filled data set
dayName <- weekdays(filleddata$date)
filledweekdata <- cbind(filleddata, dayName)
filledweekdata$dateType <- ifelse(filledweekdata$dayName %in% c("Saturday", "Sunday"),
                                  "Weekend", "Weekday")

#### Plot average daily activity as a line graph for dateType
avgDayDailyActivity <- aggregate(steps~interval + dateType, 
                                 filledweekdata, FUN = mean)
png("plot4.png", height = 480, width = 640)
plot<- ggplot(avgDayDailyActivity, aes(x = interval , y = steps, color = dateType)) +
  geom_line() +
  labs(title = "Average Daily Activity Pattern by Date Type", 
       x = "Interval (by 5-minute)", 
       y = "Average Number of Steps") +
  facet_wrap(~dateType, ncol = 1, nrow=2)
print(plot)
dev.off()
```

```
## png 
##   2
```

