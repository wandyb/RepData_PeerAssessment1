# Load the libraries first

library(plyr)
library(ggplot2)

# Now load the data 
data <- read.csv("./activity.csv")

## What is mean total number of steps taken per day?

# Calculate the number of steps per day using the ddply function

stepsperday <- ddply (data, .(date), summarize, stepSum = sum (steps, na.rm = TRUE))

### 1. Make a histogram of the total number of steps taken each day
#Plot the data in a histogramm

hist <- ggplot(stepsperday, aes(x = stepSum)) + 
        geom_histogram(fill = "darkgreen", binwidth = 1000) + 
        theme_bw() + 
        labs(title = "Steps taken per Day (NA removed)", 
             x = "Total Number of Steps per Day", y = "Frequency")
plot(hist)

### 2. Calculate and report the mean and median total number of steps taken per day

#Using the summary function
summary(stepsperday)

#Or just calculating the mean...
means <- mean(stepsperday$stepSum)
means
#... and the median, respectively.
medians <- median(stepsperday$stepSum)
medians

## What is the average daily activity pattern?

### 1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

#Calculate average first
average <- aggregate(steps ~ interval, data = data, FUN = mean, na.rm = TRUE)
# Then plot the data
pattern <- ggplot(data = average, aes(x = interval, y = steps)) + 
        geom_line(color = "blue") + 
        theme_bw() +
        labs(title = "Average Daily Activity Pattern (NA removed)", 
             x = "5 minute Interval", y = "Average Number of Steps Taken")
plot(pattern)

## 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

most <- average$interval[which.max(average$steps)]
most

## Imputing missing values

### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
#Calculate the number of missing values first

sumNA <- sum(is.na(data$steps))
sumNA

### 2. Create a new dataset that is equal to the original dataset but with the missing data filled

#Make new "clean" dataset

cleanData <- data
cleanData$steps[is.na(cleanData$steps)] <- tapply(cleanData$steps, cleanData$interval, mean, na.rm = TRUE)
### 3. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

#Plot the data

averageNA <- aggregate(steps ~ date, data = cleanData, FUN = sum)
histNA <- ggplot(averageNA, aes(x = steps)) + 
        geom_histogram(fill = "darkblue", binwidth = 1000) + 
        theme_bw() +
        labs(title = "Steps taken per Day", x = "Steps per Day", y = "Frequency")
plot(histNA)

#Calculate mean
meanNA <- mean(averageNA$steps)
meanNA

#and median
medianNA <- median(averageNA$steps)
medianNA

#The difference is not very big. The mean before removing the NAs was 9354 and removing the NAs shifted the mean to the right to 10766. The median also shifted slightly from 10395 to 10766.

## Are there differences in activity patterns between weekdays and weekends?

#Change date to as.POSIXct and add the weekday and weekend identifier, respectively.
dataNew <- data
dataNew$date <- as.POSIXct(data$date)
dataNew$day <- ifelse(weekdays(dataNew$date) == "Saturday" | weekdays(dataNew$date) == "Sunday" ,"weekend","weekday")
#Make a new dataframe
summaryWeek <- aggregate(steps ~ interval + day, data = dataNew, sum)

#Plot the data
weekly <- ggplot(summaryWeek, aes(x = interval, y = steps)) + 
        geom_line(color = "red") + 
        theme_bw() + 
        labs(title = "Comparison activity weekday and weekend", x = "Daily 5 minute intervals", y ="Number of Steps") 
weeklyplot <- weekly + facet_grid(day ~ .) 
plot(weeklyplot)