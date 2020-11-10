---
title: "Reproducible Research Project 1"
author: "Carl Naces"
date: "11/11/2020"
output: html_document
---

We start by calling libraries that we are going to need.
```{r echo = TRUE, results= 'hide', message=FALSE}
library(ggplot2)
library(dplyr)
```

Create and set the working directory
```{r}
if(!file.exists("./data")){
      dir.create("./data")
}
setwd("./data")
```

Download the zipfile
```{r}
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = "qsdata.zip")
```

The unzipping function was not working properly for me so I manually extracted activity.csv

Loading the file
```{r}
data <- read.csv("activity.csv")
```

Using str on data.

```{r}
str(data)
```

There are three columns: steps, date and interval
We see that there are a lot of missing data and so we remove these data.
Removing the NAs from the data
```{r}
data1 <- data[complete.cases(data),]
```

Rechecking for NAs in data.
```{r}
sum(is.na(data1))
```

We'll be using variable data1 from now on. Now it's time to answer the questions.

**What is mean total number of steps taken per day?**  
1. Calculate the total number of steps taken per day.

Since we know that the columns of data1 are steps, date and interval.
To calculate the total number of steps taken per day, we need to group the data1 
by date and count the sum of steps per date.

We can do this using group by + summarize in dplyr or aggregage.
```{r}
stepsperday <- aggregate(steps ~ date, data1, sum)
dim(stepsperday)
```

The total number of steps taken per day are now stored in the variable stepsperday.
It has 53 days in it. The first 5 values are shown below.
```{r}
head(stepsperday, 5)
```

2. Make a histogram of the total number of steps taken each day.
3. Calculate and report the mean and median of the total number of steps taken per day.

Before making a plot, let's start by calculating the mean and median.
```{r}
stepsmean <- mean(stepsperday$steps)
stepsmedian <- median(stepsperday$steps)
stepsmean
stepsmedian
```

We now plot a histogram of the total steps taken each day and add a vertical line
for the mean and median.

```{r}
ggplot(stepsperday, aes(x=steps)) +
      geom_histogram(color = "black", fill= "white", binwidth = 1000) +
      geom_vline(xintercept = stepsmean, color = "red") +
      geom_vline(xintercept = stepsmedian, color = "blue", linetype = "dashed") +
      xlab("Daily total steps") +
      ylab("Frequency") +
      ggtitle("Histogram of the Total Number of Steps Taken Each Day")
```
![](https://github.com/coralnaces/RepData_PeerAssessment1/blob/master/plot1.png)
**What is the average daily activity pattern?**

1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

To calculate the average number of steps taken per interval, we need to group the data1 
by interval and count the sum of steps.

```{r}
average <- aggregate(steps ~ interval, data1, mean)
```

We now make a time series plot of the 5-minute intervals.
```{r}
ggplot(average, aes(x = interval, y = steps)) +
      geom_line(color = "red") +
      xlab("5 minute intervals") +
      ylab("Average steps") +
      ggtitle("Average daily activity")
```
![](https://github.com/coralnaces/RepData_PeerAssessment1/blob/master/plot2.png)

2. Which 5-minute interval, on average across all the days in the dataset, contains
the maximum number of steps?

Since the variable average is already grouped by interval, all we need to do is 
find out which of it's rows holds the maximum value for the column *steps*.
```{r}
maxsteps <- average[which.max(average$steps),]
maxsteps
```


**Imputing Values**

We go back to *data* the original variable where we stored our data.
This data contains missing values for the *steps* column.

1. Calculate and report the total number of missing values in the dataset.
We find these rows and count the total number of missing values.

```{r}
missing <- is.na(data$steps)
missingsteps <- sum(missing)
missingsteps
```
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
To impute the data. We make use of the impute function from the Hmisc package.
We'll be imputing the missing *steps* data with the mean using the impute function.

```{r}
library(Hmisc)
newdata <- data
newdata$steps <- impute(data$steps, mean)
```

Checking if there are still missing data.
```{r}
sum(is.na(newdata$steps))
```

Now that we have successfully imputed our data we can now proceed.

4. Make a histogram of the total number of steps taken each day and calculate and
report the mean and median total number of steps taken per day. Do these values 
differ from the estimates from the first part of the assignment? What is the impact 
of imputing missing data on the estimates of the total daily number of steps?

Making a histogram of the total number of steps taken each day with the new data.

```{r}
newstepsperday <- aggregate(steps ~ date, newdata, sum)
head(newstepsperday,5)
```

Computing for the mean and median of this new imputed data.

```{r}

stepsmean1 <- mean(newstepsperday$steps)
stepsmedian1 <- median(newstepsperday$steps)
stepsmean1
stepsmedian1
```

Let's make a histogram of the total number of steps taken again now.

```{r}
ggplot(newstepsperday, aes(x=steps)) +
      geom_histogram(color = "black", fill= "white", binwidth = 1000) +
      geom_vline(xintercept = stepsmean1, color = "red") +
      geom_vline(xintercept = stepsmedian1, color = "blue", linetype = "dashed") +
      xlab("Daily total steps") +
      ylab("Frequency") +
      ggtitle("New Histogram of the Total Number of Steps Taken Each Day")
```
![](https://github.com/coralnaces/RepData_PeerAssessment1/blob/master/plot3.png)

The values of the mean and median are still the same with before imputing the data.

**Are there differences in activity patterns between weekdays and weekends?**
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r}
newdata$daytype <- ifelse(as.POSIXlt(newstepsperday$date)$wday %in% c(0,6),
                                 "Weekend", "Weekday")
head(newdata, 5)
```

Now the newdata has a new column *daytype* which is a factor that tells wheter
the date is a weekend or weekday.

2. Make a panel plot containing a time series plot (i.e.type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

First we compute for the averages on the new data and group it by daytype.
```{r}
meannewdata <- aggregate(steps ~ interval + daytype, newdata, mean)
```
Now we make the plot.

```{r}
ggplot(meannewdata, aes(x = interval, y = steps)) +
      geom_line(color = "red") +
      facet_grid(daytype ~ .) +
      xlab("5-minute interval") +
      ylab("Average steps") +
      ggtitle("Average steps in 5-minute interval")
```

![](https://github.com/coralnaces/RepData_PeerAssessment1/blob/master/plot4.png)






