
---
title: "project1_ReproducibleResearch"
output:
  word_document: default
  pdf_document: default
  html_document: default
---
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.


##Loading and preprocessing the data

```{r reading data, echo=T}
data<-read.csv("activity.csv", header=T, sep=",")
data$date<-as.Date(data$date)
weekday<-weekdays(data$date)
data<-cbind(data, weekday)

summary(data)
```

##What is mean total number of steps taken per day?

```{r total steps per day, echo=TRUE}
total_steps <- with(data, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))
names(total_steps)<-cbind("date","steps")

hist(total_steps$steps, xlab="total steps per day", main="total steps per day", col="indianred3")
![plot of chunk total steps per day](figure/total_steps_per_day.png) 
```

What are the mean and the median of total number of steps per day?

```{r mean and median, echo=TRUE}
mean(total_steps$steps)
median(total_steps$steps)
```

##What is the average daily activity pattern?

```{r daily activity pattern, echo=TRUE}
library(ggplot2)

daily_steps <- with(data, aggregate(steps, by = list(interval), FUN = sum, na.rm = TRUE))
names(daily_steps)<-cbind("interval","steps")
ggplot(daily_steps, aes(interval, steps))+geom_line()+xlab("5 minutes intervals")

```

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r interval with maximum number of steps, echo=TRUE}
daily_steps[which(daily_steps$steps==max(daily_steps)),1]
```

##Imputing missing values

Total number of rows with NAs

```{r missing values, echo=T}
sum(is.na(data$steps))
```

Creation of a new dataset, equal to the original dataset but with the missing data filled in. Given the high number of NA's I chose to replace them with mean number of the corresponding day.

```{r new dataset, echo=T}
imputed_steps <- total_steps$steps[match(data$date, total_steps$date)]
newdata<- transform(data, steps = ifelse(is.na(data$steps), yes = imputed_steps, no = data$steps))
newtotal_steps <- with(newdata, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))
names(newtotal_steps)<-cbind("date","steps")

hist(newtotal_steps$steps, xlab="total steps per day", main="total steps per day", col="indianred3")

```

What are the mean and the median of total number of steps per day with this new dataset?

```{r new mean and median, echo=TRUE}
mean(newtotal_steps$steps)
median(newtotal_steps$steps)
```

##Are there differences in activity patterns between weekdays and weekends?

Creation of a new factor variable in the dataset with two levels - "weekday" and "weekend"

```{r processing dataset, echo=T}
daylist<-as.character(newdata$weekday)
day<- ifelse(daylist %in% c("samedi","dimanche"), "weekend", "weekday")
finaldata<-cbind(newdata, day)
summary(finaldata)

```

Plotting of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

```{r weekday vs weekend plot, echo=T}
final_daily_steps <- with(finaldata, aggregate(steps~interval+day, FUN = sum, na.rm = TRUE))
ggplot(final_daily_steps, aes(interval, steps))+facet_grid(.~day)+geom_line()+xlab("5 minutes intervals")+ggtitle("average number of steps, weekdays vs weekends")

```
 
