library(tidyr)
library(dplyr)
library(ggplot2)

## Load and preprocess the data

activity <- read.csv("activity.csv")
str(activity)
activity$date <- as.Date(activity$date)
str(activity)

## What is mean total number of steps taken per day?

steps_per_day <- activity %>% group_by(date) %>% summarize(sum(steps,rm.na=TRUE))
steps_per_day <- rename(steps_per_day,steps_sum = `sum(steps, rm.na = TRUE)`)
ggplot(steps_per_day,aes(x=steps_sum)) + geom_histogram() + 
  labs(title='Histogram of Total Steps Per Day',x='Steps',y='Frequency Count')

print(paste('Median total steps per day:',median(steps, 
  na.rm = TRUE)))
print(paste('Mean total steps per day:',as.integer(mean(steps, 
  na.rm = TRUE))))

## What is the average daily activity pattern?

steps_per_interval <- activity %>% group_by(interval) %>% summarize(mean(steps,
  na.rm = TRUE))
steps_per_interval <- rename(steps_per_interval,
                             steps_mean = `mean(steps, na.rm = TRUE)`)
ggplot(steps_per_interval,aes(x=interval,y=steps_mean)) + geom_line() + 
  labs(title='Average Daiy Activity',x='Interval',y='Step Mean')
max_interval <- steps_per_interval[steps_per_interval$steps_mean == 
                                     max(steps_per_interval$steps_mean),]$interval
print(paste('Interval with most steps on average:',max_interval))

## Imputing missing values

summary(activity)
print(paste("Total # of rows with missing NAs, which only occurs with 'steps': ",
  sum(!complete.cases(activity$steps))))
activity_cleansed <- activity %>% group_by(interval) %>% mutate(steps = 
  replace_na(steps, mean(steps, na.rm = TRUE)))
steps_per_day_cleansed <- activity_cleansed %>% group_by(date) %>% 
  summarize(sum(steps))
steps_per_day_cleansed <- rename(steps_per_day_cleansed,steps_sum = `sum(steps)`)

ggplot(steps_per_day_cleansed,aes(x=steps_sum)) + geom_histogram() + 
  labs(title='Histogram of Total Steps Per Day, Cleansed',x='Steps',
       y='Frequency Count')

print(paste('Median total steps per day:',
  as.integer(median(steps_per_day_cleansed$steps_sum, na.rm = TRUE))))
print(paste('Mean total steps per day:',
  as.integer(mean(steps_per_day_cleansed$steps_sum, na.rm = TRUE))))

## Are there differences in activity patterns between weekdays and weekends?

wd_or_we <- function(x) {
  day_name<-weekdays(x)
  if(day_name=='Saturday' | day_name=='Sunday'){
    'weekend'
  } else {
    'weekday'
  }
}
activity_cleansed$day_type <- sapply(activity_cleansed$date,wd_or_we)
head(activity_cleansed)

activity_cleansed_weekday <- activity_cleansed[
  activity_cleansed$day_type == 'weekday',]
activity_cleansed_weekend <- activity_cleansed[
  activity_cleansed$day_type == 'weekend',]

steps_per_interval_weekday <- activity_cleansed_weekday %>% 
  group_by(interval) %>% summarize(mean(steps, na.rm = TRUE))
steps_per_interval_weekday <- rename(steps_per_interval_weekday,
                             steps = `mean(steps, na.rm = TRUE)`)

steps_per_interval_weekend <- activity_cleansed_weekend %>% 
  group_by(interval) %>% summarize(mean(steps, na.rm = TRUE))
steps_per_interval_weekend <- rename(steps_per_interval_weekend,
                                     steps = `mean(steps, na.rm = TRUE)`)

par(mfrow=c(1,2))
attach(steps_per_interval_weekday)
plot(interval,steps, 
     main="Avg Steps Per Interval, Weekdays",type='l')
attach(steps_per_interval_weekend)
plot(interval,steps, 
     main="Avg Steps Per Interval, Weekends",type='l')
