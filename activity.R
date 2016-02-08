library(reshape2)
library(dplyr)
library(ggplot2)

# Code for reading in the dataset and/or processing the data
activity = read.csv("activity.csv")
names(activity)

# casting the date attribute in the data as Date objects
activity$date <- as.Date(activity$date)

# HISTOGRAM OF TOTAL STEPS ON EACH DAY
# Melt and recast data by date and step on each date
# id=date, measure=steps
activity_date <- melt(activity, id.vars="date", measure.vars="steps", na.rm=FALSE)

# Sum all the steps within wach day to get teh total number of steps on each day
activity_sum_date <- dcast(activity_date, date ~ variable, sum)

# Histogram of the total number of steps taken each day
plot(activity_sum_date$date, activity_sum_date$steps, type="h", main="Histogram of Daily Steps", 
      xlab="Date", ylab="Steps per Day", col="darkorange2", lwd=10)

# Mean and median number of steps taken each day
mean(activity_sum_date$steps, na.rm=TRUE)
median(activity_sum_date$steps, na.rm=TRUE)

# Time series plot of the average number of steps taken

# The 5-minute interval that, on average, contains the maximum number of steps
# Re-melt data frame to prep for casting by interval, including removing NA values so we can take the mean a little later
activity_interval <- melt(activity, id.vars="interval", measure.vars="steps", na.rm=TRUE)

# Cast data frame to see mean steps per interval
activity_mean_interval <- dcast(activity_interval, interval ~ variable, mean)
plot(activity_mean_interval$interval, activity_mean_interval$steps, type="h", 
     main="Frequency of Steps Taken at Each Interval", xlab="Interval", ylab="Steps", col="darkorange2", lwd=5)

# The plot shows the peak at somewhere in the 800-900 interval range so let's find out exactly which interval has the max value and what that maximum value is
# Output interval that has max value along with the max value
activity_mean_interval$interval[which(activity_mean_interval$steps == max(activity_mean_interval$steps))]
max(activity_mean_interval$steps)

#IMPUTED MISSING VALUES
# Code to describe and show a strategy for imputing missing data
sum(is.na(activity$steps))
# activity_ <- activity
activity_merge = merge(activity, activity_mean_interval, by="interval", suffixes=c(".act", ".spi"))

# Get list of indexes where steps value = NA
naIndex = which(is.na(activity$steps))

# Replace NA values with value from steps.spi
activity[naIndex,"steps"] = activity_merge[naIndex,"steps.spi"]

# Histogram of the total number of steps taken each day after missing values are imputed

activity_date_noNA <- melt(activity, id.vars="date", measure.vars="steps", na.rm=FALSE)
activity_sum_noNA <- dcast(activity_date_noNA, date ~ variable, sum)

# Plot histogram with frequency of steps by day
plot(activity_sum_noNA$date, activity_sum_noNA$steps, type="h", main="Histogram of Daily Steps (Imputted NA Values)", 
     xlab="Date", ylab="Steps", col="darkorange2", lwd=8)

mean(activity_sum_noNA$steps, na.rm=TRUE)
median(activity_sum_noNA$steps, na.rm=TRUE)

# ACTIVITY PATTERN DURING WEEKDAYS AND WEEKENDS
# Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
activity <- mutate(activity, weektype = ifelse(weekdays(activity$date) == "Saturday" | weekdays(activity$date) == "Sunday", "weekend", "weekday"))
activity$weektype <- as.factor(activity$weektype)
head(activity)

  
activity_interval <- activity %>% group_by(interval, weektype) %>% summarise(steps = mean(steps))
gp <- ggplot(activity_interval, aes(x=interval, y=steps, color = weektype)) +
  geom_line() + facet_wrap(~weektype, ncol = 1, nrow=2)
print(gp)
