+
 +========================================================
 +## Loading the data
 +
 +```r
 +setwd("~/Analyse de data exploratoire/travail_3")
 +activity <- read.csv("activity.csv")
 +```
 +
 +
 +## What is the mean total number of steps taken per day?
 +
 +1 Make a histogram of the total number of steps taken each day
 +
 +```r
 +steps_sum <- aggregate(steps ~ date, data = activity, FUN = sum)
 +barplot(steps_sum$steps, names.arg = steps_sum$date, main = "Total number of steps by date", 
 +    xlab = "date", ylab = "Total steps")
 +```
 +
 +![plot of chunk Barplot of total number of steps](figure/Barplot_of_total_number_of_steps.png) 
 +
 +
 +2 Calculate and report the mean and median total number of steps taken per day
 +
 +```r
 +mean_steps <- mean(steps_sum$steps)
 +print(mean_steps)
 +```
 +
 +```
 +## [1] 10766
 +```
 +
 +```r
 +median_steps <- median(steps_sum$steps)
 +print(median_steps)
 +```
 +
 +```
 +## [1] 10765
 +```
 +
 +The mean is 1.0766 &times; 10<sup>4</sup> and the median is 10765.
 +
 +## What is the average daily activity pattern?
 +
 +1 Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
 +
 +```r
 +steps_interval <- aggregate(steps ~ interval, data = activity, FUN = mean)
 +plot(steps_interval, type = "l", main = "Average number of steps by 5-minute interval", 
 +    xlab = "5-minute interval", ylab = "averaged steps across all days")
 +```
 +
 +![plot of chunk Plot of steps_interval](figure/Plot_of_steps_interval.png) 
 +
 +
 +2 Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
 +
 +```r
 +max_interval <- steps_interval$interval[which.max(steps_interval$steps)]
 +```
 +
 +The result is the interval number 835.
 +
 +## Imputing missing values
 +
 +1 Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
 +
 +```r
 +sum(is.na(activity))
 +```
 +
 +```
 +## [1] 2304
 +```
 +
 +
 +2 Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
 +
 +I'll use the mean for filling the missing values in the dataset.
 +
 +3 Create a new dataset that is equal to the original dataset but with the missing data filled in.
 +
 +```r
 +activity <- merge(activity, steps_interval, by = "interval", suffixes = c("", 
 +    ".mean"))
 +nas <- is.na(activity$steps)
 +activity$steps[nas] <- activity$steps.mean[nas]
 +activity <- activity[, c(1:3)]
 +```
 +
 +I check if the activity dataset has no more "NA values""
 +
 +```r
 +sum(is.na(activity))
 +```
 +
 +```
 +## [1] 0
 +```
 +
 +4 Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
 +
 +```r
 +steps_noNA <- aggregate(steps ~ date, data = activity, FUN = sum)
 +barplot(steps_noNA$steps, names.arg = steps_noNA$date, main = "Total number of steps by date", 
 +    xlab = "date", ylab = "Total steps")
 +```
 +
 +![plot of chunk Barplot_total number of steps_noNA](figure/Barplot_total_number_of_steps_noNA.png) 
 +
 +```r
 +mean_noNA <- mean(steps_noNA$steps)
 +print(mean_noNA)
 +```
 +
 +```
 +## [1] 10766
 +```
 +
 +```r
 +median_noNA <- median(steps_noNA$steps)
 +print(median_noNA)
 +```
 +
 +```
 +## [1] 10766
 +```
 +
 +If we compare the two means 1.0766 &times; 10<sup>4</sup> with 1.0766 &times; 10<sup>4</sup> and the two medians 1.0766 &times; 10<sup>4</sup> with 10765 we can see that the impact of imputting missing data is null.
 +
 +## Are there differences in activity patterns between weekdays and weekends?
 +
 +1 Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day
 +
 +```r
 +activity$date <- as.character(activity$date)
 +activity$date <- as.Date(activity$date)
 +activity$date.bin <- factor(ifelse(weekdays(activity$date) %in% c("samedi", 
 +    "dimanche"), "weekend", "weekday"))
 +table(activity$date.bin)
 +```
 +
 +```
 +## 
 +## weekday weekend 
 +##   12960    4608
 +```
 +
 +
 +2 Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:
 +
 +
 +```r
 +activity_WE <- subset(activity, activity$date.bin == "weekend")
 +activity_WD <- subset(activity, activity$date.bin == "weekday")
 +par(mfrow = c(2, 1))
 +steps_interval_WE <- aggregate(steps ~ interval, data = activity_WE, FUN = mean)
 +plot(steps_interval, type = "l", main = "weekend", xlab = "5-minute interval", 
 +    ylab = "averaged steps")
 +steps_interval_WD <- aggregate(steps ~ interval, data = activity_WD, FUN = mean)
 +plot(steps_interval, type = "l", main = "weekday", xlab = "5-minute interval", 
 +    ylab = "averaged steps")
 +```
 +
 +![plot of chunk Plot weekend and weekday](figure/Plot_weekend_and_weekday.png) 
 +
 +