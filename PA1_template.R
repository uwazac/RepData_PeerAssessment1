rm(list=ls())
library("lattice", warn.conflicts = F, quietly = F)
library("dplyr", warn.conflicts = F, quietly = F)

activity<-read.csv("activity.csv")
dates<-as.POSIXlt(strftime(activity$date, format="", usetz=F))
dates<-as.Date(dates, format="%d%m%Y")
activity$date<-dates
month_dates<-months(dates)



steps_perDayTotal<-summarise(group_by(activity, date), sum(steps))
total_steps<-sum(steps_perDayTotal$"sum(steps)", na.rm=T)
total_days<-dim(steps_perDayTotal)[[1]]

cat("The total number of taken per day is: ", total_steps/total_days, "\n\n")

print(histogram(~`sum(steps)`, data=steps_perDayTotal, xlab="Total Steps", type="percent", scales=list(alternating='2', cex=c(0.9,1)), nint=26, auto.key=T, main="Frequency Distribution of Total Steps per Day"))

steps_means<-mean(steps_perDayTotal$"sum(steps)", na.rm=T)
cat("The mean number of steps is: ", steps_means, "\n\n")

steps_median<-median(steps_perDayTotal$"sum(steps)", na.rm=T)
cat("The median number of steps is: ", steps_median, "\n\n")

bad<-is.na(steps_perDayTotal$"sum(steps)")
good_steps<-steps_perDayTotal$"sum(steps)"[!bad]

activity_months<-months(activity$date)
print(lattice:::xyplot(steps~interval|activity_months, data=activity, groups=activity_months, type="l", auto.key=T))

#Find the maximum value of for the number of steps taken
activity_summary<-summary(activity)

maxSteps<-range(activity$steps, na.rm=T)[[2]]

maxInterval<-dplyr:::filter(activity, steps==maxSteps)
maxInterval_month<-months(maxInterval$date)
maxInterval<-maxInterval$interval
cat("The maximum number of steps occured in at interval: ", maxInterval,"\n\n")
cat("During the month of: ",maxInterval_month, "\n\n")

activity_NAs<-dplyr:::mutate(activity, NAs = is.na(steps))
activity_NAs_count<-dplyr:::filter(activity_NAs, NAs==TRUE)%>%summarise(n())

cat("The total number of missing values is: ", activity_NAs_count$"n()", "\n\n")

steps_perDayMean<-summarise(group_by(activity, date), mean(steps,  na.rm=T))
steps_perDayMedian<-summarise(group_by(activity, date), median(steps,  na.rm=T))

# Procedure to subsitute means for NAs values in the activities table
activities_filled<-data.frame()
colnames(steps_perDayMean)[2]<-"mean"
for(i in 1:dim(steps_perDayMean)[[1]])
{
z=i
act_date_match<-dplyr:::filter(activity, date==steps_perDayMean$date[i])
for(i in 1:dim(act_date_match)[[1]])
{
if(is.na(act_date_match[i,1]))
{
act_date_match[i,1]<-steps_means/dim(act_date_match)[[1]]
}
}
activities_filled<-rbind(activities_filled, act_date_match)
}


steps_perDayTotal_filled<-summarise(group_by(activities_filled, date), sum(steps))
total_steps_filled<-sum(steps_perDayTotal_filled$"sum(steps)", na.rm=T)
total_days_filled<-dim(steps_perDayTotal_filled)[[1]]

print(histogram(~`sum(steps)`, data=steps_perDayTotal_filled, xlab="Total Steps", type="percent", scales=list(alternating='2', cex=c(0.9,1)), nint=26, auto.key=T, main="Frequency Distribution of Total Steps per Day"))


cat("The total number of steps taken per day after filling is: ", total_steps_filled/total_days_filled, "\n\n")

steps_means_filled<-mean(steps_perDayTotal_filled$"sum(steps)", na.rm=T)
cat("The mean number of steps after filling is: ", steps_means_filled, "\n\n")

steps_median_filled<-median(steps_perDayTotal_filled$"sum(steps)", na.rm=T)
cat("The median number of steps after filling is: ", steps_median_filled, "\n\n")


weekday<-weekdays(dates)

activities_filled<-dplyr:::mutate(activities_filled, day_class=weekday)

activities_filled$day_class<-gsub("Monday", "Weekday",activities_filled$day_class )
activities_filled$day_class<-gsub("Tuesday", "Weekday",activities_filled$day_class )
activities_filled$day_class<-gsub("Wednesday", "Weekday",activities_filled$day_class )
activities_filled$day_class<-gsub("Thursday", "Weekday",activities_filled$day_class )
activities_filled$day_class<-gsub("Friday", "Weekday",activities_filled$day_class )
activities_filled$day_class<-gsub("Saturday", "Weekend",activities_filled$day_class )
activities_filled$day_class<-gsub("Sunday", "Weekend",activities_filled$day_class )


print(lattice:::xyplot(steps~interval|day_class, data=activities_filled, groups=day_class, type="l", auto.key=T))



