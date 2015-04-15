require("lattice")
require("dplyr")

activity<-read.csv("activity.csv", stringsAsFactors=F)
dates<-as.POSIXlt(strftime(activity$date, format="", usetz=F))
dates<-as.Date(dates, format="%d%m%Y")
activity$date<-dates
month_dates<-months(dates)

steps_perDay<-distinct(activity, date)

steps_perDayMean<-dplyr:::summarise(group_by(activity, date), mean(steps))

steps_perDayMedian<-dplyr:::summarise(group_by(activity, date), median(steps, na.rm=T))

steps_perDayTotal<-dplyr:::summarise(group_by(activity, date), sum(steps))
steps_perDayTotal<-cbind(steps_perDayTotal, month_dates)

print(barchart(`date`~`sum(steps)`, data=steps_perDayTotal, xlab="Total Steps", ylab="Date", scales=list(alternating='1', cex=c(0.9,1)), auto.key=T, horizontal=T, main="Total Steps per Day"))

activity<-dplyr:::mutate(activity, NAs = is.na(steps))
activity_NAs<-dplyr:::filter(activity, NAs==TRUE)%>%summarise(n())
summary(activity_NAs)


