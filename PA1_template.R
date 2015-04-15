library("lattice")
library("dplyr")

activity<-read.csv("activity.csv")
dates<-as.POSIXlt(strftime(activity$date, format="", usetz=F))
dates<-as.Date(dates, format="%d%m%Y")
activity$date<-dates
month_dates<-months(dates)

steps_perDay<-distinct(activity, steps)

steps_perDayTotal<-summarise(group_by(steps_perDay, date), sum(steps))

bad<-is.na(steps_perDayTotal$"sum(steps)")
good_steps<-steps_perDayTotal$"sum(steps)"[!bad]

print(histogram(~`sum(steps)`, data=good_steps_perDayTotal, xlab="Total Steps", scales=list(alternating='2', cex=c(0.9,1)), nint=52, auto.key=T))

mean(good_steps)
median(good_steps)
bad_rows<-lapply(steps_perDayTotal$"sum(steps)", function(x) is.na(x))
bad_rows<-as.logical(bad_rows)
good_steps_perDayTotal<-steps_perDayTotal[!bad_rows,]
