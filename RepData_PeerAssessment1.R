library("lattice")
library("dplyr")

activity<-read.csv("activity.csv")
steps_perDay<-distinct(activity, steps)

steps_perDay$steps<-as.numeric(steps_perDay$steps)

steps_perDayMean<-summarise(group_by(steps_perDay, date), mean(steps))
dates<-as.POSIXlt(strftime(steps_perDayMean$date, format="", usetz=F))
month_dates<-months(dates)

steps_perDayMean<-cbind(steps_perDayMean, month_dates)
steps_perDayMean$date<-as.integer(steps_perDayMean$date)
steps_perDayMean$date<-as.factor(steps_perDayMean$date)

barchart(`mean(steps)`~`date`, data=steps_perDayMean, groups=month_dates, xlab="Days", scales=list(alternating='1', cex=c(0.65,1)),key = list(lines = Rows(trellis.par.get("superpose.line"), c(1:2)), text = list(lab = as.character(unique(month_dates))),columns = 2, title = "Months"))

steps_perDayMedian<-summarise(group_by(steps_perDay, date), median(steps))
steps_perDayMedian<-cbind(steps_perDayMedian, month_dates)
steps_perDayMedian$date<-as.integer(steps_perDayMedian$date)
steps_perDayMedian$date<-as.factor(steps_perDayMedian$date)

barchart(`median(steps)`~`date`, data=steps_perDayMedian, groups=month_dates, xlab="Days", scales=list(alternating='1', cex=c(0.65,1)),key = list(lines = Rows(trellis.par.get("superpose.line"), c(1:2)), text = list(lab = as.character(unique(month_dates))),columns = 2, title = "Months"))
