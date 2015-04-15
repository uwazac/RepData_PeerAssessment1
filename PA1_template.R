steps_perDayTotal<-summarise(group_by(steps_perDay, date), sum(steps))
bad<-is.na(steps_perDayTotal$"sum(steps)")
good_steps<-steps_perDayTotal$"sum(steps)"[!bad]
mean(good_steps)
median(good_steps)
bad_rows<-lapply(steps_perDayTotal$"sum(steps)", function(x) is.na(x))
bad_rows<-as.logical(bad_rows)
good_steps_perDayTotal<-steps_perDayTotal[!bad_rows,]
