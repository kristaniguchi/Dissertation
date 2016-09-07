#Storm Event 2: 3/2015 
#Figure 3 in Events Report for EPA
  #rained from 2015-03-01 to 2015-03-03

#to read in PT data, read in barometric data, subtract to get water depth (sections 1-4)
setwd("F:/TJ/Validation_data/Google_drive")

#1. No barometric data from TJE during this time period
#SD pressure data
san0 = read.csv(file="KSAN-Oct2014-2015-downloaded_3_10.csv", header=TRUE) 
date.time = strptime(san0$X,"%Y-%m-%d %H:%M:%S")
time = format(date.time,"%H:%M:%S")
date = format(date.time,"%Y-%m-%d" )
san = cbind(san0, date.time, date, time)
pressure.sd.m = san$Sea.Level.PressureIn*33.8637526/(0.09806649999980076*1000) #from in Hg to mbar to depth water m

#2. read in the PT_level data
obs0 = read.csv(file = "llc_observed_data_March2015.csv", header=TRUE)
date.time0 = paste(as.character(obs0$date), as.character(obs0$time), sep=" ")
date.time = strptime(date.time0,"%m/%d/%Y %H:%M:%S")
time = format(date.time,"%H:%M:%S")
date = format(date.time,"%Y-%m-%d" )
obs = cbind(obs0, date.time, date, time)
#3. interpolate the barometric data to get the same time stamp as PT_level data
approx.san = data.frame(approx(san$date.time, pressure.sd.m, obs$date.time)) #pressure in mb
pressure.san.m.approx = approx.san$y 

#step 3 using the TJE naval barometric data
#TJE Naval Base Pressure data (time stamp is from GMT to PSD)
tje.naval0 = read.table(file = "atm_pressure_naval_base_TJEstuary.txt", skip=1, header=TRUE, sep=",", colClasses="character")
year = substr(tje.naval0$Date, 1,4) #to format the date extract out the year, month, day and time
month = substr(tje.naval0$Date, 5,6)
day = substr(tje.naval0$Date, 7,8)
hour = substr(tje.naval0$HrMn, 1,2)
#hour = as.character(as.numeric(hour0)-8)
minute = substr(tje.naval0$HrMn, 3,4)
date.time0 = paste(month, "/", day, "/",year, " ", hour, ":", minute, sep = "")
date.time.gmt = strptime(date.time0,"%m/%d/%Y %H:%M",tz="GMT") #orginial data in GMT UTC time zone!
timezone.gmt.ct = as.POSIXct(date.time.gmt)
date.time = strptime(as.character(format(timezone.gmt.ct,"%m/%d/%Y %H:%M", tz="America/Los_Angeles")), "%m/%d/%Y %H:%M")
time = format(date.time,"%H:%M:%S")
date = format(date.time,"%Y-%m-%d" )
tje.naval = cbind(tje.naval0, date.time, date, time)
pressure.tje.naval.m = as.numeric(tje.naval$Slp)/(0.09806649999980076*1000)  #from mb to m
approx.tje.naval = data.frame(approx(tje.naval$date.time, pressure.tje.naval.m, obs$date.time)) #pressure in mb
pressure.tje.naval.m.approx = approx.tje.naval$y 

#Adjustment of Barometric data for 3/2015 Event
#SAN data ajusted based on 2015-03-03 tuesday mean (baseflow day)
date = as.Date(approx.san$x)
approx.san2 = cbind(approx.san, date)
tuesday.san = approx.san2[approx.san2$date == as.Date("2015-03-03"),]
av.san = mean(tuesday.san$y)
#TJE Naval data ajusted based on 2015-03-03 tuesday mean (baseflow day)
date = as.Date(approx.tje.naval$x)
approx.tje.naval2 = cbind(approx.tje.naval, date)
tuesday.tje.naval = approx.tje.naval2[approx.tje.naval2$date == as.Date("2015-03-03"),]
av.tje.naval = mean(tuesday.tje.naval$y) 
#observed mean for Tuesday
date2 = as.Date(obs$date)
obs2 = cbind(obs, date2)
tuesday.obs = obs2[obs2$date == "3/3/2015",]
av.obs = mean(tuesday.obs$PT)
#adjusted data: subtract the diff in the means to the barometric data
diff.mean.san = av.san - av.obs
san.adj = approx.san2$y - diff.mean.san
diff.mean.tje.naval = av.tje.naval - av.obs
tje.naval.adj = approx.tje.naval2$y - diff.mean.tje.naval

#plot the pressure from the pt and other barometric sources just to see
plot(obs$date.time, obs$PT, type="l", col="blue",  xlab = "Date", ylab = "Pressure (m)",xaxt = "n", xlim = c(as.POSIXct(obs$date.time[1], "%m/%d/%Y %H:%M:%S"),as.POSIXct("2015-03-03 00:00:00", "%m/%d/%Y %H:%M:%S"))) 
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")
lines(approx.san$x, pressure.san.m.approx,type="l")
lines(approx.tje.naval$x, pressure.tje.naval.m.approx,type="l", col="green")
lines(approx.san$x, san.adj ,type="l")
lines(approx.tje.naval$x, tje.naval.adj, type = "l", col="green")

#read in precip data for the very top plot in figure 2
precip0 = read.csv(file = "llc_observed_data_March2015_precip.csv", header=TRUE)
date.time = strptime(precip0$Date.time,"%m/%d/%Y %H:%M")
time = format(date.time,"%H:%M:%S")
date = format(date.time,"%Y-%m-%d" )
precip00 = cbind(precip0, date.time, date, time)
precip = na.omit(precip00)

#CACULATE THE ADJUSTED STAGE
#calculate the stage, us for loop to replace all of the negative values with 0
stage.m0 = obs$PT - tje.naval.adj
  stage.m = 0
  for (i in 1:length(stage.m0))
    { if (stage.m0[i] < 0) 
        {stage.m[i] = 0}
      else {stage.m[i] = stage.m0[i]}  }

#plot the stage
plot(approx.tje.naval$x, stage.m, type="l",  xlab = "Date", ylab = "Stage (m)", xaxt = "n",xlim = c(as.POSIXct(obs$date.time[1]), as.POSIXct("2015-03-03 00:00:00 PST")))
axis.POSIXct(side = 1, approx.tje.naval$x, format = "%Y-%m-%d")

#calculate Q
source("F:/TJ/R/TJ/events_report/Rfiles/function_calc_Q.R")
q.cms = calculateQ(stage.m)

#plot Q timeseries 
plot(obs$date.time, q.cms, type ="l", xlab = "Date", ylab = "Discharge (cms)", xaxt = "n",xlim = c(as.POSIXct(obs$date.time[1]),as.POSIXct("2015-03-03 00:00:00 PST")))
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")

#calculate peak q and total q in mm for whole storm 1
peakq.cms = max(q.cms, na.rm = TRUE) 
source("F:/TJ/R/TJ/events_report/Rfiles/function_calc_total_Q_mm.R")
total.q.mm = calculate.total.Q.mm(q.cms)

#FIGRUE 3
#Overall Panel Plots of everything
layout(matrix(1:4, ncol = 1), widths = 1, heights = c(0.05,0.05,0.04,0.05), respect = FALSE)
par(mar = c(0, 4.1, 0, 2.1)) #set margins for bottom, L, top, R
plot(precip$date.time, precip$Cumulative.rain.mm, type="l", xaxt = 'n', ylab="Cum. Rain (mm)", xlim = c(as.POSIXct(obs$date.time[1]),as.POSIXct("2015-03-03 00:00:00 PST"))) 
legend("topleft", "A)", bty="n", cex=1.5, inset=c(-.09,-.15))
legend("topleft", "E1", bty="n", cex=1.5, inset=c(0.08,-.15))
legend("topleft", "E2", bty="n", cex=1.5, inset=c(0.45,-.15))
abline(v=as.POSIXct("2015-03-01 00:00", format="%Y-%m-%d %H:%M"), lty=2)
abline(v=as.POSIXct("2015-03-01 22:19", format="%Y-%m-%d %H:%M"), lty=2)
abline(v=as.POSIXct("2015-03-02 10:09", format="%Y-%m-%d %H:%M"), lty=2)
par(mar = c(0, 4.1, 0, 2.1))
plot(obs$date.time, obs$PT, type="l", col="blue", xaxt = 'n', ylab = "Pressure (m)", ylim=c(10.2,10.65), xlim = c(as.POSIXct(obs$date.time[1]),as.POSIXct("2015-03-03 00:00:00 PST")))
lines(approx.san$x, pressure.san.m.approx,type="l",col="brown")
lines(approx.tje.naval$x, pressure.tje.naval.m.approx,type="l", col="green")
lines(approx.san$x, san.adj ,type="l",col="brown")
lines(approx.tje.naval$x, tje.naval.adj, type = "l", col="green")
legend("topleft", c("SAN","TJE Naval","PT"),  col=c("brown","green","blue"),lwd=1, lty=c(1,1,1),inset=c(.1,-.04), cex=0.75,bty = "n" ) #bty="n" means no box
legend("topleft", "B)", bty="n", cex=1.5, inset=c(-.09,-.15))
abline(v=as.POSIXct("2015-03-01 00:00", format="%Y-%m-%d %H:%M"), lty=2)
abline(v=as.POSIXct("2015-03-01 22:19", format="%Y-%m-%d %H:%M"), lty=2)
abline(v=as.POSIXct("2015-03-02 10:09", format="%Y-%m-%d %H:%M"), lty=2)
par(mar = c(0, 4.1, 0, 2.1))
plot(approx.tje.naval$x, stage.m,  type="l",  xaxt = 'n',   ylab = "Stage (m)",xlim = c(as.POSIXct(obs$date.time[1]),as.POSIXct("2015-03-03 00:00:00 PST")))
legend("topleft", "C)", bty="n", cex=1.5, inset=c(-.09,-.2))
abline(v=as.POSIXct("2015-03-01 00:00", format="%Y-%m-%d %H:%M"), lty=2)
abline(v=as.POSIXct("2015-03-01 22:19", format="%Y-%m-%d %H:%M"), lty=2)
abline(v=as.POSIXct("2015-03-02 10:09", format="%Y-%m-%d %H:%M"), lty=2)
par(mar = c(4, 4.1, 0, 2.1))
plot(obs$date.time, q.cms, type ="l", xlab = "Date",ylab = "Discharge (cms)", xaxt = 'n',xlim = c(as.POSIXct(obs$date.time[1]),as.POSIXct("2015-03-03 00:00:00 PST")))
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")
legend("topleft", "D)", bty="n", cex=1.5, inset=c(-.09,-.4))
abline(v=as.POSIXct("2015-03-01 00:00", format="%Y-%m-%d %H:%M"), lty=2)
abline(v=as.POSIXct("2015-03-01 22:19", format="%Y-%m-%d %H:%M"), lty=2)
abline(v=as.POSIXct("2015-03-02 10:09", format="%Y-%m-%d %H:%M"), lty=2)


#put layout of graphs back to one graph
par(mfrow=c(1,1),xpd=FALSE)
dev.off() #to reset back to one plot

#Splitting the storm into multiple smaller events since AGNPS only takes a daily total rainfall
#daily total precip for 2015-03-01
precip0 = precip[precip$date == "2015-02-28",]
precip_20150228 = precip0$Cumulative.rain.mm[length(precip0$date)] #total precip for 2/28/14 is the last value for that day
#daily total precip for 2015-03-01
precip1 = precip[precip$date == "2015-03-01",]
precip_20150301.cum = precip1$Cumulative.rain.mm[length(precip1$date)] #total precip for 2/28/14 is the last value for that day
precip_20150301 = precip_20150301.cum - precip_20150228
#daily total precip for 2015-03-02
precip2 = precip[precip$date == "2015-03-02",]
precip_20150302.cum = precip2$Cumulative.rain.mm[length(precip2$date)] #cum precip up to 3/1/14
precip_20150302 = precip_20150302.cum - precip_20150301.cum #total precip for 3/1/14
#daily total precip for 2015-03-03
precip3 = precip[precip$date == "2015-03-03",]
precip_20150303.cum = precip3$Cumulative.rain.mm[length(precip3$date)] #cum precip up to 3/2/14
precip_20150303 = precip_20150303.cum - precip_20150302.cum  #total precip for 3/2/14

#Event 1: start 2015-03-01 00:00 to 2015-03-01 22:19
#calc total rainfall, total q, peak q for this new subset
totalp.event1 = precip1$Cumulative.rain.mm[precip1$time == "22:19:00"] - precip_20150228
ind.end1 = grep(as.POSIXct("2015-03-01 22:20:00"),obs$date.time)
event1.q.cms = q.cms[1:ind.end1]
event1.date.time = obs$date.time[1:ind.end1]
peakq.event1 = max(event1.q.cms, na.rm = TRUE) 
total.q.mm.1 = calculate.total.Q.mm(event1.q.cms)
#zoom plot of event to look for oscillations
plot(event1.date.time,event1.q.cms, type="l", main="3/01/15 E1", xlab="Time", ylab="Discharge (cms)")
peak.date.time.ind = which(event1.q.cms==peakq.event1)
peak.date.time = event1.date.time[peak.date.time.ind]
#calculate time to peak
time.2.peak.e1 = peak.date.time - as.POSIXct("2015-03-01 03:43:00") #start rainfall at 3:43am

#Event 2: start 2015-03-01 22:19 to 2015-03-02 10:09
totalp.event2 = precip2$Cumulative.rain.mm[precip2$time == "10:09:00"] - precip1$Cumulative.rain.mm[precip1$time == "22:19:00"]
ind.end2 = grep(as.POSIXct("2015-03-02 10:10"),obs$date.time) #since 00:00:00 time gets dropped it give you all indexes for that day, use first one to get 00:00
event2.q.cms = q.cms[(ind.end1+1):ind.end2[1]]
event2.date.time = obs$date.time[(ind.end1+1):ind.end2[1]]
peakq.event2 = max(event2.q.cms, na.rm = TRUE) 
total.q.mm.2 = calculate.total.Q.mm(event2.q.cms)
#zoom plot for E2
plot(event2.date.time,event2.q.cms, type="l", main="3/01/14 E2", xlab="Time", ylab="Discharge (cms)")
peak.date.time.ind2 = which(event2.q.cms==peakq.event2)
peak.date.time2 = event2.date.time[peak.date.time.ind2]
#calculate time to peak
time.2.peak.e2 = peak.date.time2 - as.POSIXct("2015-03-01 23:03:00") #start of rainfall at 23:03


#last day total: 2015-03-02 10:09 to end 2015-03-03
lastdayp = precip_20150303.cum - precip2$Cumulative.rain.mm[precip2$time == "10:09:00"]

#discharge plot for event1 done by plotting event1.q.cms and event1.date.time
#discharge plot for event2 done by plotting event2.q.cms and event2.date.time


date.time2 = data.frame(obs$date.time)
event1 = rep("E1",times=length(event1.q.cms))
event2 = rep("E2",times=length(event2.q.cms))
noevent = rep("NA",times=(length(q.cms)-length(event1)-length(event2)))
Event = c(event1, event2,noevent)
data = cbind(date.time2, q.cms, Event)

write.csv(data, file="F:/TJ/Validation_data/Google_drive/processed_data/20150301_discharge_timeseries.csv",row.names=F)

#output summary for observed data
date = c("2015-03-01","2015-03-02")
peak.q.obs.cms = c(peakq.event1,peakq.event2)
total.q.obs.mm = c(total.q.mm.1,total.q.mm.2)
obs.summary = cbind(date, peak.q.obs.cms, total.q.obs.mm) #may want to add time to peak column
write.csv(obs.summary, file="F:/TJ/Validation_data/Google_drive/processed_data/20150301_observed_q_summary.csv",row.names=F)


##############################################################################################################################
#For the Main channel outlet timeseries in CONCEPTS Summary 
setwd("C:/Users/Kris/Documents/GitHub/Dissertation/AGNPS/Napo_4_4_16/cc/Main_flow_05182016") 

main = read.table("TimeSeries_01.txt",skip = 9,header = FALSE)
names(main)<- c("DATE", 
                "TIME",
                "DISCHARGE.cms", 
                "VELOCITY.ms",   
                "DEPTH.m"     ,
                "STAGE.m"     ,
                "AREA.m2"    ,
                "TOP WIDTH.m2", 
                "PERIMETER.m" , 
                "RADIUS.m",
                #"CONVEYANCE","F.SLOPE","HEAD","FROUDE","BEDSHEAR","SILTDIS","SANDDIS","GRAVELDIS","TOTALDIS","SILTYLD","SANDYLD","GRAVELYLD","TOTALYLD","CUMBED","THALWEG","CUMLAT","SAFETYL","SAFETYR_APCOHL_APCOHR_POREL_PORER","MATRICL","MATRICR","WBLKL","WBLKR","WWATERL","WWATERR","HYDPRL","HYDPRR","PHREAL","PHREAR","BANKTOPL","BANKTOPR")
                "CONVEYANCE","F.SLOPE","HEAD","FROUDE","BEDSHEAR")

date.time0 = paste(as.character(main$DATE), as.character(main$TIME), sep = " ")
time = strptime(as.character(main$TIME),"%H:%M:%S")
date.time = strptime(date.time0,"%m/%d/%Y %H:%M:%S")
month = as.numeric(format(date.time, "%m"))
month.day = format(date.time, "%m/%d")
year = format(date.time, "%Y")
month.day.year = format(date.time, "%m/%d/%Y")
min.q = min(main$DISCHARGE.cms) #check to see what the minimum value is in concepts timeseries, is it diff than baseflow.sum?
q.adjusted.cms = main$DISCHARGE.cms - 0.133 #0.133 is the baseflow sum to be subtracted out
main2 = cbind(main, date.time, month, month.day, year, month.day.year, time, q.adjusted.cms)

#CONCEPTS hydrograph in comparison with Observed: 03/01/2015 E1
sub = main2[main2$month.day.year=="03/01/2015",]
peakq.concepts1 = max(sub$q.adjusted.cms)
peak.depth = max(sub$DEPTH.m)
peak.concepts.ind = which(sub$q.adjusted.cms==peakq.concepts1)
peak.concepts.date.time1 = sub$date.time[peak.concepts.ind]
time.2.peak.concepts.e1 = peak.concepts.date.time1 - sub$date.time[1] #time to peak concepts

plot(event1.date.time,event1.q.cms, type="l", main="03/01/2015 E1", xlab="Time", ylab="Discharge (cms)", ylim = c(0,peakq.concepts1))
lines(sub$date.time, sub$q.adjusted.cms, type="l", xlab = "Time (hrs)", ylab = "Discharge (cms)", main = "2/28/14 E1", col="blue")
legend("topright", c("Observed","CONCEPTS"),  col=c("black","blue"),lwd=1, cex=0.75,bty = "n" ) #bty="n" means no box

#CONCEPTS hydrograph in comparison with Observed: 03/02/2015 E2
sub2 = main2[main2$month.day.year=="03/02/2015",]
peakq.concepts2 = max(sub2$q.adjusted.cms)
peak.depth = max(sub2$DEPTH.m)
peak.concepts.ind2 = which(sub2$q.adjusted.cms==peakq.concepts2)
peak.concepts.date.time2 = sub2$date.time[peak.concepts.ind2]
time.2.peak.concepts.e2 = peak.concepts.date.time2 - sub2$date.time[1] #time to peak concepts
plot(event2.date.time,event2.q.cms, type="l", main="03/02/2015 E2", xlab="Time", ylab="Discharge (cms)", xlim=c(as.POSIXct(event2.date.time[1]), as.POSIXct(sub2$date.time[length(sub2$date.time)])))
lines(sub2$date.time, sub2$q.adjusted.cms, type="l", xlab = "Time (hrs)", ylab = "Discharge (cms)", main = "2/28/14 E1", col="blue")
legend("topright", c("Observed","CONCEPTS"),  col=c("black","blue"),lwd=1, cex=0.75,bty = "n" ) #bty="n" means no box


