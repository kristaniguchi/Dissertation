#Storm Event 5: 1/2016
#Figure 6 in Events Report for EPA

#to read in PT data, read in barometric data, subtract to get water depth (sections 1-4)
setwd("F:/TJ/Validation_data/Google_drive")

#1. read in barometric data from TJE
tje0 = read.csv(file = "TJE_atm_pressure.csv", skip=2, header=TRUE) #sheet from TJE for barometric data
date.time = strptime(tje0$Date,"%m/%d/%Y %H:%M")
time = format(date.time,"%H:%M:%S")
date = format(date.time,"%Y-%m-%d" )
tje = cbind(tje0, date.time, date, time)
#2. read in the PT_level data
obs0 = read.csv(file = "llc_observed_data_Jan05_2016.csv", header=TRUE)
date.time0 = paste(as.character(obs0$date), as.character(obs0$time), sep=" ")
date.time = strptime(date.time0,"%m/%d/%Y %H:%M:%S")
time = format(date.time,"%H:%M:%S")
date = format(date.time,"%Y-%m-%d" )
obs = cbind(obs0, date.time, date, time)
#3. interpolate the barometric data to get the same time stamp as PT_level data
approx.tje = data.frame(approx(tje$date.time, tje$Data..mb., obs$date.time)) #pressure in mb
pressure.tje.m.approx = approx.tje$y /(0.09806649999980076*1000)  #from mb to m of water column

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

#Adjustment of Barometric data for 01/2016 Event
#tje data ajusted based on 01/04/2016 at 10:00 am mean 
date = as.Date(approx.tje$x)
approx.tje2 = cbind(approx.tje, date)
ind = grep(as.POSIXct("2016-01-04 10:00:00 PST"),approx.tje2$x)
friday.tje = approx.tje2[1:ind,]
av.tje = mean(friday.tje$y/(0.09806649999980076*1000) ) #from mb to m
#TJE Naval data ajusted based on 01/04/2016 at 10:00 am mean
date = as.Date(approx.tje.naval$x)
approx.tje.naval2 = cbind(approx.tje.naval, date)
ind = grep(as.POSIXct("2016-01-04 10:00:00 PST"),approx.tje.naval2$x)
friday.tje.naval = approx.tje.naval2[1:ind,]
av.tje.naval = mean(friday.tje.naval$y) 
#observed mean for Friday til noon
ind = grep(as.POSIXct("2016-01-04 10:00:00 PST"),obs$date.time)
friday.obs = obs[1:ind,]
av.obs = mean(friday.obs$PT)
#adjusted data: subtract the diff in the means to the barometric data
diff.mean.tje = av.tje - av.obs
tje.adj = approx.tje2$y/(0.09806649999980076*1000) - diff.mean.tje
diff.mean.tje.naval = av.tje.naval - av.obs
tje.naval.adj = approx.tje.naval2$y - diff.mean.tje.naval
#plot the pressure from the pt and other barometric sources
plot(obs$date.time, obs$PT, type="l", col="blue",  xlab = "Date", ylab = "Pressure (m)")# , ylim=c(10.2,10.45),xaxt = "n")
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")
lines(approx.tje$x, pressure.tje.m.approx, col="red", type="l",  xlab = "Date", ylab = "Barometric Pressure (m)")
lines(approx.tje.naval$x, pressure.tje.naval.m.approx,type="l", col="green")
lines(approx.tje$x, tje.adj ,type="l", col="red") #adjusted tje
lines(approx.tje.naval$x, tje.naval.adj, type = "l", col="green") #adjusted tje.naval
#also tried adjusting based on Sunday, but did not work well, see PT_script_atmpressure_waterlevel.R to see sunday

#read in precip data for the very top plot in figure 2
precip0 = read.csv(file = "llc_observed_data_Jan05_2016_precip.csv", header=TRUE)
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
plot(approx.tje.naval$x, stage.m, type="l",  xlab = "Date", ylab = "Stage (m)", xaxt = "n")
axis.POSIXct(side = 1, approx.tje.naval$x, format = "%Y-%m-%d")

#calculate Q
source("F:/TJ/R/TJ/events_report/Rfiles/function_calc_Q.R")
q.cms = calculateQ(stage.m)

#plot Q timeseries 
plot(obs$date.time, q.cms, type ="l", xlab = "Date", ylab = "Discharge (cms)", xaxt = "n")
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")

#calculate peak q and total q in mm for whole storm 1
peakq.cms = max(q.cms, na.rm = TRUE) 
source("F:/TJ/R/TJ/events_report/Rfiles/function_calc_total_Q_mm.R")
total.q.mm = calculate.total.Q.mm(q.cms)

#FIGRUE 2
#Overall Panel Plots of everything
layout(matrix(1:4, ncol = 1), widths = 1, heights = c(0.05,0.05,0.04,0.05), respect = FALSE)
par(mar = c(0, 4.1, 0, 2.1)) #set margins for bottom, L, top, R
plot(precip$date.time, precip$Cumulative.rain.mm, type="l", xaxt = 'n', ylab="Cum. Rain (mm)", xlim = c(as.POSIXct(obs$date.time[1]),as.POSIXct(obs$date.time[length(obs$date.time)]))) 
legend("topleft", "A)", bty="n", cex=1.5, inset=c(-.09,-.15))
abline(v=as.POSIXct("2016-01-05 09:18", format="%Y-%m-%d %H:%M"),lty=2)
abline(v=as.POSIXct("2016-01-05 17:33", format="%Y-%m-%d %H:%M"),lty=2)
par(mar = c(0, 4.1, 0, 2.1))
plot(obs$date.time, obs$PT, type="l", col="blue", xaxt = 'n', ylab = "Pressure (m)")#,  ylim=c(10.2,10.45+.01))
lines(approx.tje$x, pressure.tje.m.approx, col="red", type="l",  xlab = "Date", ylab = "Barometric Pressure (m)")
lines(approx.tje.naval$x, pressure.tje.naval.m.approx,type="l", col="green")
lines(approx.tje.naval$x, tje.naval.adj, type = "l", col="green")
lines(approx.tje$x, tje.adj ,type="l", col="red")
legend("topleft", c("TJE","TJE Naval","PT"),  col=c("red","green","blue"),lwd=1, inset=c(.1,-.05), cex=0.75,bty = "n" ) #bty="n" means no box
legend("topleft", "B)", bty="n", cex=1.5, inset=c(-.09,-.15))
abline(v=as.POSIXct("2016-01-05 09:18", format="%Y-%m-%d %H:%M"),lty=2)
abline(v=as.POSIXct("2016-01-05 17:33", format="%Y-%m-%d %H:%M"),lty=2)
par(mar = c(0, 4.1, 0, 2.1))
plot(approx.tje.naval$x, stage.m,  type="l",  xaxt = 'n',   ylab = "Stage (m)")
legend("topleft", "C)", bty="n", cex=1.5, inset=c(-.09,-.2))
abline(v=as.POSIXct("2016-01-05 09:18", format="%Y-%m-%d %H:%M"),lty=2)
abline(v=as.POSIXct("2016-01-05 17:33", format="%Y-%m-%d %H:%M"),lty=2)
par(mar = c(4, 4.1, 0, 2.1))
plot(obs$date.time, q.cms, type ="l", xlab = "Date",ylab = "Discharge (cms)", xaxt = 'n')
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")
legend("topleft", "D)", bty="n", cex=1.5, inset=c(-.09,-.4))
abline(v=as.POSIXct("2016-01-05 09:18", format="%Y-%m-%d %H:%M"),lty=2)
abline(v=as.POSIXct("2016-01-05 17:33", format="%Y-%m-%d %H:%M"),lty=2)

#put layout of graphs back to one graph
par(mfrow=c(1,1),xpd=FALSE)
dev.off() #to reset back to one plot


#Splitting the storm into multiple smaller events since AGNPS only takes a daily total rainfall
#daily total precip for 2016-01-04
precip1 = precip[precip$date == "2016-01-04",]
precip_20160104 = precip1$Cumulative.rain.mm[length(precip1$date)] #total precip for 2016-01-04 is the last value for that day
#daily total precip for 2016-01-05
precip2 = precip[precip$date == "2016-01-05",]
precip_20160105.cum = precip2$Cumulative.rain.mm[length(precip2$date)] #cum precip up to 2016-01-05
precip_20160105 = precip_20160105.cum - precip_20160104 #total precip for 2016-01-05
#daily total precip for 2016-01-06
precip3 = precip[precip$date == "2016-01-06",]
precip_20160106.cum = precip3$Cumulative.rain.mm[length(precip3$date)] #cum precip up to 2016-01-06
precip_20160106 = precip_20160106.cum - precip_20160105.cum  #total precip for 2016-01-06
#daily total precip for 2016-01-07
precip4 = precip[precip$date == "2016-01-07",]
precip_20160107.cum = precip4$Cumulative.rain.mm[length(precip4$date)] #cum precip up to 2016-01-07
precip_20160107 = precip_20160107.cum - precip_20160106.cum  #total precip for 2016-01-07
#daily total precip for 2016-01-07
precip5 = precip[precip$date == "2016-01-08",]
precip_20160108.cum = precip5$Cumulative.rain.mm[length(precip5$date)] #cum precip up to 2016-01-07
precip_20160108 = precip_20160108.cum - precip_20160107.cum  #total precip for 2016-01-07

total = precip_20160104+precip_20160105+precip_20160106+precip_20160107+precip_20160108

#total rain prior to event: start rainfall to 2016-01-05 09:18 
prior.rain = precip2$Cumulative.rain.mm[precip2$time == "09:18:00"]
#Event 1: start 2016-01-05 09:18 to 2016-01-05 17:33
#calc total rainfall, total q, peak q for this new subse
totalp.event1 = precip2$Cumulative.rain.mm[precip2$time == "17:33:00"] - prior.rain
ind.start = grep(as.POSIXct("2016-01-05 09:20:00"),obs$date.time)
ind.end1 = grep(as.POSIXct("2016-01-05 17:35:00"),obs$date.time)
event1.q.cms = q.cms[ind.start:ind.end1]
event1.date.time = obs$date.time[ind.start:ind.end1]
peakq.event1 = max(event1.q.cms, na.rm = TRUE) 
total.q.mm.1 = calculate.total.Q.mm(event1.q.cms)
#zoom plot of event to look for oscillations
plot(obs$date.time, q.cms, type="l", main="01/05/16", xlab="Time", ylab="Discharge (cms)")
peak.date.time.ind = which(q.cms==peakq.cms)
peak.date.time = obs$date.time[peak.date.time.ind]
#calculate time to peak
time.2.peak.e1 = peak.date.time - as.POSIXct("2016-01-05 13:03:00") #start rainfall at 1/5/2016 13:03

#remaining rainfall 2016-01-05 17:33 to 2016-01-06 20:08
remaining.rain = precip3$Cumulative.rain.mm[precip3$time == "20:08:00"]  - precip2$Cumulative.rain.mm[precip2$time == "17:33:00"]

date.time2 = data.frame(obs$date.time)
data = cbind(date.time2, q.cms)
write.csv(data, file="F:/TJ/Validation_data/Google_drive/processed_data/20160104_discharge_timeseries.csv",row.names=F)


#output summary for observed data
date = c("2016-01-05")
peak.q.obs.cms = c(peakq.cms)
total.q.obs.mm = c(total.q.mm)
obs.summary = cbind(date, peak.q.obs.cms, total.q.obs.mm) #may want to add time to peak column
write.csv(obs.summary, file="F:/TJ/Validation_data/Google_drive/processed_data/20160105_observed_q_summary.csv",row.names=F)

##############################################################################################################################
#For Doug aggregate precip to 15 minute intervals
#read in precip data for the very top plot in figure 2
precip0 = read.csv(file = "llc_observed_data_Jan05_2016_precip.csv", header=TRUE)
date.time = strptime(precip0$Date.time,"%m/%d/%Y %H:%M")
time = format(date.time,"%H:%M:%S")
date = format(date.time,"%Y-%m-%d" )
precip00 = cbind(precip0, date.time, date, time)
precip = na.omit(precip00)
x = 0.25 #0.25 mm for each tip
rain.mm.depth = rep(x, length(precip$Date.time))
precip.new = cbind(precip, rain.mm.depth)

timebreak1 = ISOdatetime(2016, 1, 4, 2, 0, 0) + (seq(0,4*24*5)*15*60) #4 breaks per 24 hours for 5 days; 15 min breaks
fac = cut(precip.new$date.time, breaks = timebreak1)
data2 = data.frame(cbind(precip.new, fac))
agg = data.frame(aggregate(rain.mm.depth, by  = list(fac), FUN = sum))
write.csv(agg, file="01042016_precip_15min_LLC_Doug.csv")

#For Napo break into every 6 minute intervals
timebreak1 = ISOdatetime(2016, 1, 4, 2, 0, 0) + (seq(0,10*24*5)*6*60) #10 breaks every 24 hours for 5 days; every 6 minutes
fac = cut(precip.new$date.time, breaks = timebreak1)
data2 = data.frame(cbind(precip.new, fac))
agg = data.frame(aggregate(rain.mm.depth, by  = list(fac), FUN = sum)) #second column has precip depth mm at each time stamp

cum.rain.new.mm = cumsum(agg$x) #cumulative rainfall mm
date.time.new = agg$Group.1 #the new date time in 6 minute intervals
##############################################################################################################################

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

#CONCEPTS hydrograph in comparison with Observed: 01/05/16
sub = main2[main2$month.day.year=="01/05/2016",]
peakq.concepts1 = max(sub$q.adjusted.cms)
peak.depth = max(sub$DEPTH.m)
peak.concepts.ind = which(sub$q.adjusted.cms==peakq.concepts1)
peak.concepts.date.time1 = sub$date.time[peak.concepts.ind]
time.2.peak.concepts.e1 = peak.concepts.date.time1 - sub$date.time[1] #time to peak concepts

plot(obs$date.time, q.cms, type="l", main="01/05/16", xlab="Time", ylab="Discharge (cms)", xlim = c(as.POSIXct("2016-01-05 00:00:00"),as.POSIXct("2016-01-06 00:00:00")))
lines(sub$date.time, sub$q.adjusted.cms, type="l", xlab = "Time (hrs)", ylab = "Discharge (cms)", main = "2/28/14 E1", col="blue")
legend("topleft", c("Observed","CONCEPTS"),  col=c("black","blue"),lwd=1, cex=0.75,bty = "n" ) #bty="n" means no box







