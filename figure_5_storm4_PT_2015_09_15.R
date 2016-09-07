#Storm Event 4: 9/15/2015
#Figure 5 in Events Report for EPA

#to read in PT data, read in barometric data, subtract to get water depth (sections 1-4)
setwd("F:/TJ/Validation_data/Google_drive")

#1. read in barometric data from TJE
tje0 = read.csv(file = "TJE_atm_pressure.csv", skip=2, header=TRUE) #sheet from TJE for barometric data
date.time = strptime(tje0$Date,"%m/%d/%Y %H:%M")
time = format(date.time,"%H:%M:%S")
date = format(date.time,"%Y-%m-%d" )
tje = cbind(tje0, date.time, date, time)

#2. read in the PT_level data
obs0 = read.csv(file = "llc_observed_data_sep15_2015.csv", header=TRUE)
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

#step 3 using SD pressure data
san0 = read.csv(file="KSAN-Oct2014-2015-downloaded_3_10.csv", header=TRUE) 
date.time = strptime(san0$X,"%Y-%m-%d %H:%M:%S")
time = format(date.time,"%H:%M:%S")
date = format(date.time,"%Y-%m-%d" )
san = cbind(san0, date.time, date, time)
pressure.sd.m = san$Sea.Level.PressureIn*33.8637526/(0.09806649999980076*1000) #from in Hg to mbar to depth water m
approx.san = data.frame(approx(san$date.time, pressure.sd.m, obs$date.time)) #pressure in mb
pressure.san.m.approx = approx.san$y 
stage.m.san = obs$PT - pressure.san.m.approx

#Adjustment of Barometric data for 9/2015 Event
#TJE data ajusted based on friday 2015-09-15 til noon mean (baseflow day, right before peak)
date = as.Date(approx.tje$x)
approx.tje2 = cbind(approx.tje, date)
ind = grep(as.POSIXct("2015-09-15 12:00:00 PST"),approx.tje2$x)
friday.tje = approx.tje2[1:ind,]
av.tje = mean(friday.tje$y )/(0.09806649999980076*1000)
#SD data ajusted based on friday  2015-09-15 til noon mean (baseflow day, right before peak)
date = as.Date(approx.san$x)
approx.san2 = cbind(approx.san, date)
ind = grep(as.POSIXct("2015-09-15 12:00:00 PST"),approx.san2$x)
friday.san = approx.san2[1:ind,]
av.san = mean(friday.san$y )
#TJE Naval data ajusted based on Friday 2015-09-15 til noon mean (baseflow day)
date = as.Date(approx.tje.naval$x)
approx.tje.naval2 = cbind(approx.tje.naval, date)
ind = grep(as.POSIXct("2015-09-15 12:00:00 PST"),approx.tje.naval2$x)
friday.tje.naval = approx.tje.naval2[1:ind,]
av.tje.naval = mean(friday.tje.naval$y) 
#observed mean for Friday til noon
ind = grep(as.POSIXct("2015-09-15 12:00:00 PST"),obs$date.time)
friday.obs = obs[1:ind,]
av.obs = mean(friday.obs$PT)
#adjusted data: subtract the diff in the means to the barometric data
diff.mean.tje = av.tje - av.obs
tje.adj = approx.tje2$y/(0.09806649999980076*1000) - diff.mean.tje
diff.mean.san = av.san - av.obs
san.adj = approx.san2$y - diff.mean.san
diff.mean.tje.naval = av.tje.naval - av.obs
tje.naval.adj = approx.tje.naval2$y - diff.mean.tje.naval

#plot the pressure from the pt and other barometric sources just to see
plot(obs$date.time, obs$PT, type="l", col="blue",  xlab = "Date", ylab = "Pressure (m)",xaxt = "n") 
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")
lines(approx.tje$x, pressure.tje.m.approx,type="l", col="red")
lines(approx.san$x, pressure.san.m.approx,type="l", col="orange")
lines(approx.tje.naval$x, pressure.tje.naval.m.approx,type="l", col="green")
lines(approx.san$x, san.adj ,type="l",col="orange")
lines(approx.tje.naval$x, tje.naval.adj, type = "l", col="green")
lines(approx.tje$x, tje.adj, type = "l", col="red")


#read in precip data for the very top plot in figure 2
precip0 = read.csv(file = "llc_observed_data_sep15_2015_precip.csv", header=TRUE)
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
plot(obs$date.time, q.cms, type ="l", xlab = "Date", ylab = "Discharge (cms)", xaxt = "n",xlim = c(as.POSIXct(obs$date.time[1]),as.POSIXct("2015-03-03 00:00:00 PST")))
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")

#calculate peak q and total q in mm for whole storm 1
peakq.cms = max(q.cms, na.rm = TRUE) 
source("F:/TJ/R/TJ/events_report/Rfiles/function_calc_total_Q_mm.R")
total.q.mm = calculate.total.Q.mm(q.cms)

#FIGRUE 5
#Overall Panel Plots of everything
layout(matrix(1:4, ncol = 1), widths = 1, heights = c(0.05,0.05,0.04,0.05), respect = FALSE)
par(mar = c(0, 4.1, 0, 2.1)) #set margins for bottom, L, top, R
plot(precip$date.time, precip$Cumulative.rain.mm, type="l", xaxt = 'n', ylab="Cum. Rain (mm)", xlim = c(as.POSIXct(as.character(obs$date.time[1])),as.POSIXct(as.character(obs$date.time[length(obs$date.time)])))) 
legend("topleft", "A)", bty="n", cex=1.5, inset=c(-.09,-.15))
abline(v=as.POSIXct("2015-09-15 10:47", format="%Y-%m-%d %H:%M"), lty=2)
abline(v=as.POSIXct("2015-09-16 05:52", format="%Y-%m-%d %H:%M"), lty=2)
#abline(v=as.POSIXct("2015-09-16 00:00", format="%Y-%m-%d %H:%M"))
par(mar = c(0, 4.1, 0, 2.1))
plot(obs$date.time, obs$PT, type="l", col="blue", xaxt = 'n', ylab = "Pressure (m)", ylim=c(10,10.75))#, xlim = c(as.POSIXct(obs$date.time[1]),as.POSIXct("2015-03-03 00:00:00 PST")))
lines(approx.tje$x, pressure.tje.m.approx,type="l", col="red")
lines(approx.san$x, pressure.san.m.approx,type="l",col="brown")
lines(approx.tje.naval$x, pressure.tje.naval.m.approx,type="l", col="green")
lines(approx.san$x, san.adj ,type="l",col="brown")
lines(approx.tje.naval$x, tje.naval.adj, type = "l", col="green")
lines(approx.tje$x, tje.adj, type = "l", col="red")
legend("topleft", c("TJE","SAN","TJE Naval","PT"),  col=c("red","brown","green","blue"),lwd=1,lty=c(1,1,1,1), inset=c(.05,-.04), cex=0.75,bty = "n" ) #bty="n" means no box
legend("topleft", "B)", bty="n", cex=1.5, inset=c(-.09,-.15))
abline(v=as.POSIXct("2015-09-15 10:47", format="%Y-%m-%d %H:%M"), lty=2)
abline(v=as.POSIXct("2015-09-16 05:52", format="%Y-%m-%d %H:%M"), lty=2)
#abline(v=as.POSIXct("2015-09-16 00:00", format="%Y-%m-%d %H:%M"))
par(mar = c(0, 4.1, 0, 2.1))
plot(approx.tje.naval$x, stage.m,  type="l",  xaxt = 'n',   ylab = "Stage (m)")#xlim = c(as.POSIXct(obs$date.time[1]),as.POSIXct("2015-03-03 00:00:00 PST")))
legend("topleft", "C)", bty="n", cex=1.5, inset=c(-.09,-.2))
abline(v=as.POSIXct("2015-09-15 10:47", format="%Y-%m-%d %H:%M"), lty=2)
abline(v=as.POSIXct("2015-09-16 05:52", format="%Y-%m-%d %H:%M"), lty=2)
#abline(v=as.POSIXct("2015-09-16 00:00", format="%Y-%m-%d %H:%M"))
par(mar = c(4, 4.1, 0, 2.1))
plot(obs$date.time, q.cms, type ="l", xlab = "Date",ylab = "Discharge (cms)", xaxt = 'n')#,xlim = c(as.POSIXct(obs$date.time[1]),as.POSIXct("2015-03-03 00:00:00 PST")))
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")
legend("topleft", "D)", bty="n", cex=1.5, inset=c(-.09,-.4))
abline(v=as.POSIXct("2015-09-15 10:47", format="%Y-%m-%d %H:%M"), lty=2)
abline(v=as.POSIXct("2015-09-16 05:52", format="%Y-%m-%d %H:%M"), lty=2)
#abline(v=as.POSIXct("2015-09-16 00:00", format="%Y-%m-%d %H:%M"))


#put layout of graphs back to one graph
par(mfrow=c(1,1),xpd=FALSE)
dev.off() #to reset back to one plot


#Only one event 
#daily total precip for 2015-09-15
precip1 = precip[precip$date == "2015-09-15",]
precip_20150915 = precip1$Cumulative.rain.mm[length(precip1$date)] #total precip for 2015-09-15 is the last value for that day

#daily total precip for 2015-09-16
precip2 = precip[precip$date == "2015-09-16",]
precip_20150916.cum = precip2$Cumulative.rain.mm[length(precip2$date)] #total precip for 2015-09-15 is the last value for that day
precip_20150916 = precip_20150916.cum - precip_20150915

#Event 1: start 2015-09-15 10:47 to 2015-09-16 05:52
#calc total rainfall, total q, peak q for this new subset
ind.start = grep(as.POSIXct("2015-09-15 10:45:00"),obs$date.time)
ind.end1 = grep(as.POSIXct("2015-09-16 05:50:00"),obs$date.time)
event1.q.cms = q.cms[ind.start:ind.end1]
event1.date.time = obs$date.time[ind.start:ind.end1]
peakq.event1 = max(event1.q.cms, na.rm = TRUE) 
total.q.mm.1 = calculate.total.Q.mm(event1.q.cms)
#zoom plot of event to look for oscillations
plot(obs$date.time, q.cms, type="l", main="09/15/15", xlab="Time", ylab="Discharge (cms)")
peak.date.time.ind = which(q.cms==peakq.cms)
peak.date.time = obs$date.time[peak.date.time.ind]
#calculate time to peak
time.2.peak.e1 = peak.date.time - event1.date.time[1] #start rainfall at 2015-09-15 10:45:00

date.time2 = data.frame(obs$date.time)
data = cbind(date.time2, q.cms)
write.csv(data, file="F:/TJ/Validation_data/Google_drive/processed_data/20150915_discharge_timeseries.csv",row.names=F)

#output summary for observed data
date = c("2015-09-15")
peak.q.obs.cms = c(peakq.cms)
total.q.obs.mm = c(total.q.mm)
obs.summary = cbind(date, peak.q.obs.cms, total.q.obs.mm) #may want to add time to peak column
write.csv(obs.summary, file="F:/TJ/Validation_data/Google_drive/processed_data/20150915_observed_q_summary.csv",row.names=F)


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

#CONCEPTS hydrograph in comparison with Observed: 09/15/15
sub = main2[main2$month.day.year=="09/15/2015",]
peakq.concepts1 = max(sub$q.adjusted.cms)
peak.depth = max(sub$DEPTH.m)
peak.concepts.ind = which(sub$q.adjusted.cms==peakq.concepts1)
peak.concepts.date.time1 = sub$date.time[peak.concepts.ind]
time.2.peak.concepts.e1 = peak.concepts.date.time1 - sub$date.time[1] #time to peak concepts

plot(obs$date.time, q.cms, type="l", main="09/15/15", xlab="Time", ylab="Discharge (cms)", ylim=c(0,peakq.concepts1))
lines(sub$date.time, sub$q.adjusted.cms, type="l", xlab = "Time (hrs)", ylab = "Discharge (cms)", main = "2/28/14 E1", col="blue")
legend("topright", c("Observed","CONCEPTS"),  col=c("black","blue"),lwd=1, cex=0.75,bty = "n" ) #bty="n" means no box






