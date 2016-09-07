#Storm Event 1: 3/2014 
#Figure 2 in Events Report for EPA

#to read in PT data, read in barometric data, subtract to get water depth (sections 1-4)
setwd("F:/TJ/Validation_data/Google_drive")

#1. read in barometric data from TJE
tje0 = read.csv(file = "TJE_atm_pressure.csv", skip=2, header=TRUE) #sheet from TJE for barometric data
date.time = strptime(tje0$Date,"%m/%d/%Y %H:%M")
time = format(date.time,"%H:%M:%S")
date = format(date.time,"%Y-%m-%d" )
tje = cbind(tje0, date.time, date, time)
#2. read in the PT_level data
obs0 = read.csv(file = "llc_observed_data_March2014.csv", header=TRUE)
date.time0 = paste(as.character(obs0$date), as.character(obs0$time), sep=" ")
date.time = strptime(date.time0,"%m/%d/%Y %H:%M:%S") #had lower case s instead of S
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

#Adjustment of Barometric data for 3/2014 Event
#tje data ajusted based on friday (2014-02-28) til noon mean (baseflow day, right before peak)
  date = as.Date(approx.tje$x)
  approx.tje2 = cbind(approx.tje, date)
  ind = grep(as.POSIXct("2014-02-28 12:00:00 PST"),approx.tje2$x)
  friday.tje = approx.tje2[1:ind,]
  av.tje = mean(friday.tje$y/(0.09806649999980076*1000) ) #from mb to m
#TJE Naval data ajusted based on Friday til noon mean (baseflow day)
  date = as.Date(approx.tje.naval$x)
  approx.tje.naval2 = cbind(approx.tje.naval, date)
  ind = grep(as.POSIXct("2014-02-28 12:00:00 PST"),approx.tje.naval2$x)
  friday.tje.naval = approx.tje.naval2[1:ind,]
  av.tje.naval = mean(friday.tje.naval$y) 
#observed mean for Friday til noon
  ind = grep(as.POSIXct("2014-02-28 12:00:00 PST"),obs$date.time)
  friday.obs = obs[1:ind,]
  av.obs = mean(friday.obs$PT)
#adjusted data: subtract the diff in the means to the barometric data
  diff.mean.tje = av.tje - av.obs
  tje.adj = approx.tje2$y/(0.09806649999980076*1000) - diff.mean.tje
  diff.mean.tje.naval = av.tje.naval - av.obs
  tje.naval.adj = approx.tje.naval2$y - diff.mean.tje.naval
#plot the pressure from the pt and other barometric sources
  plot(obs$date.time, obs$PT, type="l", col="blue",  xlab = "Date", ylab = "Pressure (m)" , ylim=c(10.2,10.45),xaxt = "n")
  axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")
  lines(approx.tje$x, pressure.tje.m.approx, col="red", type="l",  xlab = "Date", ylab = "Barometric Pressure (m)")
  lines(approx.tje.naval$x, pressure.tje.naval.m.approx,type="l", col="green")
  lines(approx.tje$x, tje.adj ,type="l", col="red") #adjusted tje
  lines(approx.tje.naval$x, tje.naval.adj, type = "l", col="green") #adjusted tje.naval
#also tried adjusting based on Sunday, but did not work well, see PT_script_atmpressure_waterlevel.R to see sunday
  
  
#read in precip data for the very top plot in figure 2
  precip0 = read.csv(file = "llc_observed_data_March2014_precip.csv", header=TRUE)
  date.time = strptime(precip0$Date.time,"%m/%d/%Y %H:%M")
  time = format(date.time,"%H:%M:%S")
  date = format(date.time,"%Y-%m-%d" )
  precip00 = cbind(precip0, date.time, date, time)
  precip = na.omit(precip00)
  
#CACULATE THE ADJUSTED STAGE, for event 1 use TJE naval to adjust, for event2 use TJE to adjust
  #EVENT1: calculate the stage, us for loop to replace all of the negative values with 0 using TJE naval
  stage.m0 = obs$PT - tje.naval.adj
  stage.m = 0
  for (i in 1:length(stage.m0))
    { if (stage.m0[i] < 0) 
        {stage.m[i] = 0}
      else {stage.m[i] = stage.m0[i]}  }
  #calculate Q
  source("F:/TJ/R/TJ/events_report/Rfiles/function_calc_Q.R")
  q.cms = calculateQ(stage.m)
  ind.end1 = grep(as.POSIXct("2014-02-28 15:50:00"),obs$date.time) #index for the end of event1
  q.cms.e1 = data.frame(q.cms[1:ind.end1])
  names(q.cms.e1) <- ("q.cms")
  stage.m.e1 = data.frame(stage.m[1:ind.end1])
  names(stage.m.e1) <- ("stage.m")
  #EVENT2: use tje to adjust the second event stage and q calcs:
  #calculate the stage, us for loop to replace all of the negative values with 0
  stage.m00 = obs$PT - tje.adj
  stage.m2 = 0
  for (i in 1:length(stage.m00))
  { if (stage.m00[i] < 0) 
  {stage.m2[i] = 0}
    else {stage.m2[i] = stage.m00[i]}  }
  #calculate Q for the second event
  q.cms02 = calculateQ(stage.m2)
  q.cms.e2.on = data.frame(q.cms02[(ind.end1+1):length(q.cms02)] )#only use this adjusted for the second event
  names(q.cms.e2.on) <- ("q.cms")
  stage.m.e2.on = data.frame(stage.m2[(ind.end1+1):length(stage.m2)])
  names(stage.m.e2.on) <- ("stage.m")

  q.cms = rbind(q.cms.e1, q.cms.e2.on)
  q.cms.adj = q.cms[1:781,1]
  stage.m = rbind(stage.m.e1, stage.m.e2.on)
  stage.m.adj = stage.m[1:781,1]
  #plot Q timeseries 
  plot(obs$date.time, q.cms.adj, type ="l", xlab = "Date", ylab = "Discharge (cms)", xaxt = "n")
  axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")
  
  #calculate peak q and total q in mm for whole storm 1
  peakq.cms = max(q.cms.adj, na.rm = TRUE) 
  source("F:/TJ/R/TJ/events_report/Rfiles/function_calc_total_Q_mm.R")
  total.q.mm = calculate.total.Q.mm(q.cms.adj, obs$date.time) #updated function needs to use obs$date.time

#FIGRUE 2
  #Overall Panel Plots of everything
  layout(matrix(1:4, ncol = 1), widths = 1, heights = c(0.05,0.05,0.04,0.05), respect = FALSE)
  par(mar = c(0, 4.1, 0, 2.1)) #set margins for bottom, L, top, R
  plot(precip$date.time, precip$Cumulative.rain.mm, type="l", xaxt = 'n', ylab="Cum. Rain (mm)", xlim = c(as.POSIXct(obs$date.time[1]),as.POSIXct(obs$date.time[length(obs$date.time)]))) 
  legend("topleft", "A)", bty="n", cex=1.5, inset=c(-.09,-.15)) #inset first value is L-R, second is up-down
  legend("topleft", "E1", bty="n", cex=1.5, inset=c(0.05,-.15))
  legend("topleft", "E2", bty="n", cex=1.5, inset=c(0.21,-.15))
  abline(v=as.POSIXct("2014-02-28 00:00", format="%Y-%m-%d %H:%M"), lty=2)
  abline(v=as.POSIXct("2014-02-28 15:50", format="%Y-%m-%d %H:%M"), lty=2)
  abline(v=as.POSIXct("2014-03-01 00:00", format="%Y-%m-%d %H:%M"), lty=2)
  #abline(v=as.POSIXct("2014-02-28 13:00", format="%Y-%m-%d %H:%M"))
  par(mar = c(0, 4.1, 0, 2.1))
  plot(obs$date.time, obs$PT, type="l", col="blue", xaxt = 'n', ylab = "Pressure (m)",  ylim=c(10.2,10.45+.01))
  lines(approx.tje$x, pressure.tje.m.approx, col="red", type="l",  xlab = "Date", ylab = "Barometric Pressure (m)")
  lines(approx.tje.naval$x, pressure.tje.naval.m.approx,type="l", col="green")
  lines(approx.tje.naval$x, tje.naval.adj, type = "l", col="green")
  lines(approx.tje$x, tje.adj ,type="l", col="red")
  legend("topleft", c("TJE","TJE Naval","PT"),  col=c("red","green","blue"),lwd=1, inset=c(.1,-.05), cex=0.75,bty = "n" ) #bty="n" means no box
  legend("topleft", "B)", bty="n", cex=1.5, inset=c(-.09,-.15))
  abline(v=as.POSIXct("2014-02-28 00:00", format="%Y-%m-%d %H:%M"), lty=2)
  abline(v=as.POSIXct("2014-02-28 15:50", format="%Y-%m-%d %H:%M"), lty=2)
  abline(v=as.POSIXct("2014-03-01 00:00", format="%Y-%m-%d %H:%M"), lty=2)
  par(mar = c(0, 4.1, 0, 2.1))
  plot(approx.tje.naval$x, stage.m.adj,  type="l",  xaxt = 'n',   ylab = "Stage (m)")
  legend("topleft", "C)", bty="n", cex=1.5, inset=c(-.09,-.2))
  abline(v=as.POSIXct("2014-02-28 00:00", format="%Y-%m-%d %H:%M"), lty=2)
  abline(v=as.POSIXct("2014-02-28 15:50", format="%Y-%m-%d %H:%M"), lty=2)
  abline(v=as.POSIXct("2014-03-01 00:00", format="%Y-%m-%d %H:%M"), lty=2)
  #abline(v=as.POSIXct("2014-02-28 13:00", format="%Y-%m-%d %H:%M"))
  par(mar = c(4, 4.1, 0, 2.1))
  plot(obs$date.time, q.cms.adj, type ="l", xlab = "Date",ylab = "Discharge (cms)", xaxt = 'n')
  axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")
  legend("topleft", "D)", bty="n", cex=1.5, inset=c(-.09,-.4))
  abline(v=as.POSIXct("2014-02-28 00:00", format="%Y-%m-%d %H:%M"), lty=2)
  abline(v=as.POSIXct("2014-02-28 15:50", format="%Y-%m-%d %H:%M"), lty=2)
  abline(v=as.POSIXct("2014-03-01 00:00", format="%Y-%m-%d %H:%M"), lty=2)
  #abline(v=as.POSIXct("2014-02-28 13:00", format="%Y-%m-%d %H:%M"))
  
  #put layout of graphs back to one graph
  par(mfrow=c(1,1),xpd=FALSE)
  dev.off() #to reset back to one plot
  
#Splitting the storm into multiple smaller events since AGNPS only takes a daily total rainfall
  #daily total precip for 2/27/14
  precip0 = precip[precip$date == "2014-02-27",]
  precip_20140227 = precip0$Cumulative.rain.mm[length(precip0$date)] #total precip for 2/28/14 is the last value for that day
  #daily total precip for 2/28/14
  precip1 = precip[precip$date == "2014-02-28",]
  precip_20140228.cum = precip1$Cumulative.rain.mm[length(precip1$date)] #total precip for 2/28/14 is the last value for that day
  precip_20140228 = precip_20140228.cum - precip_20140227
  #daily total precip for 3/1/14
  precip2 = precip[precip$date == "2014-03-01",]
  precip_20140301.cum = precip2$Cumulative.rain.mm[length(precip2$date)] #cum precip up to 3/1/14
  precip_20140301 = precip_20140301.cum - precip_20140228.cum #total precip for 3/1/14
  #daily total precip for 3/2/14
  precip3 = precip[precip$date == "2014-03-02",]
  precip_20140302.cum = precip3$Cumulative.rain.mm[length(precip3$date)] #cum precip up to 3/2/14
  precip_20140302 = precip_20140302.cum - precip_20140301.cum  #total precip for 3/2/14

#Event 1: start 2/28/14 00:00 to 2/28/14 15:50
  #calc total rainfall, total q, peak q for this new subset
  totalp.event1 = precip1$Cumulative.rain.mm[precip1$time == "15:50:00"] - precip_20140227
  ind.end1 = grep(as.POSIXct("2014-02-28 15:50:00"),obs$date.time)
  event1.q.cms = q.cms.adj[1:ind.end1]
  event1.date.time = obs$date.time[1:ind.end1]
  peakq.event1 = max(event1.q.cms, na.rm = TRUE) 
  total.q.mm.1 = calculate.total.Q.mm(event1.q.cms)
  #zoom plot of event to look for oscillations
  plot(event1.date.time,event1.q.cms, type="l", main="2/28/14 E1", xlab="Time", ylab="Discharge (cms)")
  peak.date.time.ind = which(event1.q.cms==peakq.event1)
  peak.date.time = event1.date.time[peak.date.time.ind]
  #calculate time to peak, assume start of rainfall is at 2/28/2014 10:09
  time.2.peak.e1 = peak.date.time - as.POSIXct("2014-02-28 10:09:00")
  
#Event 2: start 2/28/14 15:50 to 2014-03-01 00:00 (or to the last value before it hits 3/1)
  #calc total rainfall, total q, peak q for this new subset
  totalp.event2 = precip1$Cumulative.rain.mm[length(precip1$Cumulative.rain.mm)] - precip1$Cumulative.rain.mm[precip1$time == "15:50:00"]
  ind.end2 = grep(as.POSIXct("2014-03-01 00:00:00"),obs$date.time) #since 00:00:00 time gets dropped it give you all indexes for that day, use first one to get 00:00
  event2.q.cms = q.cms.adj[(ind.end1+1):ind.end2[1]]
  event2.date.time = obs$date.time[(ind.end1+1):ind.end2[1]]
  peakq.event2 = max(event2.q.cms, na.rm = TRUE) 
  total.q.mm.2 = calculate.total.Q.mm(event2.q.cms)
  #zoom plot for E2
  plot(event2.date.time,event2.q.cms, type="l", main="3/01/14 E2", xlab="Time", ylab="Discharge (cms)")
  peak.date.time.ind2 = which(event2.q.cms==peakq.event2)
  peak.date.time2 = event2.date.time[peak.date.time.ind2]
  #calculate time to peak
  time.2.peak.e2 = peak.date.time2 - as.POSIXct("2014-02-28 15:50:00")
  
  #total precip depth for last day 3/1 to 3/2
  totalp.lastday =  precip3$Cumulative.rain.mm[length(precip3$Cumulative.rain.mm)]-precip1$Cumulative.rain.mm[length(precip1$Cumulative.rain.mm)]
  
  date.time2 = data.frame(obs$date.time)
  event1 = rep("E1",times=length(event1.q.cms))
  event2 = rep("E2",times=length(event2.q.cms))
  noevent = rep("NA",times=(length(q.cms.adj)-length(event1)-length(event2)))
  Event = c(event1, event2,noevent)
  data = cbind(date.time2, q.cms.adj, Event)
  write.csv(data, file="F:/TJ/Validation_data/Google_drive/processed_data/20140301_discharge_timeseries.csv",row.names=F)
    #discharge data for whole event
  
  #output summary for observed data
  date = c("2014-02-28","2014-03-01")
  peak.q.obs.cms = c(peakq.event1,peakq.event2)
  total.q.obs.mm = c(total.q.mm.1,total.q.mm.2)
  obs.summary = cbind(date, peak.q.obs.cms, total.q.obs.mm) #may want to add time to peak column
  write.csv(obs.summary, file="F:/TJ/Validation_data/Google_drive/processed_data/20140301_observed_q_summary.csv",row.names=F)
  
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
  
  #CONCEPTS hydrograph in comparison with Observed: 2/28/2014 E1
  sub = main2[main2$month.day.year=="02/28/2014",]
  peakq.concepts1 = max(sub$q.adjusted.cms)
  peak.depth = max(sub$DEPTH.m)
  peak.concepts.ind = which(sub$q.adjusted.cms==peakq.concepts1)
  peak.concepts.date.time1 = sub$date.time[peak.concepts.ind]
  time.2.peak.concepts.e1 = peak.concepts.date.time1 - sub$date.time[1] #time to peak concepts
  
  plot(event1.date.time,event1.q.cms, type="l", main="2/28/14 E1", xlab="Time", ylab="Discharge (cms)")
  lines(sub$date.time, sub$q.adjusted.cms, type="l", xlab = "Time (hrs)", ylab = "Discharge (cms)", main = "2/28/14 E1", col="blue")
  legend("topleft", c("Observed","CONCEPTS"),  col=c("black","blue"),lwd=1, cex=0.75,bty = "n" ) #bty="n" means no box
  
  #CONCEPTS hydrograph in comparison with Observed: 03/01/2014 E2
  sub2 = main2[main2$month.day.year=="03/01/2014",]
  peakq.concepts2 = max(sub2$q.adjusted.cms)
  peak.depth = max(sub2$DEPTH.m)
  peak.concepts.ind2 = which(sub2$q.adjusted.cms==peakq.concepts2)
  peak.concepts.date.time2 = sub2$date.time[peak.concepts.ind2]
  time.2.peak.concepts.e2 = peak.concepts.date.time2 - sub2$date.time[1] #time to peak concepts
  plot(event2.date.time,event2.q.cms, type="l", main="03/01/14 E2", xlab="Time", ylab="Discharge (cms)", xlim=c(as.POSIXct(event2.date.time[1]), as.POSIXct(sub2$date.time[length(sub2$date.time)])))
  lines(sub2$date.time, sub2$q.adjusted.cms, type="l", xlab = "Time (hrs)", ylab = "Discharge (cms)", main = "2/28/14 E1", col="blue")
  legend("topright", c("Observed","CONCEPTS"),  col=c("black","blue"),lwd=1, cex=0.75,bty = "n" ) #bty="n" means no box
  
  
