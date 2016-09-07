#If CONCEPTS model doesn't reach baseflow, need to exclude the no-flow days and the sediment generated on those days!

####################################################################################################################################################################
#TO add up all of the set trib baseflows (updated) used in CONCEPTS and subtract that from all of the values of q from main at outlet timeseries_01
setwd("C:/Users/Kris/Documents/GitHub/Dissertation/AGNPS/Napo_4_4_16/trib_files_baseflow_updated_5_16_2016")
all  = list.files("C:/Users/Kris/Documents/GitHub/Dissertation/AGNPS/Napo_4_4_16/trib_files_baseflow_updated_5_16_2016")

fname = NA
baseflowtxt = NA
for (i in 1: length(all)) {
  z = readLines(all[i])
  fname[i] = all[i]
  baseflowtxt[i] = z[3]    
}

baseflow.sum = sum(as.numeric(baseflowtxt))  #was 0.165, now 0.133
##############################################################################################################################
#To exclude or create subset of only the days that have flow in CONCEPTS timeseries
#To read in orig timeseries data, subset based on AGNPS event dates!
setwd("C:/Users/Kris/Documents/GitHub/Dissertation/AGNPS/Napo_4_4_16/cc/AGNPS_files_baseflow_eddy_5_16_16_sedlim15000_1/Main_sed_08092016_20062012")  #cc model, no sed trap wy 2006-2012

#read in the timeseries file at the outlet
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
                "CONVEYANCE","F.SLOPE","HEAD","FROUDE","BEDSHEAR","SILTDIS","SANDDIS","GRAVELDIS","TOTALDIS","SILTYLD","SANDYLD","GRAVELYLD","TOTALYLD","CUMBED","THALWEG","CUMLAT")
                #"CONVEYANCE","F.SLOPE","HEAD","FROUDE","BEDSHEAR","SILTDIS","SANDDIS","GRAVELDIS","TOTALDIS","SILTYLD","SANDYLD","GRAVELYLD","TOTALYLD","CUMBED","THALWEG","CUMLAT","SAFETYL","SAFETYR_APCOHL_APCOHR_POREL_PORER","MATRICL","MATRICR","WBLKL","WBLKR","WWATERL","WWATERR","HYDPRL","HYDPRR","PHREAL","PHREAR","BANKTOPL","BANKTOPR")
                #"CONVEYANCE","F.SLOPE","HEAD","FROUDE","BEDSHEAR")

#Subset the timeseries so it only takes lines where there was a storm event
events = read.table("C:/Users/Kris/Documents/GitHub/Dissertation/AGNPS/Napo_4_4_16/cc/AGNPS_files_baseflow_eddy_5_16_16_sedlim15000_1/Main.up.inflow.267.276.events",skip = 1,header = FALSE)
uniq.events0 = unique(substring(as.character(events$V2), 1, 10)) #all of the unique AGNPS event dates
uniq.events = paste(substring(uniq.events0, 6, 7),"/", substring(uniq.events0, 9, 10), "/", substring(uniq.events0, 1, 4), sep = "") #rearrange the date so matches format of YYYY-MM-dd
#to rearrange the dates to match dates from main concepts
uniq.dates.concepts = as.character(unique(main$DATE)) #unique dates in concepts
row.match0 = na.omit(unique(match(uniq.events,uniq.dates.concepts))) #the row numbers in uniq.dates.concepts that should be included because there was an event sim in AGNPS
row.match = row.match0[1:length(row.match0)] #to make sure the NAs are excluded from previous row.match, previous was in weird format
concepts.flow.dates = uniq.dates.concepts[row.match0] #all the dates that should be included

main.sub = main[as.character(main$DATE) %in% concepts.flow.dates,] #get the main.subset of the timeseries!  only include dates that are in this list from AGNPS events

#Now use the main.subset for analyses!
date.time0 = paste(as.character(main.sub$DATE), as.character(main.sub$TIME), sep = " ")
time = strptime(as.character(main.sub$TIME),"%H:%M:%S")
date.time = strptime(date.time0,"%m/%d/%Y %H:%M:%S")
month = as.numeric(format(date.time, "%m"))
month.day = format(date.time, "%m/%d")
year = format(date.time, "%Y")
month.day.year = format(date.time, "%m/%d/%Y")
source("F:/TJ/R/TJ/events_report/Rfiles/function_water_year.R")
wy = water.year(month, year) #calculate wy
min.q = min(main.sub$DISCHARGE.cms) #check to see what the minimum value is in concepts timeseries, is it diff than baseflow.sum?
q.adjusted.cms = main.sub$DISCHARGE.cms - baseflow.sum #subtract out the baseflow sum from each!
#if min.q is less than baseflow.sum, replace negative q with zero
for (i in 1:length(q.adjusted.cms)) {
  if(q.adjusted.cms[i] < 0) {
    q.adjusted.cms[i] = 0}
  else{q.adjusted.cms[i] = q.adjusted.cms[i]}
}

main.sub2 = cbind(main.sub, date.time, month, month.day, year, month.day.year, time, wy, q.adjusted.cms)

#loop through each event, adjust the event-wise artificial baseflow, and summarize total Q (mm), peak q (cms), and sediment totals
unique.date = unique(month.day.year)
totalq.sum.m3.concepts = NA
totalq.mm.concepts = NA
peakq.cms.concepts = NA
q.adj.cms2 = NA #adjust q even more!
wy.event = NA
#variable for sediment summary
total.sed = NA #empty vectors that will get replaced with data
total.silt.ton = NA
total.sand.ton = NA
total.gravel.ton = NA
total.sed.add = NA

for (i in 1: length(unique.date)) {
  
  sub0 = main.sub2[main.sub2$month.day.year == unique.date[i],] #nth event sub
  q.adjusted.cms2 = sub0$q.adjusted.cms - min(sub0$q.adjusted.cms) #not sure if we should subtract out the min or the mode...more likely the mode would be the artificial baseflow
  sub = data.frame(cbind(sub0, q.adjusted.cms2))
  q.adj.cms2 = c(q.adj.cms2, q.adjusted.cms2) #the adjusted cms data to be added onto the vector for each event
  peakq.cms.concepts[i] = max(sub$q.adjusted.cms2) 
  diff.s = as.numeric(sub$time[2:length(sub$time)]) - as.numeric(sub$time[1:length(sub$time)-1]) #time difference in seconds
  totalq.traps.m3 = 0.5*(sub$q.adjusted.cms2[1:length(sub$q.adjusted.cms2)-1] + sub$q.adjusted.cms2[2:length(sub$q.adjusted.cms2)]) *  diff.s #all treated as trapezoids --> 0.5*(base1 + base2) * height --> m3/s *s --> m3
  totalq.sum.m3.concepts[i] = sum(totalq.traps.m3) #cubic meters
  totalq.mm.concepts[i] = totalq.sum.m3.concepts[i]/10650000*1000 #10650000 m3 drainage area 
  wy.event[i] = sub0$wy[1] #take the wy for that event date
  #to calculate the total event-wise sediment (tons)
  total.sed[i] = sub0$TOTALYLD[length(sub0$TOTALYLD)] -  sub0$TOTALYLD[1]              #the last sediment - the first sediment to get the total event wise sediment
  total.silt.ton[i] = sub0$SILTYLD[length(sub0$SILTYLD)] -  sub0$SILTYLD[1]
  total.sand.ton[i] = sub0$SANDYLD[length(sub0$SANDYLD)] -  sub0$SANDYLD[1]
  total.gravel.ton[i] = sub0$GRAVELYLD[length(sub0$GRAVELYLD)] -  sub0$GRAVELYLD[1]
  total.sed.add[i] = total.silt.ton[i]+total.sand.ton[i]+total.gravel.ton[i] #more realistic value by adding up total from each size class!
}

plot(date.time, q.adj.cms2[2:length(q.adj.cms2)], type ="l", xlab = "Date", ylab = "Adjusted Discharge (cms)")

concepts.summary = data.frame(cbind(unique.date, totalq.sum.m3.concepts, totalq.mm.concepts, peakq.cms.concepts, wy.event, total.sed, total.silt.ton, total.sand.ton, total.gravel.ton, total.sed.add))

plot(peakq.cms.concepts, total.sed.add, xlab = "Peak Q (cms)", cex = 1.5, ylab = "Event Sediment Yield (tons)", type = "p")

#WY summaries!
total.sed.add.wy = data.frame(aggregate(total.sed.add, by=list(wy.event), FUN="sum"))
names(total.sed.add.wy)<- c("Water Year", "CONCEPTS Total Sediment (tons)")

silt.agg0 = aggregate(total.silt.ton, by=list(wy.event), FUN="sum")
sand.agg0 = aggregate(total.sand.ton, by=list(wy.event), FUN="sum")
gravel.agg0 = aggregate(total.gravel.ton, by=list(wy.event), FUN="sum")
total.summary.CONCEPTS.2002.2012 = data.frame(cbind(total.sed.add.wy, silt.agg0$x, sand.agg0$x, gravel.agg0$x))
names(total.summary.CONCEPTS.2002.2012) <- c("Water Year", "CONCEPTS Total Sediment (tons)","Silt (tons)", "Sand (tons)", "Gravel (tons)")

#reorganize table for table 2.2. calibration report
new.column = NA
WY = NA
sed.size = NA

for (i in 1:7){
  sub = total.summary.CONCEPTS.2002.2012[i,] #ith row
  WY0 = rep(sub[,1], 5)
  sed.size0 = c("total.gravel", "total.sand", "total.silt", "total.clay","total")
  new.column = c(new.column, sub[,5], sub[,4], sub[,3], "", sub[,2])
  WY = c(WY, WY0)
  sed.size = c(sed.size, sed.size0)
}
table.2.2 = data.frame(cbind(WY, sed.size, new.column))
write.csv(table.2.2, file = "F:/TJ/Validation_data/Google_drive/calibration_table2.2_CONCEPTS_summary_unlimSSC.csv")



####################################################################################################################################################################
#AGNPS SED SUMMARY!
cell = read.fwf(file="C:/Users/Kris/Documents/GitHub/Dissertation/AGNPS/Napo_4_4_16/cc/AGNPS_files_baseflow_eddy_5_16_16_sedlim15000_1/sed_cell_analysis.txt", widths = c(6,1,10,9,10,10,10,10,10,10,10,10,10,10)) #gives you how much sed from each cell for each event date
trib = read.fwf(file="C:/Users/Kris/Documents/GitHub/Dissertation/AGNPS/Napo_4_4_16/cc/AGNPS_files_baseflow_eddy_5_16_16_sedlim15000_1/sed_trib_analysis.txt", widths = c(7,10,9,10,10,10,10,10,10,10,10,10,10)) #q (Mg), clay, silt, sand, total (tons), etc.

reach.data = read.csv("F:/TJ/R/TJ/data/AGNPS_reach_optimized_UNIQUE_02282016.csv",header=TRUE,sep=",")
lat.data = read.csv("F:/TJ/R/TJ/data/AGNPS_latcell_list_model_02292016.csv",header=TRUE,sep=",")

#need to run loop to designate which model each reach-cell belongs to
#trib loop
model.list = NA
for (i in 1: length(trib[,1])){  
  sub = trib[i,]
  sub.reach.data = reach.data[reach.data$Reach_ID == sub[,1],]
  model = as.character(sub.reach.data$Reach)
  model.list = c(model.list, model)
}

model.list2 = model.list[2:length(model.list)]
trib2 = cbind(trib, model.list2)

#latcell loop
model.list = NA
for (i in 1: length(cell[,1])){  
  sub = cell[i,]
  sub.cell.data = lat.data[lat.data$Cell_ID == sub[,1],]
  model = as.character(sub.cell.data[1,2])
  model.list = c(model.list, model) 
}
model.list2 = model.list[2:length(model.list)]
cell2 = cbind(cell, model.list2) #has column with which model it's in

#WY Loop for trib2 data
date = strptime(trib2[,2],"%m/%d/%Y")
month = as.numeric(format(date, "%m"))
day = format(date, "%d")
year = format(date, "%Y")
trib3 = cbind(trib2, date, month, day, year)
source("F:/TJ/R/TJ/events_report/Rfiles/function_water_year.R")
wy1 = water.year(month, year) #calculate wy
trib4 = cbind(trib3, wy1)

#WY Loop for cell2 data
date = strptime(cell2[,3],"%m/%d/%Y")
month = as.numeric(format(date, "%m"))
day = format(date, "%d")
year = format(date, "%Y")
cell3 = cbind(cell2, date, month, day, year)
source("F:/TJ/R/TJ/events_report/Rfiles/function_water_year.R")
wy1 = water.year(month, year) #calculate wy
cell4 = cbind(cell3, wy1)

#trib4 and cell4 are the vectors to use that have reach model and water year

############NOW AGGREGATE or SUMMARIZE BY WATER YEAR 
#MAIN TOTALS
main.tribs = trib4[trib4$model.list2 =="Main",]
main.lat = cell4[cell4$model.list2 =="Main",]
agtrib.main = aggregate(main.tribs$V8,by=list(main.tribs$wy1), FUN = sum) 
agcell.main = aggregate(main.lat$V9,by=list(main.lat$wy1), FUN = sum) #V9 is the sum of clay, silt, sand for cell
totalMAIN = agtrib.main[,2] +agcell.main[,2]
WY = agtrib.main[,1]
#aggregate clay (tons)
agtrib.clay.main = aggregate(main.tribs$V5,by=list(main.tribs$wy1), FUN = sum) 
agcell.clay.main = aggregate(main.lat$V6,by=list(main.lat$wy1), FUN = sum)
total.clay.main = agtrib.clay.main[,2] +agcell.clay.main[,2]
#aggregate silt (tons)
agtrib.silt.main = aggregate(main.tribs$V6,by=list(main.tribs$wy1), FUN = sum) 
agcell.silt.main = aggregate(main.lat$V7,by=list(main.lat$wy1), FUN = sum)
total.silt.main = agtrib.silt.main[,2] +agcell.silt.main[,2]
#aggregate sand (tons)
agtrib.sand.main = aggregate(main.tribs$V7,by=list(main.tribs$wy1), FUN = sum) 
agcell.sand.main = aggregate(main.lat$V8,by=list(main.lat$wy1), FUN = sum)
total.sand.main = agtrib.sand.main[,2] +agcell.sand.main[,2]
main.summary = cbind(WY, totalMAIN, total.clay.main, total.silt.main, total.sand.main)

#SE TOTALS
SE.tribs = trib4[trib4$model.list2 =="SE",]
SE.lat = cell4[cell4$model.list2 =="SE",]
agtrib.SE = aggregate(SE.tribs$V8,by=list(SE.tribs$wy1), FUN = sum)
agcell.SE = aggregate(SE.lat$V9,by=list(SE.lat$wy1), FUN = sum)
totalSE = agtrib.SE[,2] +agcell.SE[,2]
WY = agtrib.SE[,1]
#aggregate clay (tons)
agtrib.clay.SE = aggregate(SE.tribs$V5,by=list(SE.tribs$wy1), FUN = sum) 
agcell.clay.SE = aggregate(SE.lat$V6,by=list(SE.lat$wy1), FUN = sum)
total.clay.SE = agtrib.clay.SE[,2] +agcell.clay.SE[,2]
#aggregate silt (tons)
agtrib.silt.SE = aggregate(SE.tribs$V6,by=list(SE.tribs$wy1), FUN = sum) 
agcell.silt.SE = aggregate(SE.lat$V7,by=list(SE.lat$wy1), FUN = sum)
total.silt.SE = agtrib.silt.SE[,2] +agcell.silt.SE[,2]
#aggregate sand (tons)
agtrib.sand.SE = aggregate(SE.tribs$V7,by=list(SE.tribs$wy1), FUN = sum) 
agcell.sand.SE = aggregate(SE.lat$V8,by=list(SE.lat$wy1), FUN = sum)
total.sand.SE = agtrib.sand.SE[,2] +agcell.sand.SE[,2]
SE.summary = cbind(WY, totalSE, total.clay.SE, total.silt.SE, total.sand.SE)

#SW TOTALS
SW.tribs = trib4[trib4$model.list2 =="SW",]
SW.lat = cell4[cell4$model.list2 =="SW",]
agtrib.SW = aggregate(SW.tribs$V8,by=list(SW.tribs$wy1), FUN = sum)
agcell.SW = aggregate(SW.lat$V9,by=list(SW.lat$wy1), FUN = sum)
totalSW = agtrib.SW[,2] +agcell.SW[,2]
WY = agtrib.SW[,1]
#aggregate clay (tons)
agtrib.clay.SW = aggregate(SW.tribs$V5,by=list(SW.tribs$wy1), FUN = sum) 
agcell.clay.SW = aggregate(SW.lat$V6,by=list(SW.lat$wy1), FUN = sum)
total.clay.SW = agtrib.clay.SW[,2] +agcell.clay.SW[,2]
#aggregate silt (tons)
agtrib.silt.SW = aggregate(SW.tribs$V6,by=list(SW.tribs$wy1), FUN = sum) 
agcell.silt.SW = aggregate(SW.lat$V7,by=list(SW.lat$wy1), FUN = sum)
total.silt.SW = agtrib.silt.SW[,2] +agcell.silt.SW[,2]
#aggregate sand (tons)
agtrib.sand.SW = aggregate(SW.tribs$V7,by=list(SW.tribs$wy1), FUN = sum) 
agcell.sand.SW = aggregate(SW.lat$V8,by=list(SW.lat$wy1), FUN = sum)
total.sand.SW = agtrib.sand.SW[,2] +agcell.sand.SW[,2]
SW.summary = cbind(WY, totalSW, total.clay.SW, total.silt.SW, total.sand.SW)


#####AGGREGATE FOR WHOLE MODEL:
total.all = totalSW + totalSE + totalMAIN
total.clay = total.clay.SW + total.clay.SE + total.clay.main
total.silt = total.silt.SW + total.silt.SE + total.silt.main
total.sand = total.sand.SW + total.sand.SE + total.sand.main

total.summary = cbind(WY, total.all, total.clay, total.silt, total.sand)

total.summary.agnps.2002.2012 = total.summary[2:8,]

total.summary.CONCEPTS.2002.2012

channel.total = total.summary.CONCEPTS.2002.2012[,2] - total.summary.agnps.2002.2012[,2]







####################################################################################################################################################################
#AGNPS SUMMARY
setwd("C:/Users/Kris/Documents/GitHub/Dissertation/AGNPS/Napo_4_4_16/cc")

#FOR THE AGNPS REACHES - Main outlet Summary
data = readLines("AnnAGNPS_TBL_Gaging_Station_Data_Evt.txt")#the storm event table for tribs

ind0 = grep("W/S Outlet", data) #lines that have w/s outlet (second w/s outlet is the first event date)
#storm event dates
ind.dates = ind0[2:length(ind0)] #lines with the dates of each storm
uniq.dates.ln = data[ind.dates] #strings from those lines
foo01 = gsub("^ *|(?<= ) | *$", "", uniq.dates.ln, perl = TRUE)  #take out white space and replace with one space
foo1 = strsplit(foo01, split = " ") #break up the character string 
dates0 = sapply(foo1, "[", 1) #takes the first element from the broken up lines (dates)
ppt.mm = sapply(foo1, "[", 5) #take the fifth element from borken lines (ppt)
total.q.Mg0 = sapply(foo1, "[", 11) #takes the total q from each line
peak.q.cms.agnps0 = sapply(foo1, "[", 10) #takes the peak q cms from each line
dates.unique = dates0[1:length(dates0)-1] #excludes the last value which is the "Avg. Ann. W/S Outlet [mass/yr]"
ppt.mm = ppt.mm[1:length(ppt.mm)-1]
total.q.Mg.agnps = total.q.Mg0[1:length(total.q.Mg0)-1] #1 Mg = 1 m3
total.q.mm.agnps = as.numeric(total.q.Mg.agnps)/10650000*1000 #units: m3/m2 * (1000 mm/1m)--> drainage area is 10650000 m2 to mm
peak.q.cms.agnps = peak.q.cms.agnps0[1:length(peak.q.cms.agnps0)-1]

#agnps summary table at the outlet
agnps.summary = data.frame(cbind(dates.unique, ppt.mm, total.q.Mg.agnps, total.q.mm.agnps, peak.q.cms.agnps))
####################################################################################################################################################################
#Loop CONCEPTS-AGNPS and Observed Comparison of peak and total q

#read in the observed data summary table!
data1 = read.csv("F:/TJ/Validation_data/Google_drive/processed_data/20140301_observed_q_summary.csv", header=TRUE)
data2 = read.csv("F:/TJ/Validation_data/Google_drive/processed_data/20150301_observed_q_summary.csv", header=TRUE)
data3 = read.csv("F:/TJ/Validation_data/Google_drive/processed_data/20150515_observed_q_summary.csv", header=TRUE)
data4 = read.csv("F:/TJ/Validation_data/Google_drive/processed_data/20150915_observed_q_summary.csv", header=TRUE)
data5 = read.csv("F:/TJ/Validation_data/Google_drive/processed_data/20160105_observed_q_summary.csv", header=TRUE)
obs.data0 = rbind(data1, data2, data3, data4, data5)
date2 = paste(substr(as.character(obs.data0$date), 6, 7), "/", substr(as.character(obs.data0$date), 9, 10), "/", substr(as.character(obs.data0$date), 1, 4), sep="")
obs.data = cbind(obs.data0, date2)

storms = c("02/28/2014","03/01/2014","03/01/2015","03/02/2015","05/15/2015","09/15/2015","01/05/2016")

validation.table = data.frame(matrix(nrow=1,ncol=8))
names(validation.table) = c("Date", "ppt.mm", "obs.peak.cms", "agnps.peak.cms", "concepts.peak.cms", "obs.total.mm", "agnps.total.mm", "concepts.total.mm")

for (i in 1: length(storms)){
  sub.concepts = concepts.summary[as.character(concepts.summary$unique.date)==storms[i],]
  sub.agnps = agnps.summary[as.character(agnps.summary$dates.unique)==storms[i],]
  sub.obs = obs.data[as.character(obs.data$date2)==storms[i],]
  line = c(as.character(sub.concepts$unique.date), as.character(sub.agnps$ppt.mm), as.character(sub.obs$peak.q.obs.cms), as.character(sub.agnps$peak.q.cms.agnps),  as.character(sub.concepts$peakq.cms.concepts), as.character(sub.obs$total.q.obs.mm), as.character(sub.agnps$total.q.mm.agnps),as.character(sub.concepts$totalq.mm.concepts))
  validation.table = rbind(validation.table,line)
}

validation.table2 = validation.table[2:length(validation.table$Date),]

library(htmlTable)

table.round0 = validation.table2[,2:8]
table.round = txtRound(table.round0, 3)

validation.table3 = as.data.frame(cbind(as.character(validation.table2$Date),table.round))
names(validation.table3) <- c("Date","Precip (mm)", "Observed Peak Q (cms)", "AGNPS Peak Q (cms)", "CONCEPTS Peak Q (cms)", "Observed Total Q (mm)", "AGNPS Total Q (mm)", "CONCEPTS Total Q (mm)")

validation.table.html = htmlTable(validation.table3, rnames=FALSE) #dont' know why the dates don't show up correctly



#Rainfall/Runoff Plot
#total q (mm)
plot(validation.table2$ppt.mm, validation.table2$agnps.total.mm, col = "red", cex = 1.2, xlab = "Rainfall (mm)", ylab = "Runoff (mm)")
points(validation.table2$ppt.mm, validation.table2$obs.total.mm, pch=16, cex = 1.2)
points(validation.table2$ppt.mm, validation.table2$concepts.total.mm, col = "blue", pch =2 , cex = 1.2 )
legend("topleft", c("Observed","AGNPS", "CONCEPTS"),bty = "n", pch=c(16,1,2), col = c("black","red","blue"), cex = 0.75,pt.cex = 1.2)
#Peak q
plot(validation.table2$ppt.mm, validation.table2$obs.peak.cms, pch=16, cex = 1.2, xlab = "Rainfall (mm)", ylab = "Peak Discharge (cms)")
points(validation.table2$ppt.mm, validation.table2$agnps.peak.cms, col = "red", cex = 1.2)
points(validation.table2$ppt.mm, validation.table2$concepts.peak.cms, col = "blue", pch =2 , cex = 1.2 )
legend("topleft", c("Observed","AGNPS", "CONCEPTS"),bty = "n", pch=c(16,1,2), col = c("black","red","blue"), cex = 0.75,pt.cex = 1.2)

