#AGNPS Summary at the Main outlet based on AnnAGNPS_TBL_Gaging_Station_Data_Evt.txt

####################################################################################################################################################################
#FOR THE AGNPS REACHES - Main outlet Summary
setwd("C:/Users/Kris/Documents/GitHub/Dissertation/AGNPS/Napo_4_4_16/cc")
data = readLines("AnnAGNPS_TBL_Gaging_Station_Data_Evt.txt")#the AGNPS storm event table for tribs and the outlet

ind0 = grep("W/S Outlet", data) #lines that have w/s outlet (second w/s outlet is the first event date)
#storm event dates
ind.dates = ind0[2:length(ind0)] #lines with the dates of each storm starting at the second one...first one is overall summary that we don't need
uniq.dates.ln = data[ind.dates] #strings from those lines (all the content from each of those rows...summarizing the outlet)
foo01 = gsub("^ *|(?<= ) | *$", "", uniq.dates.ln, perl = TRUE)  #take out white space and replace with one space
foo1 = strsplit(foo01, split = " ") #break up the character string 
dates0 = sapply(foo1, "[", 1) #takes the first element from the broken up lines (dates)
ppt.mm0 = sapply(foo1, "[", 5) #take the fifth element from borken lines (ppt)
total.q.Mg0 = sapply(foo1, "[", 11) #takes the total q from each line
peak.q.cms.agnps0 = sapply(foo1, "[", 10) #takes the peak q cms from each line
total.clay.Mg0 = sapply(foo1, "[", 38) #takes the total clay (suspended sediment all) in Mg from each line
total.silt.Mg0 = sapply(foo1, "[", 39) #takes the total silt (suspended sediment all) in Mg from each line
total.sand.Mg0 = sapply(foo1, "[", 40) #takes the total sand (suspended sediment all) in Mg from each line
total.sus.sed.Mg0 = sapply(foo1, "[", 43) #takes the total suspended sediment all in Mg from each line
  
dates.unique = dates0[1:length(dates0)-1] #excludes the last value which is the "Avg. Ann. W/S Outlet [mass/yr]"
ppt.mm = ppt.mm0[1:length(ppt.mm0)-1]
total.q.Mg.agnps = total.q.Mg0[1:length(total.q.Mg0)-1] #1 Mg = 1 m3
total.q.mm.agnps = as.numeric(total.q.Mg.agnps)/10650000*1000 #units: m3/m2 * (1000 mm/1m)--> drainage area is 10650000 m2 to mm
peak.q.cms.agnps = peak.q.cms.agnps0[1:length(peak.q.cms.agnps0)-1]
total.clay.Mg = total.clay.Mg0[1:length(total.clay.Mg0)-1]
total.silt.Mg = total.silt.Mg0[1:length(total.clay.Mg0)-1]
total.sand.Mg = total.sand.Mg0[1:length(total.clay.Mg0)-1]
total.sus.sed.Mg = total.sus.sed.Mg0[1:length(total.sus.sed.Mg0)-1]
#Water Year
date = strptime(dates.unique,"%m/%d/%Y")
month = as.numeric(format(date, "%m"))
day = format(date, "%d")
year = format(date, "%Y")
source("F:/TJ/R/TJ/events_report/Rfiles/function_water_year.R")
wy = water.year(month, year) #calculate wy

#agnps EVENT summary table at the outlet
agnps.event.summary = data.frame(cbind(dates.unique, ppt.mm, total.q.Mg.agnps, total.q.mm.agnps, peak.q.cms.agnps, total.clay.Mg, total.silt.Mg, total.sand.Mg, total.sus.sed.Mg, wy))

#Summary table by water year (aggregate by WY)
clay.total.agg.Mg = aggregate(as.numeric(as.character(total.clay.Mg)) ,by=list(wy), FUN = sum)
silt.total.agg.Mg = aggregate(as.numeric(as.character(total.silt.Mg)) ,by=list(wy), FUN = sum)
sand.total.agg.Mg = aggregate(as.numeric(as.character(total.sand.Mg)) ,by=list(wy), FUN = sum)
total.sed.agg.Mg = aggregate(as.numeric(as.character(total.sus.sed.Mg)) ,by=list(wy), FUN = sum)
agnps.wy.summary = data.frame(cbind(sand.total.agg.Mg[,1], sand.total.agg.Mg[,2], silt.total.agg.Mg[,2], clay.total.agg.Mg[,2], total.sed.agg.Mg[,2] ))
names(agnps.wy.summary) <-  c("wy", "sand.total.agg.Mg", "silt.total.agg.Mg", "clay.total.agg.Mg", "total.sed.agg.Mg")

#reformat the table for 2006-2012 summary by sed size table 2.2 AGNPS portion at outlet in calibration report
total.summary.agnps.2002.2012 = data.frame(agnps.wy.summary[2:8,]) #subset of table for 2002-2012
wy.unique = unique(total.summary.agnps.2002.2012$wy) #list of unique wy

new.column = NA
WY = NA
sed.size = NA

for (i in 1:length(wy.unique)){
  sub = total.summary.agnps.2002.2012[i,] #ith row
  WY0 = rep(wy.unique[i], 5)
  sed.size0 = c("total.gravel","total.sand", "total.silt", "total.clay","total")
  new.column = c(new.column, "-",sub$sand.total.agg.Mg, sub$silt.total.agg.Mg, sub$clay.total.agg.Mg, sub$total.sed.agg.Mg)
  WY = c(WY, WY0)
  sed.size = c(sed.size, sed.size0)
}

library(htmlTable)
round0 = data.frame(cbind(as.numeric(as.character(new.column)), as.numeric(as.character(new.column)))) #in order to round, must be dataframe, create fake dataframe to round
round = txtRound(round0,0) #just to round the new column
table.2.2.agnps = data.frame(cbind(WY, sed.size, round$X2))

write.csv(table.2.2.agnps, file = "F:/TJ/Validation_data/Google_drive/calibration_table2.2_agnps_summary_gagingtbl.csv")

