#Calibration Report Summary Table 2.2
#########################################################################################################
#########################################################################################################
#TRAP EFFICIENCY VALUES:

#Trap efficiency for each size class depending on the storm event and year
setwd("F:/TJ/Validation_data/Google_drive/trap_efficiency")
particle.data = read.csv(file="particle_size_fractions_upperbasin.csv")
trap.data = read.csv("sed_loadings_trap_excavation_annual.csv")

#Fall velocity for each particle size class
#m. sand, f. sand, silt, clay
density.water = 1000
visc.water = 0.00131
fall.vel.m.s = (((1636*(particle.data$particle_density_kg_m3-density.water)*particle.data$mean_diam_m^3+9*visc.water^2)^0.5)-3*visc.water)/(500*particle.data$mean_diam_m)

########################################################################################################################
#Critical Velocity of sed basin #this will be calculated for every day Vc = Qi/A
main = read.table("C:/Users/Kris/Documents/GitHub/Dissertation/AGNPS/Napo_4_4_16/cc/AGNPS_files_baseflow_eddy_5_16_16_sedlim15000_1/Main_sed_08092016_20062012/TimeSeries_01.txt",skip = 9,header = FALSE) #update this to concepts timeseries output at outlet!
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
main2 = cbind(main, date.time, month, month.day, year, month.day.year, time)

#add up all the baseflows and subtract that amt from the Q
setwd("C:/Users/Kris/Documents/GitHub/Dissertation/AGNPS/Napo_4_4_16/trib_files_baseflow_updated_5_16_2016") #edit this line
all  = list.files("C:/Users/Kris/Documents/GitHub/Dissertation/AGNPS/Napo_4_4_16/trib_files_baseflow_updated_5_16_2016")
fname = NA
baseflowtxt = NA

for (i in 1: length(all)) {
  z = readLines(all[i])
  fname[i] = all[i]
  baseflowtxt[i] = z[3]  
}
baseflow.sum = sum(as.numeric(baseflowtxt))  #0.133
adj.q.cms = main2$DISCHARGE.cms - baseflow.sum #the corrected discharge
adj.q.cms2 = adj.q.cms-0 #don't need extra adjust
adj.q.cms2[adj.q.cms2<0] <- 0 #set all negative values to 0
q.data.adj = cbind(main2, adj.q.cms2)

#loop to get the Qi for each day and to calc Vc for each day, and Trap efficiency for each day for each sed class
#set values to zero before loop, will get replaced within loop
A = 560*95  #the planform area of the sed trap (560m length x 95m width)
Qi = 0 #this is daily mean discharge
Vc = 0 #critical velocity of the sed basin
Ei.msand1 = 0 #trap efficiency for medium sand using n=1 (poor settling)
Ei.fsand1 = 0 #trap efficiency for fine sand
Ei.silt1 = 0 
Ei.clay1 = 0

Ei.msand3 = 0 #trap efficiency for medium sand for n=3 (good settling)
Ei.fsand3 = 0 #trap efficiency for fine sand
Ei.silt3 = 0 
Ei.clay3 = 0

uniq.day = unique(q.data.adj$DATE)
for (i in 1: length(uniq.day)) {
  sub = q.data.adj[q.data.adj$DATE == uniq.day[i],]
  sub.values = sub$adj.q.cms[sub$adj.q.cms>0] #take the average flow for the day, so this takes out all the zeros
  Qi[i] = mean(sub.values)
  Vc[i] = Qi[i]/A
  #for n=1, poor settling conditions
  n=1
  Ei.msand1[i] = 1-(1+(1/n)*(fall.vel.m.s[1]/Vc[i]))^-n
  Ei.fsand1[i] = 1-(1+(1/n)*(fall.vel.m.s[2]/Vc[i]))^-n
  Ei.silt1[i] = 1-(1+(1/n)*(fall.vel.m.s[3]/Vc[i]))^-n
  Ei.clay1[i] = 1-(1+(1/n)*(fall.vel.m.s[4]/Vc[i]))^-n
  #for n=3, good settling conditions
  n=3
  Ei.msand3[i] = 1-(1+(1/n)*(fall.vel.m.s[1]/Vc[i]))^-n
  Ei.fsand3[i] = 1-(1+(1/n)*(fall.vel.m.s[2]/Vc[i]))^-n
  Ei.silt3[i] = 1-(1+(1/n)*(fall.vel.m.s[3]/Vc[i]))^-n
  Ei.clay3[i] = 1-(1+(1/n)*(fall.vel.m.s[4]/Vc[i]))^-n
  
}

day = strptime(as.character(uniq.day),"%m/%d/%Y") #the day that matches with the Ei vectors
year = as.numeric(format(day, "%Y"))
month = as.numeric(format(day, "%m"))
source("F:/TJ/R/TJ/events_report/Rfiles/function_water_year.R")
water.year = water.year(month,year) 
check = cbind(month, year,water.year) #to check if water year is correct
data.Ei = data.frame(cbind(water.year, Ei.msand1, Ei.fsand1, Ei.silt1, Ei.clay1, Ei.msand3, Ei.fsand3, Ei.silt3, Ei.clay3, Qi))

uniq.wyear = unique(water.year) #water year
#set to zero outside loop, replace within loop
Eann.msand1 =  0
Eann.fsand1 = 0
Eann.silt1 = 0
Eann.clay1 = 0
Eann.msand3 =  0
Eann.fsand3 = 0
Eann.silt3 = 0
Eann.clay3 = 0

for (i in 1: length(uniq.wyear)) {
  sub.year = data.Ei[data.Ei$water.year == uniq.wyear[i],]
  
  Eann.msand1[i] =  sum(as.numeric(as.character(sub.year$Qi))*as.numeric(as.character(sub.year$Ei.msand1)),na.rm=TRUE)/sum(as.numeric(as.character(sub.year$Qi)), na.rm=TRUE)
  Eann.fsand1[i] = sum(as.numeric(as.character(sub.year$Qi))*as.numeric(as.character(sub.year$Ei.fsand1)),na.rm=TRUE)/sum(as.numeric(as.character(sub.year$Qi)), na.rm=TRUE)
  Eann.silt1[i] = sum(as.numeric(as.character(sub.year$Qi))*as.numeric(as.character(sub.year$Ei.silt1)),na.rm=TRUE)/sum(as.numeric(as.character(sub.year$Qi)), na.rm=TRUE)
  Eann.clay1[i] = sum(as.numeric(as.character(sub.year$Qi))*as.numeric(as.character(sub.year$Ei.clay1)),na.rm=TRUE)/sum(as.numeric(as.character(sub.year$Qi)), na.rm=TRUE)
  Eann.msand3[i] =  sum(as.numeric(as.character(sub.year$Qi))*as.numeric(as.character(sub.year$Ei.msand3)),na.rm=TRUE)/sum(as.numeric(as.character(sub.year$Qi)), na.rm=TRUE)
  Eann.fsand3[i] = sum(as.numeric(as.character(sub.year$Qi))*as.numeric(as.character(sub.year$Ei.fsand3)),na.rm=TRUE)/sum(as.numeric(as.character(sub.year$Qi)), na.rm=TRUE)
  Eann.silt3[i] = sum(as.numeric(as.character(sub.year$Qi))*as.numeric(as.character(sub.year$Ei.silt3)),na.rm=TRUE)/sum(as.numeric(as.character(sub.year$Qi)), na.rm=TRUE)
  Eann.clay3[i] = sum(as.numeric(as.character(sub.year$Qi))*as.numeric(as.character(sub.year$Ei.clay3)),na.rm=TRUE)/sum(as.numeric(as.character(sub.year$Qi)), na.rm=TRUE)  
}

Eann.data = data.frame(cbind(uniq.wyear, Eann.msand1, Eann.fsand1, Eann.silt1, Eann.clay1, Eann.msand3, Eann.fsand3, Eann.silt3, Eann.clay3))

#HTML Table!
library(htmlTable)
mytable = data.frame(matrix(nrow=4,ncol=5))
names(mytable) = c("Removal Date","Tons Removed","Eann n=1", "Eann n=3","Corrected Load (tons)")
rownames(mytable) = c("Medium Sand","Fine Sand", "Silt", "Clay")
mytableout = htmlTable (mytable, rgroup="2006", n.rgroup=4)
print(mytableout)

#loops to arrange the information into a table and calculate the mass removed based on %in each size class and mass remove adjusted by adding in what got released

Eann1 = NA
Eann3 = NA
mass.rem = NA
mass.rem.adj = NA
size.class = NA
wy = NA
for (i in 1:7) { #starting with 2006-2012 excavation, where complete excavation
  sub = Eann.data[i,]
  Eann1 = c(Eann1, sub$Eann.msand1,sub$Eann.fsand1,sub$Eann.silt1,sub$Eann.clay1,"","")
  Eann3 = c(Eann3, sub$Eann.msand3,sub$Eann.fsand3,sub$Eann.silt3,sub$Eann.clay3,"","")
  sub.trap = trap.data[trap.data$water_year_sed == uniq.wyear[i],]
  total.no.clay = as.numeric(as.character(sub.trap$Mass_remove_tons)) - as.numeric(as.character(sub.trap$Mass_remove_tons))*as.numeric(as.character(particle.data[4,2]))
  mass.rem = c(mass.rem, as.numeric(as.character(sub.trap$Mass_remove_tons))*as.numeric(as.character(particle.data[1,2])),as.numeric(as.character(sub.trap$Mass_remove_tons))*as.numeric(as.character(particle.data[2,2])),as.numeric(as.character(sub.trap$Mass_remove_tons))*as.numeric(as.character(particle.data[3,2])),as.numeric(as.character(sub.trap$Mass_remove_tons))*as.numeric(as.character(particle.data[4,2])),as.numeric(as.character(sub.trap$Mass_remove_tons)),total.no.clay)
  #calc mass removed adjusted
  mass.rem0 = c(as.numeric(as.character(sub.trap$Mass_remove_tons))*as.numeric(as.character(particle.data[1,2])),as.numeric(as.character(sub.trap$Mass_remove_tons))*as.numeric(as.character(particle.data[2,2])),as.numeric(as.character(sub.trap$Mass_remove_tons))*as.numeric(as.character(particle.data[3,2])),as.numeric(as.character(sub.trap$Mass_remove_tons))*as.numeric(as.character(particle.data[4,2])))
  Eann1.0 = c(sub$Eann.msand1,sub$Eann.fsand1,sub$Eann.silt1,sub$Eann.clay1)
  Eann3.0 = c(sub$Eann.msand3,sub$Eann.fsand3,sub$Eann.silt3,sub$Eann.clay3)
  mass.rem.adj0 = mass.rem0/Eann3.0  #Eann*total = trapped, total = trapped/eann
  sum.mass.rem.adj = sum(mass.rem.adj0)
  total.adj.no.clay = sum.mass.rem.adj- mass.rem.adj0[4]
  mass.rem.adj = c(mass.rem.adj, mass.rem.adj0, sum.mass.rem.adj, total.adj.no.clay)
  size.class = c(size.class,"Medium sand (a)","Fine sand (b)","Silt (c)","Clay (d)","Total","Total without Clay")
  wy0 = rep(sub$uniq.wyear, 6)
  wy = c(wy,wy0)
}

#round the tons removed to whole number, and the trap efficiency to 2 decimal places
tons.rem.round = txtRound(as.matrix(cbind(mass.rem[2:length(size.class)],mass.rem.adj[2:length(size.class)])),0)
Eann.round = txtRound(as.matrix(cbind( Eann1[2:length(size.class)], Eann3[2:length(size.class)])),2)
table = as.matrix(cbind(size.class[2:length(size.class)],tons.rem.round[,1], Eann.round[,1], Eann.round[,2], tons.rem.round[,2]))
#table0 = as.matrix(cbind(size.class[2:length(size.class)],mass.rem[2:length(size.class)], Eann1[2:length(size.class)], Eann3[2:length(size.class)], mass.rem.adj[2:length(size.class)]))
mytableout = htmlTable(table, 
                       rnames = c("","", "","","", "","","", "","","", "","","", "","","", "","","", "","","", "","","", "","","", "","","", "","","","", "","","", "","",""),
                       header = c("Removal Date","Tons Removed","Eann n=1", "Eann n=3","Corrected Load (tons)"), 
                       rgroup=c("2006","2007","2008","2009","2010","2011","2012"), 
                       n.rgroup=c(6,6,6,6,6,6,6),
                       caption="Table 4.2 Sediment removed from traps (Tons Removed), annual trap efficiency, and corrected sediment load from the watershed by size class.")
print(mytableout)
table.export = data.frame(cbind(wy[2:length(wy)],table))
names(table.export) <- c("Removal_WY_Date","Sed_size","Tons_removed_uncorrected","Eann_1", "Eann_3","Corrected_load_tons")

#aggregate the fine and medium sand into one for tons removed and corrected loads (tons)
unique.wy = unique(table.export$Removal_WY_Date)
Tons_removed_uncorrected_agg = NA
Corrected_load_tons_agg = NA
wy.rep = NA

for (i in 1: length(unique.wy)) {
  sub = table.export[table.export$Removal_WY_Date == unique.wy[i],]
  sand.uncor = as.numeric(as.character(sub$Tons_removed_uncorrected[1])) + as.numeric(as.character(sub$Tons_removed_uncorrected[2]))
  sand.cor = as.numeric(as.character(sub$Corrected_load_tons[1])) + as.numeric(as.character(sub$Corrected_load_tons[2]))
  Tons_removed_uncorrected_agg0 = c(sand.uncor, as.numeric(as.character(sub$Tons_removed_uncorrected[3:6])))
  Corrected_load_tons_agg0 = c(sand.cor, as.numeric(as.character(sub$Corrected_load_tons[3:6])))
  Tons_removed_uncorrected_agg = c(Tons_removed_uncorrected_agg, Tons_removed_uncorrected_agg0)
  Corrected_load_tons_agg = c(Corrected_load_tons_agg, Corrected_load_tons_agg0)
  wy.rep0 = rep(as.character(unique.wy[i]), 5)
  wy.rep = c(wy.rep, wy.rep0)
}

sed.size.agg = c("NA", rep(c("sand","Silt", "Clay", "Total", "Total without Clay"), 7))
summary.table.a = data.frame(cbind(wy.rep, sed.size.agg, Tons_removed_uncorrected_agg, Corrected_load_tons_agg ))
write.csv(summary.table.a, file="F:/TJ/Validation_data/Google_drive/trap_efficiency/summary.table.2002.2012.aggsand.csv")
#FOR table 2.2 in AGNPS/CONCEPTS Callibration report! --> just copied and pasted values into that table


#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#FOR AGNPS SEDIMENT SUMMARY BY WATER YEAR --> Draining to Channel
#to sum the sedmass_trib.txt  and sedmass_cell.txt files to get total from AGNPS

#setwd("C:/Users/Kris/Documents/GitHub/Dissertation/AGNPS/Napo_4_4_16/cc/AGNPS_files_baseflow_eddy_5_16_16")
setwd("C:/Users/Kris/Documents/GitHub/Dissertation/AGNPS/Napo_4_4_16/cc/AGNPS_files_no_sed_limit_07252016")
setwd("C:/Users/Kris/Documents/GitHub/Dissertation/AGNPS/Napo_4_4_16/cc/AGNPS_files_baseflow_eddy_5_16_16_sedlimit10_1")
setwd("C:/Users/Kris/Documents/GitHub/Dissertation/AGNPS/Napo_4_4_16/cc/AGNPS_files_baseflow_eddy_5_16_16_sedlim15000_1")

cell = read.fwf(file="sed_cell_analysis.txt", widths = c(6,1,10,9,10,10,10,10,10,10,10,10,10,10)) #gives you how much sed from each cell for each event date
trib = read.fwf(file="sed_trib_analysis.txt", widths = c(7,10,9,10,10,10,10,10,10,10,10,10,10)) #q (Mg), clay, silt, sand, total (tons), etc.

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

mean.ann = mean(total.all)

#reorder table for table 2.2. calibration report for 2006-2012 (sand, silt, clay, total)
total.summary.agnps.2002.2012 = data.frame(total.summary[2:8,]) #subset of table for 2002-2012
wy.unique = unique(total.summary.agnps.2002.2012$WY) #list of unique wy

new.column = NA
WY = NA
sed.size = NA

for (i in 1:length(wy.unique)){
  sub = total.summary.agnps.2002.2012[i,] #ith row
  WY0 = rep(wy.unique[i], 4)
  sed.size0 = c("total.sand", "total.silt", "total.clay","total")
  new.column = c(new.column, sub$total.sand, sub$total.silt, sub$total.clay, sub$total.all)
  WY = c(WY, WY0)
  sed.size = c(sed.size, sed.size0)
  
}

table.2.2.b = data.frame(cbind(WY, sed.size, new.column))
write.csv(table.2.2.b, file = "F:/TJ/Validation_data/Google_drive/calibration_table2.2_agnps_summary_unlimSSC.csv")

#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#FOR AGNPS AT OUTLET Summary:
#AGNPS Summary at the Main outlet based on AnnAGNPS_TBL_Gaging_Station_Data_Evt.txt

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

#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#FOR CONCEPTS VALUES
#To create subset of only the days that have flow in CONCEPTS timeseries/exclude no flow days

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

table.2.2.c = data.frame(cbind(WY, sed.size, new.column))
write.csv(table.2.2.c, file = "F:/TJ/Validation_data/Google_drive/calibration_table2.2_CONCEPTS_summary_unlimSSC.csv")
#########################################################################################################
#########################################################################################################




