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
  all  = list.files("C:/Users/Kris/Documents/GitHub/Dissertation/AGNPS/Napo_4_4_16/trib_files_baseflow_updated")
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
write.csv(table.export,file= "F:/TJ/Validation_data/Google_drive/trap_efficiency/sediment_loads_2006_2012_corrected_uncorrected_8_11_2016.csv")


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
summary.table = data.frame(cbind(wy.rep, sed.size.agg, Tons_removed_uncorrected_agg, Corrected_load_tons_agg ))
write.csv(summary.table, file="F:/TJ/Validation_data/Google_drive/trap_efficiency/summary.table.2002.2012.aggsand.csv")
  #FOR table 2.2 in AGNPS/CONCEPTS Callibration report! --> just copied and pasted values into that table
