#function calculate Q after you have the stage.m

#Manning's to calculate Q for given stages; uses xs geometry at PT_outlet to get hydraulic radius
  #where the top width and bottom width may vary for larger depths (because trapezoidal xsa)
calculateQ <- function(stage.m) {

geom.data0 = read.csv("F:/TJ/Validation_data/Google_drive/PT_xs_188.6771.csv",header=TRUE,sep=",") #cross section distance and elevation for PT xs
stage.m.xs = geom.data0$elevation - min(geom.data0$elevation) #to get the relative elevation to the bed elevation (or the depth of the water instead of absolution elevation)
geom.data = cbind(geom.data0, stage.m.xs)

#check to see if stage in observed data is larger than bankfull stage (overbank, need diff calculation of q)
overbank.check = min(2.103 - stage.m) #bankfull depth is 2.103 m minus observed stage (if negative, then overbank)

q.cms = 0
top.width = 0
bottom.width = 0

for (i in 1: length(stage.m)) {
  
  #bottom width of xs at PT (trapezoid so this is the first base)
  bottom.width[i] = as.numeric(as.character(geom.data[geom.data$notes=="Rtoe",]$station)) - as.numeric(as.character(geom.data[geom.data$notes=="Ltoe",]$station))
  #get the distance x of the depth that we're using
  sub.L = geom.data[1:which(geom.data$notes =="Ltoe"),] #take all pts on the L side of channel
  sub.R = geom.data[which(geom.data$notes =="Rtoe"):length(geom.data$station),]
  approx.L = approx(sub.L$stage.m.xs, sub.L$station, stage.m[i]) #want to find distance on L for that stage
  approx.R = approx(sub.R$stage.m.xs, sub.R$station, stage.m[i]) #want to find distance on R for that stage
  top.width[i] = approx.R$y - approx.L$y #top width of the water
  area.m2 = 0.5*(top.width[i] + bottom.width[i])*stage.m[i] #wetted area as trapezoid
  #wetted perimeter
  dist.L = c(approx.L$y, sub.L$station[which(sub.L$notes =="Ltoe")]) #to get distance from L water pt to L toe
  stage.L = c(approx.L$x, sub.L$stage.m.xs[which(sub.L$notes =="Ltoe")]) 
  dist.R = c(sub.R$station[which(sub.R$notes =="Rtoe")], approx.R$y ) #to get distance from R toe to R water pt
  stage.R = c(sub.R$stage.m.xs[which(sub.R$notes =="Rtoe")], approx.R$x)
  L.sub = as.matrix(cbind(dist.L, stage.L))
  R.sub = as.matrix(cbind(dist.R, stage.R))
  dist.L2 = dist(L.sub) 
  dist.R2 = dist(R.sub)
  wetted.perim = dist.L2 + bottom.width[i] + dist.R2
  hyd.radius = area.m2/wetted.perim #R
  
  s = 0.011    #slope
  n = 0.02     #manning's n of 0.02
  v = (hyd.radius^(2/3)* s^(1/2))/n #manning's equation
  q.cms[i] = v*area.m2 }
  q.cms #the last line is what is returned from this function
}



