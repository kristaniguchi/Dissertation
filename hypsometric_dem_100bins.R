#hypsometric Curve to plot elevation on x-axis, and cumulative fraction of watershed area on y-axis with red curve
  #arrows showing elevation of HOR and GC gages

#read in clipped DEM to wtshd boundary
install.packages('raster')
install.packages('rgdal')
library(raster)
library(sp)
library(rgdal)
setwd("F:/TJ/TJ_Database/burn_dem_files/dem_burn_final_8_8_14")  
grd <- readGDAL("F:/TJ/TJ_Database/burn_dem_files/dem_burn_final_8_8_14/demburn_clp.ovr")
grd2 <- raster("F:/TJ/TJ_Database/burn_dem_files/dem_burn_final_8_8_14/demburn_clp.ovr")
grd2 <- setMinMax(grd2)
crs(grd2) <- "+proj=utm +zone=11N  +datum=NAD83 +units=m +no_defs" #+ellps=GRS80


#Step1: Reclassify clipped DEm into 100 elevation bins
#source("F:/TJ/R/TJ/events_report/Rfiles")
#function for hypsometric curve for watershed: reclassify DEM into 100 elevation bins (DEM must be clipped to watershed)
WESSO<-function(DGM,Bin){
  min<-trunc(minValue(DGM))
  max<-ceiling(maxValue(DGM))
  DIFF<-max-min
  class<-DIFF/Bin
  break_v<-matrix(seq(min+class,max,class))
  low<-cbind(seq(min,max-class,class))
  numb<-seq(1,Bin,1)
  rclmat<-cbind(low,break_v,numb)
  rc<- reclassify(DGM, rclmat,include.lowest=T)
  list<-list(a=rclmat,b=rc)
  raster<-list$b
  rclmat<-list$a
  count<-freq(raster,useNA='no')
  count_2<-hist(raster,breaks=Bin)
  count_2<-c(count_2$counts)
  count_2[1]
  DIFFA<-as.numeric(count_2[1]-count[1,2])
  laenge<-as.numeric(length(count[,2]))
  count_3<-count[3:laenge,2]
  DIFFA_2<-as.numeric(count[1,2])
  as<-rbind(DIFFA_2,DIFFA)
  nn<-matrix(rbind(count_2[2:length(count_2)]))
  end<-rbind(as,nn)
  end<-matrix(end)
  info<-cbind(rclmat,end)
  return(list(a=raster,b=info))}


WESSO(grd2, 100)


#read in raster bin elevation data from  arcmap
data = read.csv("F:/TJ/R/TJ/events_report/Data/hypsometric_curve_pixelcountbins.csv")

par(mar = c(5, 5,1,1))
plot(data$TO_elevm, data$cum_area_km2, type="p", xlab = "Elevation (m)", ylab = expression("Cumulative Area "~ (km^{2})))
arrows(27.457, 4, 27.457, 2, length = 0.1, code = 2, col = par("fg"), lty = par("lty"),lwd = par("lwd"))
arrows(174.283707, 10, 174.283707, 8, length = 0.1, code = 2, col = par("fg"), lty = par("lty"),lwd = par("lwd"))
legend("topleft", "RG.GC", bty="n", cex=1, inset=c(-.05,.52)) #inset L-R, up-down
legend("topleft", "RG.HOR", bty="n", cex=1, inset=c(.41,.045)) #inset L-R, up-down

#cum. percent of the watershed area
cum.freq.wtshd = data$cum_area_km2/(max(data$cum_area_km2))
plot(data$TO_elevm, cum.freq.wtshd, type="p", xlab = "Elevation (m)", ylab = "Cumulative fraction of the watershed area")
arrows(27.457, .2, 27.457, .06, length = 0.1, code = 2, col = par("fg"), lty = par("lty"),lwd = par("lwd"))
arrows(174.283707, .6, 174.283707, .48, length = 0.1, code = 2, col = par("fg"), lty = par("lty"),lwd = par("lwd"))
legend("topleft", "GC", bty="n", cex=1, inset=c(-.028,.63)) #inset L-R, up-down
legend("topleft", "HOR", bty="n", cex=1, inset=c(.43,.26)) #inset L-R, up-down

#cum % of area above HOR and below HOR
below.hor = 4.960737/(max(data$cum_area_km2))
above.hor = 1-below.hor
