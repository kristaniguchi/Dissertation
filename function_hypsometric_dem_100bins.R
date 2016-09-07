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
  

  