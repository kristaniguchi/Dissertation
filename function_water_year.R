#function to get the water year when you have the month and year for that date
  #input data is month and year vectors
water.year <- function(month, year) {
  water_year = 0
  for (i in 1: length(month)){
  if(as.numeric(month[i])<=9) {
    water_year[i] = as.numeric(year[i])
  }else{water_year[i] = as.numeric(year[i])+1
  } 
    }
  water_year
}
