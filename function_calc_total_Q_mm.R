#function to calculate total q in mm for a given storm using drainage area at the outlet of GC
calculate.total.Q.mm <- function(q.cms, date.time) {
diff.s = as.numeric(date.time[2:length(date.time)]) - as.numeric(date.time[1:(length(date.time)-1)]) #time difference in seconds
totalq.traps.m3 = 0.5*(q.cms[1:length(q.cms)-1] + q.cms[2:length(q.cms)]) *  diff.s #all treated as trapezoids --> 0.5*(base1 + base2) * height --> m3/s *s --> m3
totalq.sum.m3 = sum(totalq.traps.m3, na.rm = TRUE) #cubic meters
totalq.mm = totalq.sum.m3/10231811.9*1000 #10.2318119 km2 = 10231811.9 m2 drainage area to mm at PT
}