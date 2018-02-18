createReservoir.base <-
function(name,label,priority,netEvaporation,downstream,initialStorage,seepageFraction,seepageCode,geometry)
{
   reservoir<-list(name=name,
                   label=label,
                   priority=priority,
                   netEvaporation=netEvaporation,
                   downstream=downstream,
                   initialStorage=initialStorage,
                   seepageFraction=seepageFraction,
                   seepageCode=seepageCode,
                   geometry=geometry)
      
   return(reservoir)
}
