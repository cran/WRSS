createDiversion.base <-
function(name,capacity,divertObject, downstream,priority,latlon)
{
   diversion<-list(name=name,
                  label=runif(1),
                  capacity=capacity,
                  divertObject=divertObject,
                  downstream=downstream,
                  priority=priority,
                  latlon=latlon)
   return(diversion)
}
