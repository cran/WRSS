createRiver.base <-
function(name ,downstream,seepageFraction,seepageObject,discharge,priority,latlon)
{
   river<-list(name=name,
               label=runif(1),
               downstream=downstream,
               seepageFraction=seepageFraction,
               seepageObject=seepageObject,
               discharge=discharge,
               priority=priority,
               latlon=latlon)
   return(river)
}
