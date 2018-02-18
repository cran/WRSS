createRiver.base <-
function(name ,label,downstream,seepageFraction,seepageCode,discharge,priority)
{
   river<-list(name=name,
               label=label,
               downstream=downstream,
               seepageFraction=seepageFraction,
               seepageCode=seepageCode,
               discharge=discharge,
               priority=priority)

   return(river)
}
