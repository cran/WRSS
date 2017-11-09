createDiversion.base <-
function(name,label,capacity,divertTo, downstream,priority)
{
   diversion<-list(name=name,
                  label=label,
                  capacity=capacity,
                  divertTo=divertTo,
                  downstream=downstream,
                  priority=priority)
   return(diversion)
}
