createDemandSite.base <-
function(name,label,demandTS,demandParams,returnFlowFraction,suppliers,downstream,priority)
{
   demand<-list(name=name,
                label=label,
                demandTS=demandTS,
                demandParams=demandParams,
                returnFlowFraction=returnFlowFraction,
                suppliers=suppliers,
                downstream=downstream,
                priority=priority)
   return(demand)
}