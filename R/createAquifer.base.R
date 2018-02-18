createAquifer.base <-
function(name,area,label,capacity,rechargeTS,Sy,leakageFraction,initialStorage,leakageCode,priority)
{
   aquifer<-list(name=name,area=area,label=label,capacity=capacity,
                 rechargeTS=rechargeTS,Sy=Sy,leakageFraction=leakageFraction,
                 initialStorage=initialStorage,
                 leakageCode=leakageCode,priority=priority)
   return(aquifer)
}
