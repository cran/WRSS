createAquifer.base <-
function(name,area,label,volume,rechargeTS,Sy,leakageFraction,initialStorage,leakageCode,priority)
{
   aquifer<-list(name           =name           ,
                 area           =area           ,
                 label          =label          ,
                 volume         =volume         ,
                 rechargeTS     =rechargeTS     ,
                 Sy             =Sy             ,
                 leakageFraction=leakageFraction,
                 initialStorage =initialStorage ,
                 leakageCode    =leakageCode    ,
                 priority       =priority)
   return(aquifer)
}
