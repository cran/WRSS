createAquifer.base <-
function(name,area,volume,rechargeTS,Sy,leakageFraction,initialStorage,leakageObject,priority, latlon)
{
   aquifer<-list(name           =name           ,
                 area           =area           ,
                 label          =runif(1)       ,
                 volume         =volume         ,
                 rechargeTS     =rechargeTS     ,
                 Sy             =Sy             ,
                 leakageFraction=leakageFraction,
                 initialStorage =initialStorage ,
                 leakageObject  =leakageObject  ,
                 priority       =priority,
                 latlon         =latlon)
   return(aquifer)
}
