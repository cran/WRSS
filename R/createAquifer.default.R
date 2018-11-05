createAquifer.default <-
function(name           ="Aquifer1" ,
         area                       ,
         label                      ,
         volume                     ,
         rechargeTS     =NA         ,
         Sy             =0.1        ,
         leakageFraction=NA         ,
         initialStorage =NA         ,
         leakageCode                ,
         priority       =NA)
{
   if(missing(label)){stop("label code is not specified!")}
   if(is.na(priority)){priority<-Inf}
   if(missing(volume)){stop("volume is not specified!")}
   if(missing(leakageCode)){stop("aquifer downstream code is not specified!")}
   if(is.na(leakageFraction)){leakageFraction<-0}
   if(!is.na(initialStorage)){if((initialStorage>volume*Sy) | (initialStorage<0)){stop('Bad initial storage!')}}
   if(is.na(Sy)){Sy<-0.1}

   resault<-list()
   operation<-createAquifer.base(name,area,label,volume,rechargeTS,Sy,leakageFraction,initialStorage,leakageCode,priority)
   resault$operation<-operation
   resault$call<-match.call()
   class(resault)<-'createAquifer'
   return(resault)
}
