createAquifer.default <-
function(name           ="Aquifer1" ,
         area                       ,
         volume                     ,
         rechargeTS     =NA         ,
         Sy             =0.1        ,
         leakageFraction=NA         ,
         initialStorage =NA         ,
         leakageObject  =NA         ,
         priority       =NA,
         latlon         =NULL)
{
   if(is.na(priority)){priority<-Inf}
   if(missing(volume)){stop("volume is not specified!")}
   if(!any(c(inherits(leakageObject,c("createAquifer","createRiver","createReservoir","createDiversion","createJunction","createDemandSite")),all(is.na(leakageObject)))))
   {
      stop("aquifer's downstream is wrongly specified!")
   }
   if(all(!is.na(leakageObject)))
   {
      leakageObject<-leakageObject$operation$label
   }
  if (!is.null(latlon)) {
    
    if (length(latlon) != 2) {
      stop("`latlon` must be a vector of length 2.")
    }
    
    if (!(latlon[1] > -90  && latlon[1] < 90 &&
          latlon[2] > -180 && latlon[2] < 180)) {
      stop("Latitude must be in (-90, 90) and longitude in (-180, 180).")
    }
  }
   if(is.na(leakageFraction)){leakageFraction<-0}
   if(!is.na(initialStorage)){if((initialStorage>volume*Sy) | (initialStorage<0)){stop('Bad initial storage!')}}
   if(is.na(Sy)){Sy<-0.1}
   resault<-list()
   operation<-createAquifer.base(name,area,volume,rechargeTS,Sy,leakageFraction,initialStorage,leakageObject,priority,latlon)
   resault$operation<-operation
   resault$call<-match.call()
   class(resault)<-'createAquifer'
   return(resault)
}
