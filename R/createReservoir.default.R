createReservoir.default <-
function(name         ="resrvoir1"              ,
         label                                  ,
         priority        =NA                    ,
         netEvaporation                         ,
         downstream =NA                         ,
         initialStorage  =NA                    ,
         seepageFraction =NA                    ,
         seepageCode     =NA                    ,
         geometry   =list(deadStorage    = NULL ,
                          capacity       = NULL ,
                          ratingCurve    = NULL))
{

   if(missing(label))
   {
      stop("label code is missing!")
   }
   if(is.na(priority))
   {
      priority<-Inf
   }
   if(missing(netEvaporation))
   {
      stop("net evaporation is missing!")
   }
   if(is.null(geometry$ratingCurve))
   {
      stop("rating curve is missing!")
   }
   if(is.null(geometry$deadStorage))
   {
      stop("Minimum storage is missing!")
   }
   if(is.null(geometry$capacity))
   {
      stop("Maximum storage is missing!")
   }
   if(geometry$deadStorage>geometry$capacity)
   {
      stop("Minimum storage cannot be greater than capacity!")
   }
   if(!is.na(initialStorage))
   {
      if(initialStorage>geometry$capacity | initialStorage<geometry$deadStorage)
      {
         stop('bas initial storage is set!')
      }
   }
   if((is.na(seepageFraction)+is.na(seepageCode))==1)
   {
      stop("Seepage parameters missing!")
   }

   if(!is.na(seepageFraction))
   {
      if(seepageFraction>1 | seepageFraction<0)
      {
         stop('seepageFraction must be in [0, 1] interval!')
      }
   }else{
      seepageFraction<-0
   }

   resault<-list()
   operation<-createReservoir.base (name,label,priority,netEvaporation,downstream,initialStorage,seepageFraction,seepageCode,geometry)
   resault$operation<-operation
   resault$call<-match.call()
   class(resault)<-'createReservoir'
   return(resault)
}
