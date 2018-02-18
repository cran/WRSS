createRiver.default <-
function(name            ="river1",
         label                    ,
         downstream      =NA      ,
         seepageFraction =NA      ,
         seepageCode     =NA      ,
         discharge,
         priority        =NA)
{
   if(missing(label))
   {
      stop("label code is not specified!")
   }
   if(missing(discharge))
   {
      stop("discharge time series is not specified!")
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
   if(is.na(priority)){priority<-Inf}
   resault<-list()
   operation<-createRiver.base(name,
                               label,
                               downstream,
                               seepageFraction,
                               seepageCode,
                               discharge,
                               priority)
   resault$operation<-operation
   resault$call<-match.call()
   class(resault)<-'createRiver'
   return(resault)
}
