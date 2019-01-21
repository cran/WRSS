createDiversion.default <-
function(name         ="Div1"  ,
         capacity               ,
         divertObject               ,
         downstream   =NA       ,
         priority     =NA)
{
   if(!any(c(class(downstream)==c("createAquifer","createRiver","createReservoir","createDiversion","createJunction","createDemandSite"),is.na(downstream))))
   {
      stop("diversion downstream code is wrongly specified!")
   }
   if(all(!is.na(downstream)))
   {
      downstream<-downstream$operation$label
   }
   if(!any(class(divertObject)==c("createAquifer","createRiver","createReservoir","createDiversion","createJunction","createDemandSite")))
   {
      stop("diversion target object is wrongly specified!")
   }
   divertObject<-divertObject$operation$label
   if(missing(capacity))
   {
      stop("capacity is not specified!")
   }
   if(missing(divertObject))
   {
      stop("the diversion outlet is not specified!")
   }
   if(is.na(priority))
   {
      priority<-Inf
   }
   resault<-list()
   operation<-createDiversion.base(name,
                                   capacity,
                                   divertObject,
                                   downstream,
                                   priority)
   resault$operation<-operation
   resault$call<-match.call()
   class(resault)<-'createDiversion'
   return(resault)
}
