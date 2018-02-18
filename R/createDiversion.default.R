createDiversion.default <-
function(name         ="junc1"  ,
         label                  ,
         capacity               ,
         divertTo               ,
         downstream   =NA       ,
         priority     =NA)
{
   if(missing(label))
   {
      stop("label code is not specified!")
   }
   if(missing(capacity))
   {
      stop("capacity is not specified!")
   }
   if(missing(divertTo))
   {
      stop("the diversion outlet is not specified!")
   }
   if(is.na(priority))
   {
      priority<-Inf
   }
   resault<-list()
   operation<-createDiversion.base(name,
                                   label,
                                   capacity,
                                   divertTo,
                                   downstream,
                                   priority)
   resault$operation<-operation
   resault$call<-match.call()
   class(resault)<-'createDiversion'
   return(resault)
}
