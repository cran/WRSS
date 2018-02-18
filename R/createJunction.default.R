createJunction.default <-
function(name         ="junc1"  ,
         label                  ,
         downstream   =NA)
{
   if(missing(label))
   {
      stop("label code is not specified!")
   }
   
   resault<-list()
   operation<-createJunction.base(name,
                                  label,
                                  downstream)
   resault$operation<-operation
   resault$call<-match.call()
   class(resault)<-'createJunction'
   return(resault)
}
