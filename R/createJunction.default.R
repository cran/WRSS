createJunction.default <-
function(name         ="junc1"  ,
         downstream   =NA)
{
   if(!any(c(class(downstream)==c("createAquifer","createRiver","createReservoir","createDiversion","createJunction","createDemandSite"),is.na(downstream))))
   {
      stop("junction downstream object is wrongly specified!")
   }
   if(all(!is.na(downstream)))
   {
      downstream<-downstream$operation$label
   }
   resault<-list()
   operation<-createJunction.base(name,
                                  downstream)
   resault$operation<-operation
   resault$call<-match.call()
   class(resault)<-'createJunction'
   return(resault)
}
