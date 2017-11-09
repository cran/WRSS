createDemandSite.default <-
function(name              ="Unknown"          ,
         label                                 ,
         demandTS          =NA                 ,
         demandParams=list(annualUseRate=NULL  ,
                           annualVariation=NULL,
                           cropArea=NULL)      ,
         returnFlowFraction =0.0               ,
         suppliers                             ,
         downstream        =NA                 ,
         priority          =NA)
{
   
   if(any(c(is.null(demandParams$annualUseRate  ),
            is.null(demandParams$annualVariation),
            is.null(demandParams$cropArea       ))) && is.na(demandTS))  
   {
      stop("missing demand parameter(s) !")
   }

   if(is.null(demandTS))
   {
      if(all.equal(sum(demandParams$annualVariation),100))
      {
         cat("Sum of annual variation is OK!")
      }else{
         stop("Sum of annual variation must be equal to 100 % !")
      }
   }
   if(missing(suppliers))
   {
      stop("Supplier label is not specified !")
   }
   if(is.na(priority))
   {
      priority<-Inf
   }
   
   resault<-list()
   operation<-createDemandSite.base(name=name,
                                    label=label,
                                    demandTS=demandTS,
                                    demandParams=demandParams,
                                    returnFlowFraction=returnFlowFraction,
                                    suppliers=suppliers,
                                    downstream=downstream,
                                    priority=priority)
   resault$operation<-operation
   resault$call<-match.call()
   class(resault)<-'createDemandSite'
   return(resault)
}
