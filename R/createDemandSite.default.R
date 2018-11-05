createDemandSite.default <-
function(name              ="Unknown"          ,
         label                                 ,
         demandTS          =NA                 ,
         demandParams=list(waterUseRate=NULL   ,
                           waterVariation=NULL ,
                           cropArea=NULL)      ,
         returnFlowFraction =0.0               ,
         suppliers                             ,
         downstream        =NA                 ,
         priority          =NA)
{
   
   if(any(c(is.null(demandParams$waterUseRate  ),
            is.null(demandParams$waterVariation),
            is.null(demandParams$cropArea       ))) && all(is.na(demandTS)))  
   {
      stop("missing demand parameter(s) !")
   }
 
   if(all(is.na(demandTS)))
   {
      if(sum(demandParams$waterVariation)!=100)
      {
         warning('waterVariation is adjusted! the sum of it must be 100 %')
         demandParams$waterVariation<-demandParams$waterVariation+
                                      demandParams$waterVariation/sum(demandParams$waterVariation)*(100-sum(demandParams$waterVariation))
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
   operation<-createDemandSite.base(name              =name              ,
                                    label             =label             ,
                                    demandTS          =demandTS          ,
                                    demandParams      =demandParams      ,
                                    returnFlowFraction=returnFlowFraction,
                                    suppliers         =suppliers         ,
                                    downstream        =downstream        ,
                                    priority          =priority)
   resault$operation<-operation
   resault$call<-match.call()
   class(resault)<-'createDemandSite'
   return(resault)
}