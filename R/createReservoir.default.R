createReservoir.default <-
function(type='storage',name='unknown',
         priority=NA,downstream=NA,netEvaporation=NA,
         seepageFraction=NA,seepageObject=NA,
         geometry=list(storageAreaTable=NULL,
                       storageElevationTable=NULL,
                       dischargeElevationTable=NULL,
                       deadStorage=NULL,capacity=NULL),
         plant=list(installedCapacity=NULL,
                    efficiency=NULL,
                    designHead=NULL,
                    designFlow=NULL,
                    turbineAxisElevation=NULL,
                    submerged=FALSE,loss=0),
         penstock=list(diameter=NULL,length=NULL,roughness=110),
         initialStorage=NA)
{
   if(!any(c(class(downstream)==c("createAquifer","createRiver","createReservoir","createDiversion","createJunction","createDemandSite"),is.na(downstream))))
   {
      stop("reservoir downstream object is wrongly specified!")
   }
   if(all(!is.na(downstream)))
   {
      downstream<-downstream$operation$label
   }
   if(!any(c(class(seepageObject)==c("createAquifer","createRiver","createReservoir","createDiversion","createJunction","createDemandSite"),is.na(seepageObject))))
   {
      stop("reservoir seepage object is wrongly specified!")
   }
   if(all(!is.na(seepageObject)))
   {
      seepageObject<-seepageObject$operation$label
   }
   if(type == 'storage')
   {
      if(any(c(is.null(geometry$storageAreaTable),is.null(geometry$capacity))))
      {
         stop('reservoir geometric specifications are not specified!')
      }
   }
   if(type == 'hydropower')
   {
      if(any(c(is.null(geometry$storageElevationTable),
               is.null(geometry$capacity),
               ifelse(plant$submerged,is.null(geometry$dischargeElevationTable),FALSE),
               is.null(plant$installedCapacity),
               is.null(plant$efficiency),
               is.null(plant$designHead),
               is.null(plant$designFlow),
               is.null(plant$turbineAxisElevation),
               ifelse(penstock$length>0,is.null(penstock$diameter),FALSE))))
      {
          stop('plant parameter(s) or reservoir geometric specifications required for power simulation are missing!')
      }
   }

   if(is.na(priority))
   {
      priority<-Inf
   }
   if(is.null(geometry$deadStorage))
   {
      geometry$deadStorage<-0
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
      if(initialStorage>geometry$capacity | initialStorage<0)
      {
         stop('bad initial storage is set!')
      }
   }
   if((is.na(seepageFraction)+is.na(seepageObject))==1)
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
   operation<-createReservoir.base (type,name,priority,downstream,netEvaporation,seepageFraction,seepageObject,geometry,plant,penstock,initialStorage)
   resault$operation<-operation
   resault$call<-match.call()
   class(resault)<-'createReservoir'
   return(resault)
}
