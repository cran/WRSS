addObjectToArea <-
function(area,object)
{
   if(missing(area))
   {
      stop("Area is missing !")
   }
   if(missing(object))
   {
      stop("The object you want to be added to area is missing !")
   }
   
   start<-area$operation$simulatio$start
   end<-area$operation$simulatio$end
   simulationPeriod<-sum((end-start)*c(12,1))

   if(class(object)=="createAquifer")
   {
      object$operation$outflow<-matrix(0,simulationPeriod)
      if(!all(is.na(object$operation$rechargeTS)))
      {
         if(length(object$operation$rechargeTS)==simulationPeriod)
         {
            rechargeTS<-as.matrix(object$operation$rechargeTS)
            colnames(rechargeTS)<-'natural recharge'
            object$operation$inflow<-rechargeTS
         }
         if(length(object$operation$rechargeTS)!=simulationPeriod)
         {
            stop("aquifer's recharge time series doesn't match with simulation interval!")
         }
      }else{
         inflow<-as.matrix(0,simulationPeriod)
         colnames(inflow)<-'zero flow'
         object$operation$inflow<-inflow
      }
      object$operation$rechargeTS<-NULL
      i<-length(area$operation$aquifers)+1
      area$operation$aquifers[[i]]<-object
   }

   if(class(object)=="createRiver")
   {
      object$operation$outflow<-matrix(0,simulationPeriod)
      if(length(object$operation$discharge)==simulationPeriod)
      {
          riverDischarge            <-as.matrix(object$operation$discharge)
          colnames(riverDischarge)  <- 'river discharge'
          object$operation$inflow   <-riverDischarge
          object$operation$discharge<-NULL
      }else{
          stop("source discharge time series doesn't match with simulation interval!")
      }
      i<-length(area$operation$rivers)+1
      area$operation$rivers[[i]]<-object
   }

   if(class(object)=="createReservoir")
   {
      if(length(object$operation$netEvaporation)!=simulationPeriod)
      {
         stop("source net ET time series doesn't match with simulation interval!")
      }
      object$operation$outflow<-matrix(0,simulationPeriod)
      object$operation$inflow<-matrix(0,simulationPeriod)
      i<-length(area$operation$reservoirs)+1
      area$operation$reservoirs[[i]]<-object
   }

   if(class(object)=="createDiversion")
   {
      object$operation$outflow<-matrix(0,simulationPeriod)
      object$operation$inflow<-matrix(0,simulationPeriod)
      i<-length(area$operation$diversions)+1
      area$operation$diversions[[i]]<-object
   }

   if(class(object)=="createJunction")
   {
      object$operation$outflow<-matrix(0,simulationPeriod)
      object$operation$inflow<-matrix(0,simulationPeriod)
      i<-length(area$operation$junctions)+1
      area$operation$junctions[[i]]<-object
   }

   if(class(object)=="createDemandSite")
   {
      if(all(is.na(object$operation$demandTS)))
      {
         object$operation$demandTS<-rep(object$operation$demandParams$annualUseRate  *
                                        object$operation$demandParams$annualVariation*
                                        object$operation$demandParams$cropArea/100,
                                        floor(simulationPeriod/12)+1)[1:simulationPeriod]
      }else{
         if(length(object$operation$demandTS)!=simulationPeriod)
          {
            stop("The simulation period and demand time series simulation period miss match !")
          }
      }
      object$operation$outflow<-matrix(0,simulationPeriod)
      object$operation$inflow<-matrix(0,simulationPeriod)
      i<-length(area$operation$demands)+1
      area$operation$demands[[i]]<-object
   }
   return(area)
}