createArea.default <-
function(name="unknown",location="unknown",simulation=list(start=NULL,end=NULL))
{
   if(is.null(simulation$start))
   {
      stop("Simulation start date is not specified !")
   }
   if(is.null(simulation$end))
   {
      stop("Simulation end date is not specified !")
   }
   duration<-(simulation$end[1]-simulation$start[1])*12+(simulation$end[2]-simulation$start[2])
   if(duration<0)
   {
      stop("The enterd dates are not accuarte !")
   }

   resault<-list()
   operation<-createArea.base(name,
                              location,
                              simulation)
   resault$operation<-operation
   resault$call<-match.call()
   class(resault)<-'createArea'
   return(resault)
}
