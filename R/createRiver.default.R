createRiver.default <-
  function(name            ="river1",
           downstream      =NA      ,
           seepageFraction =NA      ,
           seepageObject   =NA      ,
           discharge       =NA      ,
           priority        =NA      ,
           latlon          =NULL)
  {
    if(!any(c(inherits(downstream,c("createAquifer","createRiver","createReservoir","createDiversion","createJunction","createDemandSite")),all(is.na(downstream)))))
    {
      stop("river downstream object is wrongly specified!")
    }
    if(all(!is.na(downstream)))
    {
      downstream<-downstream$operation$label
    }
    if(!any(c(inherits(seepageObject,c("createAquifer","createRiver","createReservoir","createDiversion","createJunction","createDemandSite")),all(is.na(seepageObject)))))
    {
      stop("river seepage object is wrongly specified!")
    }
    if(all(!is.na(seepageObject)))
    {
      seepageObject<-seepageObject$operation$label
    }
    if(missing(discharge))
    {
      stop("discharge time series is not specified!")
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
    if (!is.null(latlon)) {
      
      if (length(latlon) != 2) {
        stop("`latlon` must be a vector of length 2.")
      }
      
      if (!(latlon[1] > -90  && latlon[1] < 90 &&
            latlon[2] > -180 && latlon[2] < 180)) {
        stop("Latitude must be in (-90, 90) and longitude in (-180, 180).")
      }
    }
    
    if(is.na(priority)){priority<-Inf}
    resault<-list()
    operation<-createRiver.base(name,
                                downstream,
                                seepageFraction,
                                seepageObject,
                                discharge,
                                priority,
                                latlon)
    resault$operation<-operation
    resault$call<-match.call()
    class(resault)<-'createRiver'
    return(resault)
  }
