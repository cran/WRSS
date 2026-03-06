createDiversion.default <-
  function(name         ="Div1",
           capacity            ,
           divertObject =NA    ,
           downstream   =NA    ,
           priority     =NA,
           latlon       =NULL)
  {
    if(!any(c(inherits(downstream,c("createAquifer","createRiver","createReservoir","createDiversion","createJunction","createDemandSite")),all(is.na(downstream)))))
    {
      stop("diversion downstream is wrongly specified!")
    }
    if(all(!is.na(downstream)))
    {
      downstream<-downstream$operation$label
    }
    
    if(!any(c(inherits(divertObject,c("createAquifer","createRiver","createReservoir","createDiversion","createJunction","createDemandSite")),all(is.na(divertObject)))))
    {
      stop("diversion target is wrongly specified!")
    }
    if(!all(is.na(divertObject)))
    {
      divertObject<-divertObject$operation$label
    }
    if(missing(capacity))
    {
      stop("capacity is not specified!")
    }
    if(is.na(priority))
    {
      priority<-Inf
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
    
    resault<-list()
    operation<-createDiversion.base(name,
                                    capacity,
                                    divertObject,
                                    downstream,
                                    priority,
                                    latlon)
    resault$operation<-operation
    resault$call<-match.call()
    class(resault)<-'createDiversion'
    return(resault)
  }
