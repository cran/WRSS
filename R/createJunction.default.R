createJunction.default <-
  function(name         ="junc1"  ,
           downstream   =NA,
           latlon       =NULL)
  {
    
    if(!any(c(inherits(downstream,c("createAquifer","createRiver","createReservoir","createDiversion","createJunction","createDemandSite")),all(is.na(downstream)))))
    {
      stop("junction downstream is wrongly specified!")
    }
    if(all(!is.na(downstream)))
    {
      downstream<-downstream$operation$label
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
    operation<-createJunction.base(name,
                                   downstream,
                                   latlon)
    resault$operation<-operation
    resault$call<-match.call()
    class(resault)<-'createJunction'
    return(resault)
  }
