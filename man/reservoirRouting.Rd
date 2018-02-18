\name{reservoirRouting}
\alias{reservoirRouting}
\title{
base function for reservoir simulation
}
\description{
Given a sort of demand(s), \code{reservoirRouting} function enable us to simulate the performance and effect of a dam  under givn hydrometeorological time series, e.g. \code{inflow} and \code{netEvaporation}, on the drainage network.
}
\usage{
reservoirRouting(demand, priority, inflow, netEvaporation,
                 geometry = list(deadStorage=NULL        ,
                                 capacity   =NULL        ,
                                 ratingCurve=NULL)       ,
                 initialStorage = NA                     ,
                 seepageFraction = NA, simulation)
}
\arguments{
  \item{demand}{
A matrix: is column-wise matrix of demands, at which the rows presents demands for each monthly time steps and columns are for different individual demand sites (MCM).
}
  \item{priority}{
A vector: is a vector of priorities associated to \code{demand}
}
  \item{inflow}{
A vector : a vector of water flowing into the diversion (MCM)
}
  \item{netEvaporation}{
A vector: is a vector of net evaporation depth time series at the location of dam site (meter).
}
  \item{geometry}{
A list of reservoir geometric specifications:
\code{deadStorage}: refers to water in a reservoir that cannot be drained by gravity through a dam's outlet works (MCM).
\code{capacity}:    The maximum capacity of the reservoir.
\code{ratingCurve}: is a matrix whose first column includes reservoir volume (MCM) for different elevation levels and the second column contains reservoir area (Km^2) corresponding to the first column.
}
  \item{initialStorage}{
(optional) The initial stored water at the reservoir in the first step of the simulation (MCM). If is missing the the function iterate to carry over the aquifer.
}
  \item{seepageFraction}{
(optional) The seepage coeffcient of reservoir storage. The seepage is computed as the product of \code{seepageFraction} and reservoir storage.
}
  \item{simulation}{
A list: \code{simulation} is a list which includes two vectors of start and end of simulation interval respectively, at which each vector contains two number equivalant to year and month. 
}
}

\value{
the \code{reservoirRouting} function returns a list of features given as folows: 
\code{release}: a matrix of release(s) equivalant to each \code{demand} (MCM)
\code{spill}  : a vector of spilage time series (MCM) 
\code{seepage}: a vector of steepage time series (MCM) 
\code{storage}: a vector of storage time series (MCM) 
\code{loss}   : a vector of evaporation loss time series (MCM) 
}
\references{
Yeh, William WG. "Reservoir management and operations models: A state of the art review." Water resources research 21.12 (1985): 1797-1818.
}
\author{
Rezgar Arabzadeh
}

\seealso{\code{\link[WRSS]{aquiferRouting}}}

\examples{
demand         <-matrix(rnorm(480,10,3),120)
priority       <-sample(1:3,4,replace=TRUE)
inflow         <-rlnorm(120,log(50),log(4))
netEvaporation <-rnorm(120,0.4,0.1)
simulation     <-list(start=c(2000,1),end=c(2010,1))
seepageFraction<-0.05
geometry       <-list(deadStorage= 50,capacity= 100,
                      ratingCurve= cbind(seq(0,100,10),seq(0,10,1)))
reservoirRouting(demand=demand,
                 priority=priority, 
                 inflow=inflow,
                 netEvaporation=netEvaporation,
                 geometry=geometry,
                 seepageFraction=seepageFraction,
                 simulation=simulation)
}
\keyword{ list }
\keyword{ matrix }
