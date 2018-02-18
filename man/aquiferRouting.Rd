\name{aquiferRouting}
\alias{aquiferRouting}
\title{
base function for aquifer simulation
}
\description{
Given a sort of demand(s), \code{aquiferRouting} function simulates a lumped and simple model of an unconfined aquifer under an optional givn recharge time series, \code{rechargeTS}, and specific yield, \code{Sy}.
}
\usage{
aquiferRouting(demand, priority = NA, area, capacity,
               rechargeTS = NA, leakageFraction = NA,
               initialStorage = NA, Sy, simulation)
}
\arguments{
  \item{demand}{
A matrix: is column-wise matrix of demands, at which the rows presents demands for each monthly time step and columns are for different individual demand sites (MCM).
}
  \item{priority}{
A vector: is a vector of priorities associated to \code{demand}
}
  \item{area}{
The area of aquifer (Km^2)
}
  \item{capacity}{
The aquifer volume (MCM)
}
  \item{rechargeTS}{
A vector : a vector of water flowing into the aquifer (MCM)
}
  \item{leakageFraction}{
The leakage coeffcient of aquifer storage. The leakage is computed as the product of \code{leakageFraction} and aquifer storage
}
  \item{initialStorage}{
The initial volume of aquifer at the first step of the simulation (MCM). If missing, the function iterates to carry over the aquifer
}
  \item{Sy}{
Specific yield (default: 0.1)
}
  \item{simulation}{
A list: \code{simulation} is a list which includes two vectors of start and end of simulation interval respectively, at which each vector contains two number equivalant to year and month. 
}
}
\value{
the \code{aquiferRouting} function returns a list of features discussed as folows: 
\code{release}: a matrix of release(s) equivalant to each \code{demand} (MCM) 
\code{leakage}: a vector of leakage time series (MCM) 
\code{storage}: a vector of storage time series (MCM) 
}
\references{
Mart nez-Santos, P., and J. M. Andreu. "Lumped and distributed approaches to model natural recharge in semiarid karst aquifers." Journal of hydrology 388.3 (2010): 389-398.
}
\author{
Rezgar Arabzadeh
}

\seealso{\code{\link[WRSS]{reservoirRouting}}}
\examples{

area           <-200
leakageFraction<-0.01
Sy             <-0.15
capacity       <-20000
priority       <-c(3,1,1,2)
rechargeTS     <-rnorm(120,60,8)
demand         <-matrix(rnorm(480,10,3),120)
simulation     <-list(start=c(2000,1),end=c(2010,1))

res<-
aquiferRouting(demand         =demand         ,
               priority       =priority       ,
               area           =area           ,
               capacity       =capacity       ,
               rechargeTS     =rechargeTS     ,
               leakageFraction=leakageFraction,
               Sy             =Sy             ,
               simulation     =simulation)

plot(ts(res$storage,start=simulation$start,frequency=12),ylab='Storage (MCM)')
}
\keyword{list}
\keyword{matrix}
