\name{diversionRouting}
\alias{diversionRouting}
\title{
base function for diversion simulation
}
\description{
Given a sort of demand(s), \code{diversionRouting} function enable us to simulate the performance and effect of a diversion dam  under a givn recharge time series, \code{inflow}, on the drainage network.
}
\usage{
diversionRouting(demand, priority = NA,
                 capacity, inflow, simulation)
}
\arguments{
  \item{demand}{
A matrix: is column-wise matrix of demands, at which the rows presents demands for each monthly time step and columns are for different individual demand sites (MCM).
}
  \item{priority}{
A vector: is a vector of priorities associated to \code{demand}
}
  \item{capacity}{
The maximum capacity of diversion dam (CMS).
}
  \item{inflow}{
A vector : a vector of water flowing into the diversion (MCM)
}
  \item{simulation}{
A list: \code{simulation} is a list which includes two vectors of start and end of simulation interval respectively, at which each vector contains two number equivalant to year and month. 
}
}
\value{
the \code{diversionRouting} function returns a list of features discussed as folows: 
\code{release} : a matrix of release(s) equivalant to each \code{demand} (MCM) 
\code{diverted}: a vector of diverted volumes (MCM), release(s) are included 
\code{overflow}: a vector of overflow passing the diversion (MCM) 
}
\author{
Rezgar Arabzadeh
}

\seealso{\code{\link[WRSS]{aquiferRouting}}}

\examples{
demand         <-matrix(rnorm(480,10,3),120)
priority       <-sample(1:3,4,replace=TRUE)
capacity       <-12
inflow         <-rlnorm(120,log(50),log(4))
simulation     <-list(start=c(2000,1),end=c(2010,1))
diversionRouting(demand=demand,
                 priority=priority,
                 capacity=capacity,
                 inflow=inflow,
                 simulation=simulation)
}
\keyword{ list }
\keyword{ matrix }
