\name{riverRouting}
\alias{riverRouting}
\title{
base function for rivers and reachs simulation
}
\description{
Given a sort of demand(s), \code{riverRouting} function enable us to simulate rivers and channels under givn a hydrologic time series, \code{inflow}, and optional \code{demand}(s).
}
\usage{
riverRouting(demand, priority = NA, discharge = NA, simulation)
}
\arguments{
  \item{demand}{
A matrix: is column-wise matrix of demands, at which the rows presents demands for each monthly time steps and columns are for different individual demand sites (MCM).
}
  \item{priority}{
A vector: is a vector of priorities associated to \code{demand}
}
  \item{discharge}{
A vector : a vector of water flowing into the diversion (MCM)
}
  \item{simulation}{
A list: \code{simulation} is a list which includes two vectors of start and end of simulation interval respectively, at which each vector contains two number equivalant to year and month. 
}
}

\value{
the \code{riverRouting} returns a matrix of release(s) corresponding to each demand(s).
}
\author{
Rezgar Arabzadeh
}

\seealso{\code{\link[WRSS]{diversionRouting}}}

\examples{
demand         <-matrix(rnorm(480,10,3),120)
priority       <-sample(1:3,4,replace=TRUE)
discharge      <-rlnorm(120,log(50),log(4))
simulation     <-list(start=c(2000,1),end=c(2010,1))

riverRouting(demand    = demand   ,
             priority  = priority ,
             discharge = discharge,
             simulation= simulation)

}
\keyword{ matrix }
