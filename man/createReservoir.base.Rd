\name{createReservoir.base}
\alias{createReservoir.base}
\title{
base function for class of \code{createReservoir}
}
\description{
 this function constructs an object from class of \code{createReservoir}, which is able to simulate a storage reservoir under given a sort of demand(s).
}
\usage{
\method{createReservoir}{base}(name, label, priority, netEvaporation,
                 downstream, initialStorage,
                 seepageFraction, seepageCode, geometry)
}
\arguments{
  \item{name}{
(optional) A string: the name of the reservoir
}
  \item{label}{
An individual label assigned to the object as a reference code.
}
  \item{priority}{
(optional) An integer: the supplying priority. \code{priority} is a value in [1, 99] interval. If missing, the \code{priority} is set to Inf.
}
  \item{netEvaporation}{
A vector: is a vector of net evaporation depth time series at the location of dam site (meter).
}
  \item{downstream}{
The code of an object which spilage volume pours to it.
}
  \item{initialStorage}{
(optional) The initial stored water at the reservoir in the first step of the simulation (MCM). If is missing the the function iterate to carry over the aquifer.
}
  \item{seepageFraction}{
(optional) The seepage coeffcient of reservoir storage. The seepage is computed as the product of \code{seepageFraction} and reservoir storage.
}
  \item{seepageCode}{
The code of an object which seepage volume pours to it.
}
  \item{geometry}{
A list of reservoir geometric specifications:
\code{deadStorage}:  refers to water in a reservoir that cannot be drained by gravity through a dam's outlet works (MCM) 
\code{capacity}: The maximum capacity of the reservoir
\code{ratingCurve}: is a matrix whose first column includes reservoir volume (MCM) for different elevation levels and the second column contains reservoir area (Km^2) corresponding to the first column.
}
}
\value{
An object from class of \code{list}
}
\seealso{\code{\link[WRSS]{createReservoir}}}
\keyword{ list }