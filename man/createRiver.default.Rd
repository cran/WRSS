\name{createRiver.default}
\alias{createRiver.default}
\title{
default function for class of \code{createRiver}
}
\description{
this function constructs an object from class of \code{createRiver}, which is able to act as a chanel or resource to supply a seort of demand(s).
}
\usage{
\method{createRiver}{default}(name = "river1", label, downstream = NA,
               seepageFraction = NA, seepageCode = NA,
               discharge, priority = NA)
}
\arguments{
  \item{name}{(optional) A string: the name of the river}
  \item{label}{An individual label assigned to the object as a reference code.}
  \item{downstream}{The code of an object which outflow volume pours to it.}
  \item{seepageFraction}{(optional) The seepage coeffcient of river discharge flow. The seepage is computed as the product of \code{seepageFraction} and river discharge. It is in \code{[0, 1]} interval}
  \item{seepageCode}{The code of an object which seepage volume pours to it.}
  \item{discharge}{(optional) A vector: is a vector of river discharge time series (MCM).}
  \item{priority}{(optional) An integer: the supplying priority. \code{priority} is a value in [1, 99] interval. If missing, the \code{priority} is set to \code{Inf}.}
}
\value{
An object from class of \code{createRiver}
}

\seealso{\code{\link[WRSS]{createRiver}}}

\keyword{list}