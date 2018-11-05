\name{createJunction.default}
\alias{createJunction.default}
\title{
default function for class of \code{createJunction}
}
\description{
this function constructs an object from class of \code{createDiversion}, acting as a junction in the basin which is able to aggregate outflow water from upper tributaries and/or objects in the upstream.
}
\usage{
\method{createJunction}{default}(name = "junc1", label, downstream = NA)
}
\arguments{
  \item{name}{(optional) A string: the name of the junction}
  \item{label}{An individual label assigned to the object as a reference code.}
  \item{downstream}{The code of an object which outflow volume pours to it.}
}
\value{
An object from class of \code{list}
}

\seealso{\code{\link[WRSS]{createJunction}}}
\keyword{list}
