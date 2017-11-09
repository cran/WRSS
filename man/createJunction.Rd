\name{createJunction}
\alias{createJunction}
\title{
Constructor for class of \code{createJunction}
}
\description{
this function constructs an object from class of \code{createDiversion}, acting as a junction in the basin which is able to aggregate outflow water from upper tributaries and/or objects in the upstream.
}
\usage{
createJunction(name, label, downstream)
}
\arguments{
  \item{name}{
(optional) A string: the name of the junction
}
  \item{label}{
An individual label assigned to the object as a reference code.
}
  \item{downstream}{
The code of an object which outflow volume pours to it.
}
}
\value{
An object from class of \code{createJunction}
}
\author{
Rezgar Arabzadeh
}
\seealso{\code{\link[WRSS]{addObjectToArea}}}
\keyword{list}