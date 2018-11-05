\name{createDiversion.default}
\alias{createDiversion.default}
\title{
default function for class of \code{createDiversion}
}
\description{
this function constructs an object from class of \code{createDiversion}, acting as a diversion dam which is able to divert water up to a specified capacity.
}
\usage{
\method{createDiversion}{default}(name = "Div1", label, capacity,
                    divertTo, downstream = NA, priority=NA)
}
\arguments{
  \item{name}{(optional) A string: the name of the diversion}
  \item{label}{An individual label assigned to the object as a reference code.}
  \item{capacity}{The maximum capacity of diversion dam (CMS).}
  \item{divertTo}{The code of an object which recieves the diverted water volume.}
  \item{downstream}{The code of an object which overflow volume pours to it.}
  \item{priority}{(optional) An integer: the supplying priority. \code{priority} is a value in [1, 99] interval. If missing, the \code{priority} is set to \code{Inf}.}
}

\value{
An object from class of \code{createDiversion}
}
\seealso{\code{\link[WRSS]{createDiversion}}}
\keyword{list}
