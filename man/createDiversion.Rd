\name{createDiversion}
\alias{createDiversion}
\title{
Constructor for class of \code{createDiversion}
}
\description{
this function constructs an object from class of \code{createDiversion}, acting as a diversion dam which is able to divert water up to a specified capacity.
}
\usage{
createDiversion(name, label, capacity,
                divertTo, downstream, priority)
}
\arguments{
  \item{name}{
(optional) A string: the name of the diversion
}
  \item{label}{
An individual label assigned to the object as a reference code.
}
  \item{capacity}{
The maximum capacity of diversion dam (CMS).
}
  \item{divertTo}{
The code of an object which recieves the diverted water volume.
}
  \item{downstream}{
The code of an object which overflow volume pours to it.
}
  \item{priority}{
(optional) An integer: the supplying priority. \code{priority} is a value in [1, 99] interval. If missing, the \code{priority} is set to \code{Inf}.
}
}
\value{
An object from class of \code{createDiversion}
}
\author{
Rezgar Arabzadeh}

\seealso{\code{\link[WRSS]{addObjectToArea}}}

\keyword{ list }
