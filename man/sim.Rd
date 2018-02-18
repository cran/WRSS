\name{sim}
\alias{sim}
\title{
Constructor for class of \code{sim}
}
\description{
\code{sim} simulates an object inherited from class of \code{createArea} using Standard Operating Policy (SOP).
}
\usage{
sim(object)
}
\arguments{
  \item{object}{
an object inherited from class of \code{createArea}.
}
}
\value{
an object inherited from class of \code{sim} and including features as list(s), which are accessable as follows:

reservoirs:  $operation$operation$reservoirs
rivers:      $operation$operation$rivers
junctions:   $operation$operation$junctions
aquifers:    $operation$operation$aquifers
diversions:  $operation$operation$diversions
demands:     $operation$operation$demands
}
\references{
Loucks, Daniel P., et al. Water resources systems planning and management: an introduction to methods, models and applications. Paris: Unesco, 2005.
}
\author{
Rezgar Arabzadeh
}

\seealso{\code{\link[WRSS]{addObjectToArea}}}

\keyword{ list }
