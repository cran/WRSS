\name{WRSS-package}
\alias{WRSS-package}
\alias{WRSS}
\docType{package}
\title{
Water Resources System Simulator
}
\description{
The WRSS is an object-oriented R package, which provides tools for simulation and analysis of large-scale supply hydrosystems. The package includes functions and methods for building, simulation, and visualization of water resources components.

}
\details{
\tabular{ll}{
Package: \tab WRSS\cr
Type: \tab Package\cr
Version: \tab 1.2\cr
Date: \tab 2018-04-18\cr
License: \tab GPL-3\cr
}
the package includes three major types of functions as follows:


1- functions for construction and manipulatation of water resources features:

   a) \code{\link[WRSS]{createArea}}.        constructor for basin/study area

   b) \code{\link[WRSS]{createJunction}}.    constructor for junction

   c) \code{\link[WRSS]{createRiver}}.       constructor for reach, rivers, and channels

   d) \code{\link[WRSS]{createReservoir}}.   constructor for reservoirs

   e) \code{\link[WRSS]{createDiversion}}.   constructor for diversions

   f) \code{\link[WRSS]{createAquifer}}.     constructor for aquifers

   g) \code{\link[WRSS]{addObjectToArea}}.   adds objects form mentioned above constructors to a basin inherited from class of \code{createBasin}
                                                

                                                
2- functions for analysis and operation of water resources objects using Standard Operating Policy (SOP):
                                                
   a) \code{\link[WRSS]{riverRouting}}.      river operation using 

   b) \code{\link[WRSS]{reservoirRouting}}.  reservoir operation

   c) \code{\link[WRSS]{aquiferRouting}}.    aquifer operation

   d) \code{\link[WRSS]{diversionRouting}}.  diversion operation

   e) \code{\link[WRSS]{sim}}.               simulates an objects inherited from class of \code{createArea}

   f) \code{\link[WRSS]{rippl}}.             computes no-failure storage volume using the sequent peak algorithm(SPA)



3- functions for performance analysis and visualization.

   a) \code{\link[WRSS]{plot.sim}}.          plots the results of simulations for an object inherited from class of \code{sim}

   b) \code{\link[WRSS]{risk}}.              computes risk-based criateria for an object inherited from class of \code{sim}

}
\author{
Rezgar Arabzadeh; Parisa Aberi; Kaveh Panaghi; Shahab Araghinejad; Majid Montaseri

Maintainer: Rezgar Arabzadeh <rezgararabzadeh@ut.ac.ir>
}
\references{
Loucks, Daniel P., et al. Water resources systems planning and management: an introduction to methods, models and applications. Paris: Unesco, 2005.
}
\keyword{package}
\seealso{\code{\link[WRSS]{addObjectToArea}}, \code{\link[WRSS]{plot.sim}}}