\name{createDemandSite.default}
\alias{createDemandSite.default}
\title{
default function for class of \code{createDemandSite}
}
\description{
this function constructs an object from class of \code{createDemandSite}, which represents a demand site such as domestic, agricultural, and etc, with a specified demand time series.
}
\usage{
\method{createDemandSite}{default}(name  ="Unknown",
         label                                 ,
         demandTS          =NA                 ,
         demandParams=list(annualUseRate=NULL  ,
                           annualVariation=NULL,
                           cropArea=NULL)      ,
         returnFlowFraction =0.0               ,
         suppliers                             ,
         downstream        =NA                 ,
         priority          =NA)
}
\arguments{
  \item{name}{
(optional) A string: the name of the demand site
}
  \item{label}{
An individual label assigned to the object as a reference code.
}
  \item{demandTS}{
A vector: a vector of demand time series (MCM). If \code{demandParams} is null, \code{demandTS} is compulsory, otherwise the \code{demandTS} is not needed!.
}
  \item{demandParams}{
A list: If \code{demandTS} is missing, \code{demandParams} should not be omitted. \code{demandParams} includes three parts as folows:
\code{annualUseRate}: The annual water demand per hectare (MCM).
\code{annualVariation}: the precentage of water demand distribution within a year (the precentages in each month)
\code{cropArea}: the area of cropping farms (hectare).
}
  \item{returnFlowFraction}{
(optional) \code{returnFlowFraction} is fraction of total supplied water to the demand site.
The return flow is computed as the product of \code{returnFlowFraction} and the amount of water the demand sites receives. \code{returnFlowFraction} must be in [0, 1] interval.
}
  \item{suppliers}{
the reference code number(s) of existing suppliers (objects inherited from the folowing classes: \code{createAquifer}, \code{createRiver}, \code{createReservoir}, \code{createDiversion}).
}
  \item{downstream}{
The code of an object which return flow volume pours to it.
}
  \item{priority}{
An integer: the priority to be supplied. A value in [1, 99] interval.
}
}
\value{
An object from class of \code{createDemandSite}
}

\seealso{\code{\link[WRSS]{createDemandSite}}}

\keyword{ list }
