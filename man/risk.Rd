\name{risk}
\alias{risk}
\title{
risk-based criteria 
}
\description{
this function returns risk-based criteria for demand site(s) built-in the object inherited from class of \code{sim}.
}
\usage{
risk(object , s.const = 0.95)
}
\arguments{
  \item{object}{an object from class of \code{sim}}
  \item{s.const}{satisfactory constant: a value in [0, 1] interval, which refers to the level at which if a demand is supplied over the \code{s.const} is considered fully supplied.}
}
\details{
This function computes the riks criteria based on the formulations proposed by Hashimoto et.al (1982).
}
\value{
a matrix of criteria
}
\references{
Hashimoto, Tsuyoshi, Jery R. Stedinger, and Daniel P. Loucks. "Reliability, resiliency, and vulnerability criteria for water resource system performance evaluation." Water resources research 18.1 (1982): 14-20.
}
\author{
Rezgar Arabzadeh
}
\seealso{\code{\link[WRSS]{sim}}}
\examples{
R<-createRiver(name="Riv1",label=1,downstream=2,discharge=rnorm(120,4.5,1.5))
Res<-createReservoir(name="R1",label=2,priority=1,
                     netEvaporation=rnorm(120,0.5,0.1),downstream =NA,
                     geometry=list(deadStorage= 10,
                                   capacity= 20,
                                   ratingCurve= cbind(seq(0,90,10),seq(0,9,1))))
annualVariation<-round(sin(seq(0,pi,length.out=12))*
                       100/sum(sin(seq(0,pi,length.out=12))))
D1<-createDemandSite(name ="D1",label=5,demandTS=NA,
                     demandParams=list(annualUseRate=0.01,
                                       annualVariation=annualVariation,
                                       cropArea=3000),
                     suppliers=2,downstream=NA,priority=1)
D2<-createDemandSite(name ="D2",label=6,demandTS=NA,
                     demandParams=list(annualUseRate=0.01,
                                       annualVariation=annualVariation,
                                       cropArea=2000),
                     suppliers=2,downstream=NA,priority=2)
area<-createArea(name="unknown",location="unknown",
                 simulation=list(start=c(1900,1),end=c(1910,01)))
area<-addObjectToArea(area,R)
area<-addObjectToArea(area,Res)
area<-addObjectToArea(area,D1)
area<-addObjectToArea(area,D2)
risk(sim(area))
}
\keyword{ matrix }
