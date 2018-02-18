\name{addObjectToArea}
\alias{addObjectToArea}
\title{
Adds a feature to area
}
\description{
This function adds objects from the basin primary features to the object inherited from class of \code{createArea}.
}
\usage{
addObjectToArea(area, object)
}
\arguments{
  \item{area}{
An object inherited from \code{\link[WRSS]{createArea}}
}
  \item{object}{
An objects inherited from any of the following constructors: \code{\link[WRSS]{createAquifer}} , \code{\link[WRSS]{createRiver}}, \code{\link[WRSS]{createReservoir}}, \code{\link[WRSS]{createJunction}}, \code{\link[WRSS]{createDiversion}}, and \code{\link[WRSS]{createDemandSite}}.
}
}

\value{
an object from class of \code{createArea}
}
\references{
Loucks, Daniel P., et al. Water resources systems planning and management: an introduction to methods, models and applications. Paris: Unesco, 2005.
}
\author{
Rezgar Arabzadeh
}

\details{
The examples included in this documentation show construction and simulation of primary features of a water resources system using \code{WRSS} package.
The Figure below presents schematic layouts attributed to the examples at the rest of the page:

\if{html}{\figure{Fig.png}{options: width="60\%" alt="Figure: Fig.png"}}
\if{latex}{\figure{Fig.pdf}{options: width=14cm}}

}

\seealso{\code{\link[WRSS]{sim}}}

\examples{

#--------------------1st Example--------------------
R<-createRiver(name="river1",label=1,downstream=2,discharge=rnorm(120,5,2))
Res<-createReservoir(name="res3",label=2,
                     priority=1,netEvaporation=rnorm(120,0.5,0.1),downstream =NA ,
                     geometry=list(deadStorage= 10 ,capacity= 90 ,
                     ratingCurve= cbind(seq(0,90,10),seq(0,9,1))))
annualVariation<-round(sin(seq(0,pi,length.out=12))*
                       100/sum(sin(seq(0,pi,length.out=12))))
D<-createDemandSite(name ="Agri5",label=5,demandTS=NA,
                    demandParams=list(annualUseRate=0.01,
                                      annualVariation=annualVariation,
                                      cropArea=5000),
                    returnFlowFraction =0.2,suppliers=2,
                    downstream=NA,priority=1)
area<-createArea(name="unknown",location="unknown",
                 simulation=list(start=c(1900,1),
                                 end  =c(1910,01)))
area<-addObjectToArea(area,R)
area<-addObjectToArea(area,Res)
area<-addObjectToArea(area,D)
\dontrun{
simulated<-sim(area)
plot(simulated)
}


#--------------------2nd Example--------------------
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
\dontrun{
simulated<-sim(area)
plot(simulated)
}


#--------------------3rd Example--------------------
R1<-createRiver(name="Riv1",label=1,downstream=3,discharge=rnorm(120,4.5,1.5))
R2<-createRiver(name="Riv2",label=2,downstream=4,discharge=rnorm(120,6.5,2))
Res1<-createReservoir(name="R1",label=3,priority=1,
                      netEvaporation=rnorm(120,0.5,0.1),downstream =5,
                      geometry=list(deadStorage= 10,
                                    capacity= 20,
                                    ratingCurve= cbind(seq(0,90,10),seq(0,9,1))))
Res2<-createReservoir(name="R2",label=4,priority=2,
                      netEvaporation=rnorm(120,0.5,0.1),downstream =5,
                      geometry=list(deadStorage= 10 ,
                                    capacity= 30 ,
                                    ratingCurve= cbind(seq(0,90,10),seq(0,9,1))))
J1<-createJunction(name="j1",label=5,downstream=NA)
annualVariation<-round(sin(seq(0,pi,length.out=12))*
                       100/sum(sin(seq(0,pi,length.out=12))))
D1<-createDemandSite(name ="D1",label=6,demandTS=NA,
                     demandParams=list(annualUseRate=0.01,
                                       annualVariation=annualVariation,
                                       cropArea=3000),
                     returnFlowFraction=0.2,suppliers=3,
                     downstream=5,priority=1)
D2<-createDemandSite(name ="D2",label=7,demandTS=NA,
                     demandParams=list(annualUseRate=0.01,
                                       annualVariation=annualVariation,
                                       cropArea=3000),
                     returnFlowFraction=0.2,
                     suppliers=4,downstream=5,priority=1)
D3<-createDemandSite(name ="D3",label=8,demandTS=NA,
                     demandParams=list(annualUseRate=0.01,
                                       annualVariation=annualVariation,
                                       cropArea=4000),
                     returnFlowFraction=0.2,suppliers=c(3,4),
                     downstream=5,priority=2)
area<-createArea(name="unknown",location="unknown",
                 simulation=list(start=c(1900,1),end=c(1910,01)))
area<-addObjectToArea(area,R1)
area<-addObjectToArea(area,R2)
area<-addObjectToArea(area,Res1)
area<-addObjectToArea(area,Res2)
area<-addObjectToArea(area,D1)
area<-addObjectToArea(area,D2)
area<-addObjectToArea(area,D3)
area<-addObjectToArea(area,J1)
\dontrun{
simulated<-sim(area)
plot(simulated)
}


#--------------------4th Example--------------------
annualVariation<-round(sin(seq(0,pi,length.out=12))*
                       100/sum(sin(seq(0,pi,length.out=12))))
R1<-createRiver(name="Riv1",label=1,downstream=3,discharge=rnorm(120,4.5,1))
R2<-createRiver(name="Riv2",label=2,downstream=4,discharge=rnorm(120,2.5,0.25))
Res1<-createReservoir(name="R1",label=3,priority=1,
                      netEvaporation=rnorm(120,0.5,0.1),downstream =8,
                      seepageFraction=0.05, seepageCode=8,
                      geometry=list(deadStorage=10,
                                    capacity= 20,
                                    ratingCurve= cbind(seq(0,90,10),seq(0,9,1))))
Auq1<-createAquifer(name="Aquifer1",area=100,label=4,capacity=5000,
                    rechargeTS=rnorm(120,10,3),Sy=0.1,
                    leakageFraction=0.02,leakageCode=8,priority=2)
D1<-createDemandSite(name ="D1",label=5,demandTS=NA,
                     demandParams=list(annualUseRate=0.01,
                                       annualVariation=annualVariation,
                                       cropArea=3000),
                     returnFlowFraction=0.2,suppliers=c(3,4),
                     downstream=8,priority=1)
D2<-createDemandSite(name ="D2",label=6,demandTS=NA,
                     demandParams=list(annualUseRate=0.01,
                                       annualVariation=annualVariation,
                                       cropArea=3000),
                     returnFlowFraction=0.2,suppliers=c(3,4),
                     downstream=8,priority=1)
D3<-createDemandSite(name ="D3",label=7,demandTS=NA,
                     demandParams=list(annualUseRate=0.01,
                                       annualVariation=annualVariation,
                                       cropArea=4000),
                     returnFlowFraction=0.2,suppliers=c(3,4),
                     downstream=8,priority=2)
J1<-createJunction(name="j1",label=8,downstream=NA)
area<-createArea(name="unknown",location="unknown",
                 simulation=list(start=c(1900,1),end=c(1910,01)))
area<-addObjectToArea(area,R1)
area<-addObjectToArea(area,R2)
area<-addObjectToArea(area,Res1)
area<-addObjectToArea(area,Auq1)
area<-addObjectToArea(area,D1)
area<-addObjectToArea(area,D2)
area<-addObjectToArea(area,D3)
area<-addObjectToArea(area,J1)
\dontrun{
simulated<-sim(area)
plot(simulated)
}


#--------------------5th Example--------------------
annualVariation<-round(sin(seq(0,pi,length.out=12))*
                       100/sum(sin(seq(0,pi,length.out=12))))
R1<-createRiver(name="River1",label=1,
                downstream=2,discharge=rnorm(120,20,3),
                seepageFraction=0.1,seepageCode=3)
Res1<-createReservoir(name="Reservoir1",label=2,priority=1,
                      netEvaporation=rnorm(120,0.5,0.1),downstream=8,
                      seepageFraction =0.05,seepageCode=3,
                      geometry=list(deadStorage= 10 ,
                                    capacity= 20 ,
                                    ratingCurve= cbind(seq(0,90,10),seq(0,9,1))))
Auq1<-createAquifer(name="Aquifer1",area=50,label=3,
                    capacity=2000,rechargeTS=rnorm(120,10,3),
                    Sy=0.1,leakageFraction=0.02,leakageCode=6,priority=2)
D1<-createDemandSite(name ="Demand1",label=4,demandTS=NA,
                     demandParams=list(annualUseRate=0.01,
                                       annualVariation=annualVariation,
                                       cropArea=2000),
                     returnFlowFraction=0.2,suppliers=1,
                     downstream=2,priority=1)
D2<-createDemandSite(name ="Demand2",label=5,demandTS=NA,
                     demandParams=list(annualUseRate=0.01,
                                       annualVariation=annualVariation,
                                       cropArea=4000),
                     returnFlowFraction=0.2,suppliers=1,
                     downstream=2,priority=2)
D3<-createDemandSite(name ="Demand3",label=6,demandTS=NA,
                     demandParams=list(annualUseRate=0.01,
                                       annualVariation=annualVariation,
                                       cropArea=8000),
                     returnFlowFraction=0.2,suppliers=c(2,3),
                     downstream=8,priority=2)
D4<-createDemandSite(name ="Demand4",label=7,demandTS=NA,
                     demandParams=list(annualUseRate=0.01,
                                       annualVariation=annualVariation,
                                       cropArea=3000),
                     returnFlowFraction=0.2,suppliers=2,
                     downstream=8,priority=1)
J1<-createJunction(name="junction1",label=8,downstream=NA)
area<-createArea(name="unknown",location="unknown",
                 simulation=list(start=c(1900,1),end=c(1910,01)))
area<-addObjectToArea(area,R1)
area<-addObjectToArea(area,Res1)
area<-addObjectToArea(area,Auq1)
area<-addObjectToArea(area,D1)
area<-addObjectToArea(area,D2)
area<-addObjectToArea(area,D3)
area<-addObjectToArea(area,D4)
area<-addObjectToArea(area,J1)
\dontrun{
simulated<-sim(area)
plot(simulated)
}


#--------------------6th Example--------------------
R1<-createRiver(name="river1",label=1,
                downstream=3,discharge=rnorm(120,12,3))
R2<-createRiver(name="river2",label=2,
                downstream=4,discharge=rnorm(120,12,3))
R8<-createRiver(name="river8",label=8,
                downstream=10,discharge=rnorm(120,5,1))
R9<-createRiver(name="river9",label=9,
                downstream=11,discharge=rnorm(120,5,1))
Res3<-createReservoir(name="res3",label=3,priority=1,
                      netEvaporation=rnorm(120,0.5,0.1),downstream =8 ,
                      geometry=list(deadStorage= 10 ,
                      capacity= 90 ,
                      ratingCurve= cbind(seq(0,90,10),seq(0,9,1))))
Res4<-createReservoir(name="res4",label=4,priority=2,
                      netEvaporation=rnorm(120,0.5,0.1),downstream =9 ,
                      geometry=list(deadStorage= 10 ,
                                    capacity= 90 ,
                                    ratingCurve= cbind(seq(0,90,10),seq(0,9,1))))
annualVariation<-round(sin(seq(0,pi,length.out=12))*
                       100/sum(sin(seq(0,pi,length.out=12))))
D5<-createDemandSite(name ="Agri5",label=5,demandTS=NA,
                     demandParams=list(annualUseRate=0.01,
                                       annualVariation=annualVariation,
                                       cropArea=500),
                     returnFlowFraction =0.2,
                     suppliers=3,downstream=10,priority=1)
D6<-createDemandSite(name ="Agri6",label=6,demandTS=NA,
                     demandParams=list(annualUseRate=0.01,
                                       annualVariation=annualVariation,
                                       cropArea=700),
                     returnFlowFraction =0.2,suppliers=c(3,4),
                     downstream=10,priority=2)
D7<-createDemandSite(name ="Agri7",label=7,demandTS=NA,
                     demandParams=list(annualUseRate=0.01,
                                       annualVariation=annualVariation,
                                       cropArea=600),
                     returnFlowFraction =0.2,suppliers=4,
                     downstream=11,priority=3)
D13<-createDemandSite(name ="Agri13",label=13,demandTS=NA,
                      demandParams=list(annualUseRate=0.01,
                                        annualVariation=annualVariation,
                                        cropArea=300),
                      returnFlowFraction =0.2,suppliers=14,
                      downstream=10,priority=1)
J10<-createJunction(name="junc10",label=10,downstream=12)
J11<-createJunction(name="junc11",label=11,downstream=12)
Div12<-createDiversion(name="Div12",label=12,capacity=10,
                       divertTo=14,downstream=NA)
Auq14<-createAquifer(name="Aquifer14",area=100,label=14,
                     capacity=5000,rechargeTS=rnorm(120,10,3),
                     Sy=0.1,leakageFraction=0.02,leakageCode=10,priority=1)
area<-createArea(name="unknown",location="unknown",
                 simulation=list(start=c(1900,1),end=c(1910,01)))
area<-addObjectToArea(area,R1)
area<-addObjectToArea(area,R2)
area<-addObjectToArea(area,R8)
area<-addObjectToArea(area,R9)
area<-addObjectToArea(area,Res3)
area<-addObjectToArea(area,Res4)
area<-addObjectToArea(area,D5)
area<-addObjectToArea(area,D6)
area<-addObjectToArea(area,D7)
area<-addObjectToArea(area,D13)
area<-addObjectToArea(area,Div12)
area<-addObjectToArea(area,Auq14)
area<-addObjectToArea(area,J10)
area<-addObjectToArea(area,J11)
simulated<-sim(area)
\dontrun{
plot(simulated)
}
}

\keyword{ list }
\keyword{ matrix }
