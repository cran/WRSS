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
  \item{area}{An object inherited from \code{\link[WRSS]{createArea}}}
  \item{object}{An objects inherited from any of the following constructors: \code{\link[WRSS]{createAquifer}} , \code{\link[WRSS]{createRiver}}, \code{\link[WRSS]{createReservoir}}, \code{\link[WRSS]{createJunction}}, \code{\link[WRSS]{createDiversion}}, and \code{\link[WRSS]{createDemandSite}}.}
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
R<-createRiver(name="river1",label=1,downstream=2,discharge=rnorm(120,5,1.5))
Res<-createReservoir(name="res3",label=2,type='storage',
                     priority=1,netEvaporation=rnorm(120,0.5,0.1),downstream =NA ,
                     geometry=list(deadStorage= 10 ,capacity= 90 ,
                     storageAreaTable= cbind(seq(0,90,10),seq(0,9,1))))
waterVariation<-round(sin(seq(0,pi,length.out=12))*
                       100/sum(sin(seq(0,pi,length.out=12))))
D<-createDemandSite(name ="Agri5",label=5,
                    demandParams=list(waterUseRate=1,
                                      waterVariation=waterVariation,
                                      cropArea=1000),
                    returnFlowFraction =0.2,suppliers=2,
                    downstream=NA,priority=1)
area<-createArea(name="unknown",location="unknown",
                 simulation=list(start='2000-01-01',
                                 end  ='2000-04-29',
                                 interval='day'))
area<-addObjectToArea(area,R)
area<-addObjectToArea(area,Res)
area<-addObjectToArea(area,D)
\dontrun{
plot(area)
simulated<-sim(area)
plot(simulated)
}


#--------------------2nd Example--------------------
R<-createRiver(name="river1",label=1,downstream=2,discharge=rnorm(120,5,1.5))
Res<-createReservoir(name="res3",label=2,type='storage',
                     priority=1,netEvaporation=rnorm(120,0.5,0.1),downstream =NA ,
                     geometry=list(deadStorage= 10 ,capacity= 90 ,
                     storageAreaTable= cbind(seq(0,90,10),seq(0,9,1))))
waterVariation<-round(sin(seq(0,pi,length.out=12))*
                       100/sum(sin(seq(0,pi,length.out=12))))
D1<-createDemandSite(name ="Agri1",label=3,
                    demandParams=list(waterUseRate=1,
                                      waterVariation=waterVariation,
                                      cropArea=1000),
                    returnFlowFraction =0.2,suppliers=2,
                    downstream=NA,priority=1)
D2<-createDemandSite(name ="Agri2",label=4,
                    demandParams=list(waterUseRate=1,
                                      waterVariation=waterVariation,
                                      cropArea=1000),
                    returnFlowFraction =0.2,suppliers=2,
                    downstream=NA,priority=1)
area<-createArea(name="unknown",location="unknown",
                 simulation=list(start='2000-01-01',
                                 end  ='2000-04-29',
                                 interval='day'))
area<-addObjectToArea(area,R)
area<-addObjectToArea(area,Res)
area<-addObjectToArea(area,D1)
area<-addObjectToArea(area,D2)
\dontrun{
plot(area)
simulated<-sim(area)
plot(simulated)
}

#--------------------3rd Example--------------------
R1<-createRiver(name="river1",label=1,downstream=3,discharge=rnorm(120,5,1.5))
R2<-createRiver(name="river2",label=2,downstream=4,discharge=rnorm(120,5,1.5))
Res1<-createReservoir(name="res1",label=3,type='storage',
                     priority=1,netEvaporation=rnorm(120,0.5,0.1),downstream =5 ,
                     geometry=list(deadStorage= 10 ,capacity= 90 ,
                     storageAreaTable= cbind(seq(0,90,10),seq(0,9,1))))
Res2<-createReservoir(name="res2",label=4,type='storage',
                     priority=2,netEvaporation=rnorm(120,0.5,0.1),downstream =5 ,
                     geometry=list(deadStorage= 10 ,capacity= 90 ,
                     storageAreaTable= cbind(seq(0,90,10),seq(0,9,1))))
J1<-createJunction(name="j1",label=5,downstream=NA)
waterVariation<-round(sin(seq(0,pi,length.out=12))*
                       100/sum(sin(seq(0,pi,length.out=12))))
D1<-createDemandSite(name ="Agri1",label=6,
                    demandParams=list(waterUseRate=1,
                                      waterVariation=waterVariation,
                                      cropArea=1000),
                    returnFlowFraction =0.2,suppliers=3,
                    downstream=5,priority=1)
D2<-createDemandSite(name ="Agri2",label=7,
                    demandParams=list(waterUseRate=1,
                                      waterVariation=waterVariation,
                                      cropArea=1000),
                    returnFlowFraction =0.2,suppliers=c(3,4),
                    downstream=5,priority=2)
D3<-createDemandSite(name ="Agri3",label=8,
                    demandParams=list(waterUseRate=1,
                                      waterVariation=waterVariation,
                                      cropArea=1000),
                    returnFlowFraction =0.2,suppliers=4,
                    downstream=5,priority=1)
area<-createArea(name="unknown",location="unknown",
                 simulation=list(start='2000-01-01',
                                 end  ='2000-04-29',
                                 interval='day'))
area<-addObjectToArea(area,R1)
area<-addObjectToArea(area,R2)
area<-addObjectToArea(area,Res1)
area<-addObjectToArea(area,Res2)
area<-addObjectToArea(area,D1)
area<-addObjectToArea(area,D2)
area<-addObjectToArea(area,D3)
area<-addObjectToArea(area,J1)
\dontrun{
plot(area)
simulated<-sim(area)
plot(simulated)
}

#--------------------4th Example--------------------
waterVariation<-round(sin(seq(0,pi,length.out=12))*
                       100/sum(sin(seq(0,pi,length.out=12))))
R1<-createRiver(name="river1",label=1,downstream=3,discharge=rnorm(120,5,1.5))
R2<-createRiver(name="river2",label=2,downstream=4,discharge=rnorm(120,5,1.5))
Res1<-createReservoir(name="res1",label=3,type='storage',
                     priority=1,netEvaporation=rnorm(120,0.5,0.1),downstream =5 ,
                     geometry=list(deadStorage= 10 ,capacity= 90 ,
                     storageAreaTable= cbind(seq(0,90,10),seq(0,9,1))))
Auq1<-createAquifer(name="Aquifer1",area=100,label=4,volume=5000,
                    rechargeTS=rnorm(120,10,3),Sy=0.1,
                    leakageFraction=0.02,leakageCode=5,priority=2)
J1<-createJunction(name="j1",label=5,downstream=NA)
D1<-createDemandSite(name ="Agri1",label=6,
                    demandParams=list(waterUseRate=1,
                                      waterVariation=waterVariation,
                                      cropArea=1000),
                    returnFlowFraction =0.2,suppliers=c(3,4),
                    downstream=5,priority=1)
D2<-createDemandSite(name ="Agri2",label=7,
                    demandParams=list(waterUseRate=1,
                                      waterVariation=waterVariation,
                                      cropArea=1000),
                    returnFlowFraction =0.2,suppliers=c(3,4),
                    downstream=5,priority=2)
D3<-createDemandSite(name ="Agri3",label=8,
                    demandParams=list(waterUseRate=1,
                                      waterVariation=waterVariation,
                                      cropArea=1000),
                    returnFlowFraction =0.2,suppliers=c(3,4),
                    downstream=5,priority=1)
area<-createArea(name="unknown",location="unknown",
                 simulation=list(start='2000-01-01',
                                 end  ='2000-04-29',
                                 interval='day'))
area<-addObjectToArea(area,R1)
area<-addObjectToArea(area,R2)
area<-addObjectToArea(area,Res1)
area<-addObjectToArea(area,Auq1)
area<-addObjectToArea(area,D1)
area<-addObjectToArea(area,D2)
area<-addObjectToArea(area,D3)
area<-addObjectToArea(area,J1)
\dontrun{
plot(area)
simulated<-sim(area)
plot(simulated)
}

#--------------------5th Example--------------------
waterVariation<-round(sin(seq(0,pi,length.out=12))*
                       100/sum(sin(seq(0,pi,length.out=12))))
R1<-createRiver(name="River1",label=1,
                downstream=2,discharge=rnorm(120,20,3),
                seepageFraction=0.1,seepageCode=3)
Res1<-createReservoir(name="res1",label=2,type='storage',
                     priority=1,netEvaporation=rnorm(120,0.5,0.1),downstream =NA ,
                     geometry=list(deadStorage= 10 ,capacity= 90 ,
                     storageAreaTable= cbind(seq(0,90,10),seq(0,9,1))))
Auq1<-createAquifer(name="Aquifer1",area=100,label=3,volume=5000,
                    rechargeTS=rnorm(120,10,3),Sy=0.1,
                    leakageFraction=0.02,leakageCode=7 ,priority=2)
J1<-createJunction(name="junction1",label=4,downstream=NA)
D1<-createDemandSite(name ="Agri1",label=5,
                    demandParams=list(waterUseRate=1,
                                      waterVariation=waterVariation,
                                      cropArea=1000),
                    returnFlowFraction =0.2,suppliers=1,
                    downstream=4,priority=1)
D2<-createDemandSite(name ="Agri2",label=6,
                    demandParams=list(waterUseRate=1,
                                      waterVariation=waterVariation,
                                      cropArea=1000),
                    returnFlowFraction =0.2,suppliers=1,
                    downstream=4,priority=2)
D3<-createDemandSite(name ="Agri3",label=7,
                    demandParams=list(waterUseRate=1,
                                      waterVariation=waterVariation,
                                      cropArea=1000),
                    returnFlowFraction =0.2,suppliers=c(2,3),
                    downstream=4,priority=2)
D4<-createDemandSite(name ="Agri4",label=8,
                    demandParams=list(waterUseRate=1,
                                      waterVariation=waterVariation,
                                      cropArea=1000),
                    returnFlowFraction =0.2,suppliers=2,
                    downstream=4,priority=1)
area<-createArea(name="unknown",location="unknown",
                 simulation=list(start='2000-01-01',
                                 end  ='2000-04-29',
                                 interval='day'))
area<-addObjectToArea(area,R1)
area<-addObjectToArea(area,Res1)
area<-addObjectToArea(area,Auq1)
area<-addObjectToArea(area,D1)
area<-addObjectToArea(area,D2)
area<-addObjectToArea(area,D3)
area<-addObjectToArea(area,D4)
area<-addObjectToArea(area,J1)
\dontrun{
plot(area)
simulated<-sim(area)
plot(simulated)
}


#--------------------6th Example--------------------
R1<-createRiver(name="river1",label=1,
                downstream=5,discharge=rnorm(120,12,3))
R2<-createRiver(name="river2",label=2,
                downstream=6,discharge=rnorm(120,12,3))
R3<-createRiver(name="river3",label=3,
                downstream=7,discharge=rnorm(120,5,1))
R4<-createRiver(name="river4",label=4,
                downstream=8,discharge=rnorm(120,5,1))
Res1<-createReservoir(name="res1",label=5,type='storage',
                     priority=1,netEvaporation=rnorm(120,0.5,0.1),downstream =3 ,
                     geometry=list(deadStorage= 10 ,capacity= 90 ,
                     storageAreaTable= cbind(seq(0,90,10),seq(0,9,1))))
Res2<-createReservoir(name="res2",label=6,type='storage',
                     priority=1,netEvaporation=rnorm(120,0.5,0.1),downstream =4 ,
                     geometry=list(deadStorage= 10 ,capacity= 90 ,
                     storageAreaTable= cbind(seq(0,90,10),seq(0,9,1))))
J1<-createJunction(name="junc1",label=7,downstream=NA)
J2<-createJunction(name="junc2",label=8,downstream=9)
Div1<-createDiversion(name="Div1",label=9,capacity=10,
                       divertTo=10,downstream=NA)
Auq1<-createAquifer(name="Aquifer1",area=100,label=10,volume=5000,
                    rechargeTS=rnorm(120,10,3),Sy=0.1,
                    leakageFraction=0.02,leakageCode=7)
waterVariation<-round(sin(seq(0,pi,length.out=12))*
                       100/sum(sin(seq(0,pi,length.out=12))))
D1<-createDemandSite(name ="Agri1",label=11,
                    demandParams=list(waterUseRate=1,
                                      waterVariation=waterVariation,
                                      cropArea=1000),
                    returnFlowFraction =0.2,suppliers=6,
                    downstream=8,priority=1)
D2<-createDemandSite(name ="Agri2",label=12,
                    demandParams=list(waterUseRate=1,
                                      waterVariation=waterVariation,
                                      cropArea=1000),
                    returnFlowFraction =0.2,suppliers=c(5,6),
                    downstream=8,priority=2)
D3<-createDemandSite(name ="Agri3",label=13,
                    demandParams=list(waterUseRate=1,
                                      waterVariation=waterVariation,
                                      cropArea=1000),
                    returnFlowFraction =0.2,suppliers=5,
                    downstream=7,priority=1)
D4<-createDemandSite(name ="Agri4",label=14,
                    demandParams=list(waterUseRate=1,
                                      waterVariation=waterVariation,
                                      cropArea=1000),
                    returnFlowFraction =0.2,suppliers=10,
                    downstream=7,priority=1)
area<-createArea(name="unknown",location="unknown",
                 simulation=list(start='2000-01-01',
                                 end  ='2000-04-29',
                                 interval='day'))
area<-addObjectToArea(area,R1)
area<-addObjectToArea(area,R2)
area<-addObjectToArea(area,R3)
area<-addObjectToArea(area,R4)
area<-addObjectToArea(area,Res1)
area<-addObjectToArea(area,Res2)
area<-addObjectToArea(area,D1)
area<-addObjectToArea(area,D2)
area<-addObjectToArea(area,D3)
area<-addObjectToArea(area,D4)
area<-addObjectToArea(area,Div1)
area<-addObjectToArea(area,Auq1)
area<-addObjectToArea(area,J1)
area<-addObjectToArea(area,J2)
simulated<-sim(area)
\dontrun{
plot(area)
plot(simulated)
}
}

\keyword{ list }
\keyword{ matrix }
