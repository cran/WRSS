plot.sim <-
function(x,...)
{
   readkeygraph <- function(prompt)
   {
      getGraphicsEvent(prompt = prompt, 
                       onMouseDown = NULL,
                       onMouseMove = NULL,
                       onMouseUp = NULL,
                       onKeybd = onKeybd,
                       consolePrompt = "[press any key to continue.....]")
      Sys.sleep(0.01)
      return(keyPressed)
   }
   onKeybd <- function(key) keyPressed <<- key

   nRes<-length(x$operation$operation$reservoirs)
   nRiv<-length(x$operation$operation$rivers)
   nAuq<-length(x$operation$operation$aquifers)
   nJun<-length(x$operation$operation$junctions)
   nDiv<-length(x$operation$operation$diversions)
   nDem<-length(x$operation$operation$demands)
   simulation<- x$operation$operation$simulation
   duration<-sum((simulation$end-simulation$start)*c(12,1))
   months<-c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
   
   getMonthlyMean<-function(data)
   {
      mat<-t(matrix(NA,(floor(length(data)/12)+1),12))
      mat[simulation$start[2]:(simulation$start[2]+duration-1)]<-data
      mat<-t(mat)
      out<-apply(mat,2,mean,na.rm=TRUE)
      names(out)<-months
      return(out)
   }

   if(nRes>0)
   {
      for(i in 1:nRes)
      {
         inflow  <-x$operation$operation$reservoirs[[i]]$operation$inflow
         outflow <-x$operation$operation$reservoirs[[i]]$operation$outflow
         capacity<-x$operation$operation$reservoirs[[i]]$operation$geometry$capacity
         Storage    <-getMonthlyMean(x$operation$operation$reservoirs[[i]]$operation$sim_result[,1])
         Spill      <-getMonthlyMean(x$operation$operation$reservoirs[[i]]$operation$sim_result[,2])
         Evaporation<-getMonthlyMean(x$operation$operation$reservoirs[[i]]$operation$sim_result[,3])
         Release    <-getMonthlyMean(x$operation$operation$reservoirs[[i]]$operation$sim_result[,4])
         Inflow     <-getMonthlyMean(apply(x$operation$operation$reservoirs[[i]]$operation$inflow,1,sum))
         Storage    <-Storage-Inflow
         title<-x$operation$operation$reservoirs[[i]]$operation$name
         bars<-t(cbind(Evaporation,Release,Storage,Inflow,Spill))
         ylim<-c(0,max(apply(bars,2,sum)))*(1+nrow(bars)*0.05)
         barplot(bars,las=2,col=1:5,ylab='Volume (MCM)',ylim=ylim,main=title)
         lines(0:15,rep(capacity,16),col=6,typ='o',lwd=2,pch=19)
         legend('top',
                legend=c('evaporation','release','storage','inflow','spill','capacity'),
                ncol=3,
                fill=c(1:5,NA),
                box.lwd=0,
                box.col=NA,
                lty=c(rep(0,5),1),
                col=1:6,
                lwd=c(rep(0,5),2),
                border=c(rep(1,5),NA))
         keyPressed = readkeygraph("[press any key to continue.....]")
         graphics.off()
      }
   }

   if(nRiv>0)
   {
      for(i in 1:nRiv)
      {
         inflow <-x$operation$operation$rivers[[i]]$operation$inflow
         outflow<-x$operation$operation$rivers[[i]]$operation$outflow
         I<-rep(NA,12)
         O<-rep(NA,12)
         for(j in 1:ncol(inflow)) {I<-cbind(I,getMonthlyMean(inflow [,j]))};I<-I[,-1,drop=FALSE]
         for(j in 1:ncol(outflow)){O<-cbind(O,getMonthlyMean(outflow[,j]))};O<-O[,-1,drop=FALSE]
         I<-t(I);O<-t(O)
         ylim<-c(0,max(apply(I,2,sum),apply(O,2,sum)))*(1+(ncol(inflow)+ncol(outflow))*0.1)
         title<-x$operation$operation$rivers[[i]]$operation$name
         barplot(I,ylim=ylim,ylab='Volume (MCM)',las=2,main=title,col=gray((1:ncol(inflow))/(1.2*ncol(inflow))))
         for(j in 1:nrow(O)){lines(seq(.7,13.9,1.2),O[j,],typ='o',pch=21,bg='white',col=j+1)}
         legend('top',
                legend=c(colnames(inflow),colnames(outflow)),
                ncol=2,
                fill=c(gray((1:ncol(inflow))/(1.2*ncol(inflow))),rep(NA,ncol(outflow))),
                lty=c(rep(0,ncol(inflow)),rep(1,ncol(outflow))),
                col=c(rep(1,ncol(inflow)),2:(ncol(outflow)+1)),
                border=c(rep(1,ncol(inflow)),rep(NA,ncol(outflow))),
                box.lwd=0,box.col=NA)
         keyPressed = readkeygraph("[press any key to continue.....]")
         graphics.off()
      }
   }

   if(nAuq>0)
   {
      for(i in 1:nAuq)
      {
         par(dev.new(width = 16, height = 8),mfrow=c(1,2))
         inflow <-x$operation$operation$aquifers[[i]]$operation$inflow
         outflow<-x$operation$operation$aquifers[[i]]$operation$outflow
         storage<-x$operation$operation$aquifers[[i]]$operation$storage
         I<-rep(NA,12)
         O<-rep(NA,12)
         for(j in 1:ncol(inflow)) {I<-cbind(I,getMonthlyMean(inflow [,j]))};I<-I[,-1,drop=FALSE]
         for(j in 1:ncol(outflow)){O<-cbind(O,getMonthlyMean(outflow[,j]))};O<-O[,-1,drop=FALSE]
         I<-t(I);O<-t(O)
         
         ylim<-c(0,max(apply(I,2,sum),apply(O,2,sum)))*(1+(ncol(inflow)+ncol(outflow))*0.1)
         title<-x$operation$operation$aquifers[[i]]$operation$name
         barplot(I,ylim=ylim,ylab='Volume (MCM)',las=2,main=title,col=gray((1:ncol(inflow))/(1.2*ncol(inflow))))
         for(j in 1:nrow(O)){lines(seq(.7,13.9,1.2),O[j,],typ='o',pch=21,bg='white',col=j+1)}
         legend('top',
                legend=c(colnames(inflow),colnames(outflow)),
                ncol=2,
                fill=c(gray((1:ncol(inflow))/(1.2*ncol(inflow))),rep(NA,ncol(outflow))),
                lty=c(rep(0,ncol(inflow)),rep(1,ncol(outflow))),
                col=c(1:ncol(inflow),2:(ncol(outflow)+1)),
                border=c(rep(1,ncol(inflow)),rep(NA,ncol(outflow))),
                box.lwd=0,box.col=NA)

         inflow <-apply(inflow,1,sum)
         outflow<-apply(outflow,1,sum)
         ylim<-c(0,max(inflow+outflow))*1.2
         par(mar = c(5, 4, 4, 4) + 0.3)
         inflow<-ts(inflow,start=simulation$start,frequency=12)
         outflow<-ts(outflow,start=simulation$start,frequency=12)
         barplot(rbind(inflow,outflow),col=gray(c(1,3)/4),ylab='Volume of inflow & outflow (MCM)',xlab='Time',ylim=ylim)
         par(new = TRUE)
         plot(storage,axes = FALSE, bty = "n", xlab = "", ylab = "",typ='o',col=2,pch=21,bg='white')
         axis(side=4, at = pretty(range(storage)))
         mtext("Aquifer storage (MCM)", side=4, line=3)
         legend('top',
                legend=c('inflow','outflow','storage'),
                ncol=3,
                fill=c(gray(c(1,3)/4),NA),
                lty=c(0,0,1),
                col=c(1,1,2),
                border=c(1,1,NA),
                box.lwd=0,box.col=NA)
         keyPressed = readkeygraph("[press any key to continue.....]")
         graphics.off()
      }
   }

   if(nDiv>0)
   {
      for(i in 1:nDiv)
      {
         par(dev.new(width = 16, height = 8),mfrow=c(1,2))
         inflow  <-x$operation$operation$diversions[[i]]$operation$inflow
         outflow <-x$operation$operation$diversions[[i]]$operation$outflow
         diverted<-x$operation$operation$diversions[[i]]$operation$sim_result$diverted
         overflow<-x$operation$operation$diversions[[i]]$operation$sim_result$overflow
         I<-rep(NA,12)
         O<-getMonthlyMean(overflow)
         D<-getMonthlyMean(diverted)
         for(j in 1:ncol(inflow)) {I<-cbind(I,getMonthlyMean(inflow [,j]))};I<-I[,-1,drop=FALSE]
         I<-apply(I,1,sum)
         ylim<-c(0,max(apply(rbind(I,O,D),2,sum)))*1.1
         title<-x$operation$operation$diversions[[i]]$operation$name
         barplot(rbind(I,O,D),las=2, ylab=c('Volume (MCM)'),ylim=ylim,col=gray(1:3/4),main=title,)
         legend('top',
                legend=c('inflow','outflow','diverted'),
                ncol=3,
                fill=c(gray(c(1:3)/4),NA),
                box.lwd=0,box.col=NA)
         plot(ecdf(diverted)(seq(min(diverted)-0.01,max(diverted)+0.01,0.01)),
                             seq(min(diverted)-0.01,max(diverted)+0.01,0.01),
              typ='l',xlab='Probability',ylab='Diverted Volume (MCM)')
         keyPressed = readkeygraph("[press any key to continue.....]")
         graphics.off()
      }
   }

   if(nJun>0)
   {
      for(i in 1:nJun)
      {
         inflow  <-x$operation$operation$junctions[[i]]$operation$inflow
         outflow <-x$operation$operation$junctions[[i]]$operation$outflow
         I<-rep(NA,12)
         for(j in 1:ncol(inflow)) {I<-cbind(I,getMonthlyMean(inflow [,j]))};I<-I[,-1,drop=FALSE]
         O<-getMonthlyMean(outflow)
         ylim<-c(0,max(I,O))*(1+0.05*(ncol(I)+1))
         title<-x$operation$operation$junctions[[i]]$operation$name
         plot(I[,1],xaxt='n',ylab='Volume (MCM)',ylim=ylim,typ='o',xlab='',pch=0,main=title)
         axis(1, at=1:12, labels=months,las=2)
         if(ncol(inflow)>1)
         {
            for(j in 2:ncol(I))
            {
               lines(I[,j],col=j,pch=j-1,bg='white',typ='o')
            }
         }
         lines(O,typ='o',col=j+1,pch=j,bg='white')
         name<-c(colnames(inflow),colnames(outflow))
         legend('top',
                legend=name,
                ncol=2,
                col=1:(j+1),
                pch=0:j,
                box.lwd=0,box.col=NA)
         keyPressed = readkeygraph("[press any key to continue.....]")
         graphics.off()
      }
   }

   if(nDem>0)
   {
      for(i in 1:nDem)
      {
         outflow <-x$operation$operation$demands[[i]]$operation$outflow
         demandTS<-getMonthlyMean(x$operation$operation$demands[[i]]$operation$demandTS)
         if(ncol(outflow)>1)
         {
            inflow  <-x$operation$operation$demands[[i]]$operation$inflow
            out<-as.matrix(apply(outflow,1,sum))
            colnames(out)<-colnames(outflow)[1]
            O<-getMonthlyMean(out)
            I<-rep(NA,12)
            for(j in 1:ncol(inflow)) {I<-cbind(I,getMonthlyMean(inflow [,j]))};I<-t(I[,-1,drop=FALSE])
            ylim<-c(0,max(apply(rbind(I,O),2,sum),demandTS))*1.1
            title<-x$operation$operation$demands[[i]]$operation$name
            col<-gray(1:(ncol(inflow)+1)/(ncol(inflow)+1))
            barplot(I,las=2, ylab=c('Volume (MCM)'),ylim=ylim,main=title,col=col[1:ncol(inflow)])
            lines(seq(0.7,13.9,1.2),demandTS,typ='o',pch=21,bg='white',col=2)
            barplot(O,las=2, ylab=c('Volume (MCM)'),ylim=ylim,main=title,col=col[length(col)],add=TRUE)
            legend('top',
                   legend=c(colnames(inflow),colnames(outflow)[2],'demand'),
                   ncol=2,
                   fill=c(col,NA),
                   lty=c(rep(0,ncol(inflow)+1),1),
                   col=c(rep(0,ncol(inflow)+1),2),
                   border=c(rep(1,ncol(inflow)+1),NA),
                   box.lwd=0,box.col=NA)
         }else{
            inflow  <-x$operation$operation$demands[[i]]$operation$inflow
            I<-rep(NA,12)
            for(j in 1:ncol(inflow)) {I<-cbind(I,getMonthlyMean(inflow [,j]))};I<-t(I[,-1,drop=FALSE])
            ylim<-c(0,max(apply(I,2,sum),demandTS))*1.1
            title<-x$operation$operation$demands[[i]]$operation$name
            col<-gray(1:ncol(inflow)/ncol(inflow))
            barplot(I,las=2, ylab=c('Volume (MCM)'),ylim=ylim,main=title,col=col)
            lines(seq(0.7,13.9,1.2),demandTS,typ='o',pch=21,bg='white',col=2)
            legend('top',
                   legend=c(colnames(inflow),'demand'),
                   ncol=2,
                   fill=c(col,NA),
                   lty=c(rep(0,ncol(inflow)),1),
                   col=c(rep(0,ncol(inflow)),2),
                   border=c(rep(1,ncol(inflow)),NA),
                   box.lwd=0,box.col=NA)
            
         }
         keyPressed = readkeygraph("[press any key to continue.....]")
         graphics.off()
      }
   }
}
