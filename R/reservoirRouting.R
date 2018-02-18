reservoirRouting <-
function(demand       ,
              priority                  ,
              inflow                    ,
              netEvaporation            ,
              geometry=list(deadStorage=NULL ,
                            capacity=NULL    ,
                            ratingCurve=NULL),
              initialStorage=NA         ,
              seepageFraction=NA        ,
              simulation)
{
   duration<-(simulation$end[1]-simulation$start[1])*12+(simulation$end[2]-simulation$start[2])
   if(is.na(seepageFraction)){seepageFraction<-0}
   spill  <-rep(0,duration)
   storage<-rep(0,duration)
   loss   <-rep(0,duration)
   seepage<-rep(0,duration)
   release<-matrix(0,duration,ncol(demand))
   demand<-as.matrix(demand)
   if(nrow(demand)>duration)
   {
      demand<-demand[1:duration,]
   }
   if(nrow(demand)<duration)
   {
      demand<-rbind(demand,matrix(0,duration-nrow(demand),ncol(demand)))
   }
   D<-apply(demand,1,sum)
   R<-rep(0,duration)
   storage[1]<-ifelse(is.na(initialStorage),geometry$capacity,initialStorage)
   for(iter in 1:ifelse(is.na(initialStorage),10,1))
   {
      if(iter>1){storage[1]<-storage[duration]}
      if((inflow[1]+storage[1]-D[1])>geometry$capacity)
      {
         seep1<-storage[1]*seepageFraction
         a1<-approxExtrap(x=geometry$ratingCurve[,1],y=geometry$ratingCurve[,2],xout=storage[1])$y
         spill[1]<-inflow[1]+storage[1]-geometry$capacity
         R[1]<-D[1] 
         storage[1]<-geometry$capacity
         seep2<-storage[1]*seepageFraction
         a2<-approxExtrap(x=geometry$ratingCurve[,1],y=geometry$ratingCurve[,2],xout=storage[1])$y
         seepage[1]<-(seep1+seep2)/2
         loss[1]<-netEvaporation[1]*(a1+a2)/2
         if((storage[1]-seepage[1]-loss[1])<0){seepage[1]<-seepage[1]-(seepage[1]+loss[1]-storage[1])*seepage[1]/(seepage[1]+loss[1]);loss[1]<-loss[1]-(seepage[1]+loss[1]-storage[1])*loss[1]/(seepage[1]+loss[1])}
         storage[1]<-storage[1]-seepage[1]-loss[1]
      }
      if((inflow[1]+storage[1]-D[1])<geometry$deadStorage)
      {
         seep1<-storage[1]*seepageFraction
         a1<-approxExtrap(x=geometry$ratingCurve[,1],y=geometry$ratingCurve[,2],xout=storage[1])$y
         if((inflow[1]+storage[1])<geometry$deadStorage){R[1]<-0}else{R[1]<-inflow[1]+storage[1]-geometry$deadStorage;storage[1]<-geometry$deadStorage}
         seep2<-storage[1]*seepageFraction
         a2<-approxExtrap(x=geometry$ratingCurve[,1],y=geometry$ratingCurve[,2],xout=storage[1])$y
         seepage[1]<-(seep1+seep2)/2
         loss[1]<-netEvaporation[1]*(a1+a2)/2
         if((storage[1]-seepage[1]-loss[1])<0){seepage[1]<-seepage[1]-(seepage[1]+loss[1]-storage[1])*seepage[1]/(seepage[1]+loss[1]);loss[1]<-loss[1]-(seepage[1]+loss[1]-storage[1])*loss[1]/(seepage[1]+loss[1])}
         storage[1]<-storage[1]-seepage[1]-loss[1]
         spill[1]<-0
      }
      if((inflow[1]+storage[1]-D[1])>geometry$deadStorage && (inflow[1]+storage[1]-D[1])<geometry$capacity)
      {
         seep1<-storage[1]*seepageFraction
         a1<-approxExtrap(x=geometry$ratingCurve[,1],y=geometry$ratingCurve[,2],xout=storage[1])$y
         R[1]<-D[1]
         storage[1]<-inflow[1]+storage[1]-R[1]
         seep2<-storage[1]*seepageFraction
         a2<-approxExtrap(x=geometry$ratingCurve[,1],y=geometry$ratingCurve[,2],xout=storage[1])$y
         loss[1]<-netEvaporation[1]*(a1+a2)/2
         seepage[1]<-(seep1+seep2)/2
         if((storage[1]-seepage[1]-loss[1])<0){seepage[1]<-seepage[1]-(seepage[1]+loss[1]-storage[1])*seepage[1]/(seepage[1]+loss[1]);loss[1]<-loss[1]-(seepage[1]+loss[1]-storage[1])*loss[1]/(seepage[1]+loss[1])}
         storage[1]<-storage[1]-seepage[1]-loss[1]
         spill[1]<-0
      }
      for(t in 2:duration)
      {
         if((inflow[t]+storage[t-1]-D[t])>geometry$capacity)
         {
            seep1<-storage[t-1]*seepageFraction
            a1<-approxExtrap(x=geometry$ratingCurve[,1],y=geometry$ratingCurve[,2],xout=storage[t-1])$y
            spill[t]<-inflow[t]+storage[t-1]-geometry$capacity
            R[t]<-D[t]
            storage[t]<-geometry$capacity
            seep2<-storage[t]*seepageFraction
            a2<-approxExtrap(x=geometry$ratingCurve[,1],y=geometry$ratingCurve[,2],xout=storage[t])$y
            seepage[t]<-(seep1+seep2)/2
            loss[t]<-(netEvaporation[t]+netEvaporation[t-1])*(a1+a2)/4
            if((storage[t]-seepage[t]-loss[t])<0){seepage[t]<-seepage[t]-(seepage[t]+loss[t]-storage[t])*seepage[t]/(seepage[t]+loss[t]);loss[t]<-loss[t]-(seepage[t]+loss[t]-storage[t])*loss[t]/(seepage[t]+loss[t])}
            storage[t]<-storage[t]-loss[t]-seepage[t]
         }
         if((inflow[t]+storage[t-1]-D[t])<geometry$deadStorage)
         {
            seep1<-storage[t-1]*seepageFraction
            a1<-approxExtrap(x=geometry$ratingCurve[,1],y=geometry$ratingCurve[,2],xout=storage[t-1])$y
            if((inflow[t]+storage[t-1])<geometry$deadStorage){R[t]<-0;storage[t]<-storage[t-1]}else{R[t]<-inflow[t]+storage[t-1]-geometry$deadStorage;storage[t]<-geometry$deadStorage}
            seep2<-storage[t]*seepageFraction
            a2<-approxExtrap(x=geometry$ratingCurve[,1],y=geometry$ratingCurve[,2],xout=storage[t])$y
            seepage[t]<-(seep1+seep2)/2
            loss[t]<-(netEvaporation[t]+netEvaporation[t-1])*(a1+a2)/4
            if((storage[t]-seepage[t]-loss[t])<0){seepage[t]<-seepage[t]-(seepage[t]+loss[t]-storage[t])*seepage[t]/(seepage[t]+loss[t]);loss[t]<-loss[t]-(seepage[t]+loss[t]-storage[t])*loss[t]/(seepage[t]+loss[t])}
            storage[t]<-storage[t]-loss[t]-seepage[t]
            spill[t]<-0
         }
         if((inflow[t]+storage[t-1]-D[t])>geometry$deadStorage && (inflow[t]+storage[t-1]-D[t])<geometry$capacity)
         {
            seep1<-storage[t-1]*seepageFraction
            a1<-approxExtrap(x=geometry$ratingCurve[,1],y=geometry$ratingCurve[,2],xout=storage[t-1])$y
            R[t]<-D[t]
            storage[t]<-inflow[t]+storage[t-1]-R[t]
            seep2<-storage[t]*seepageFraction
            a2<-approxExtrap(x=geometry$ratingCurve[,1],y=geometry$ratingCurve[,2],xout=storage[t])$y
            loss[t]<-(netEvaporation[t]+netEvaporation[t-1])*(a1+a2)/4
            seepage[t]<-(seep1+seep2)/2
            if((storage[t]-seepage[t]-loss[t])<0){seepage[t]<-seepage[t]-(seepage[t]+loss[t]-storage[t])*seepage[t]/(seepage[t]+loss[t]);loss[t]<-loss[t]-(seepage[t]+loss[t]-storage[t])*loss[t]/(seepage[t]+loss[t])}
            storage[t]<-storage[t]-loss[t]-seepage[t]
            spill[t]<-0
         }
      }
   }

   if(all(is.na(priority)))
   {
      for(i in 1:ncol(demand))
      {
         release[,i]<-ifelse(R>demand[,i],demand[,i],R)
         R<-R-release[,i]
      }
   }else{
      sorted<-sort(priority,index.return = TRUE)
      priority_index<-sorted$ix
      unique_priority<-sort(unique(priority))
      if(length(unique_priority)<length(priority))
      {
         merged_Demand<-matrix(NA,duration,length(unique_priority))
         for(d in 1:length(unique_priority)){merged_Demand[,d]<-apply(as.matrix(demand[,which(!is.na(match(priority,unique_priority[d])))]),1,sum)}
         release<-matrix(0,duration,ncol(merged_Demand))
         for(d in 1:length(unique_priority)){release[,d]<-ifelse(R>merged_Demand[,d],merged_Demand[,d],R);R<-R-release[,d]}
         Release<-matrix(NA,duration,ncol(demand))
         for(d in 1:length(unique_priority))
         {
            Release[,which(!is.na(match(priority,unique_priority[d])))]<-
                           release[,d]*
                           (as.matrix(demand[,which(!is.na(match(priority,unique_priority[d])))])/
                           apply(as.matrix(demand[,which(!is.na(match(priority,unique_priority[d])))]),1,sum))
         }
         if(!is.null(colnames(demand))){colnames(Release)<-colnames(demand)}
      }else{
         Release<-matrix(NA,duration,ncol(demand))
         if(!is.null(colnames(demand))){colnames(Release)<-colnames(demand)}
         demand<-demand[,priority_index,drop=FALSE]
         release<-matrix(0,duration,ncol(demand))
         for(d in 1:length(unique_priority)){release[,d]<-ifelse(R>demand[,d],demand[,d],R);R<-R-release[,d]}
         for(d in 1:length(unique_priority)){Release[,which(!is.na(match(priority,unique_priority[d])))]<-release[,d]}
      }
   }
   Release[which(is.nan(Release[,1])),]<-0
   if(seepageFraction==0)
   {
      return(list(release=Release,spill=spill,storage=storage,loss=loss))
   }else{
      return(list(release=Release,spill=spill,storage=storage,loss=loss,seepage=seepage))
   }
}
