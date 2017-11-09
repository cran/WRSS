aquiferRouting <-
function(demand           ,
              priority        =NA         ,
              area                        ,
              capacity                    ,
              rechargeTS      =NA         ,
              leakageFraction =NA         ,
              initialStorage  =NA         ,
              Sy                          ,
              simulation)
{
   duration<-(simulation$end[1]-simulation$start[1])*12+(simulation$end[2]-simulation$start[2])
   actual_capacity<-capacity*Sy
   if(nrow(demand)>duration)
   {
      demand<-demand[1:duration,]
   }
   if(nrow(demand)<duration)
   {
      demand<-rbind(demand,matrix(0,duration-nrow(demand),ncol(demand)))
   }
   leakage  <-rep(0,duration)
   storage<-rep(0,duration)
   energy<-rep(0,duration)
   release<-matrix(0,duration,ncol(demand))
   demand<-as.matrix(demand)
   D<-apply(demand,1,sum)
   R<-rep(0,duration)
   storage[1]<-ifelse(is.na(initialStorage),actual_capacity,initialStorage)

   for(iter in 1:ifelse(is.na(initialStorage),10,1))
   {
     if(iter>1)
     {
        storage[1]<-storage[duration]
     }
     if((rechargeTS[1]+storage[1]-D[1])>actual_capacity)
     {
       R[1]<-D[1]
       leakage[1]<-rechargeTS[1]+storage[1]-D[1]-actual_capacity + actual_capacity*leakageFraction
       storage[1]<-actual_capacity-storage[1]*leakageFraction
     }
     if((rechargeTS[1]+storage[1]-D[1])<actual_capacity && (rechargeTS[1]+storage[1]-D[1])>0)
     {
       R[1]<-D[1]
       storage[1]<-rechargeTS[1]+storage[1]-D[1]
       leakage[1]<-storage[1]*leakageFraction
       storage[1]<-storage[1]-leakage[1]
     }
     if((rechargeTS[1]+storage[1])<D[1])
     {
       R[1]<-rechargeTS[1]+storage[1]
       storage[1]<-0
       leakage[1]<-storage[1]*leakageFraction
     }
     energy[1]<-storage[1]/area/Sy
    
     for(t in 2:duration)
     {
        if((rechargeTS[t]+storage[t-1]-D[t])>actual_capacity)
        {
           R[t]<-D[t]
           leakage[t]<-rechargeTS[t]+storage[t-1]-D[t]-actual_capacity+actual_capacity*leakageFraction
           storage[t]<-actual_capacity-leakage[t]
        }
        if((rechargeTS[t]+storage[t-1]-D[t])<actual_capacity && (rechargeTS[t]+storage[t-1]-D[t])>0)
        {
           R[t]<-D[t]
           leakage[t]<-storage[t-1]*leakageFraction
           storage[t]<-rechargeTS[t]+storage[t-1]-R[t]-leakage[t]
        }
        if((rechargeTS[t]+storage[t-1])<D[t])
        {
           R[t]<-rechargeTS[t]+storage[t-1]-storage[t-1]*leakageFraction
           leakage[t]<-storage[t-1]*leakageFraction
           storage[t]<-0
        }
        energy[t]<-storage[t]/area/Sy
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
         demand<-as.matrix(demand[,priority_index])
         release<-matrix(0,duration,ncol(demand))
         for(d in 1:length(unique_priority)){release[,d]<-ifelse(R>demand[,d],demand[,d],R);R<-R-release[,d]}
         for(d in 1:length(unique_priority)){Release[,which(!is.na(match(priority,unique_priority[d])))]<-release[,d]}
      }
   }
    Release[which(is.nan(Release[,1])),]<-0
    return(list(release=Release,leakage=leakage,storage=storage))
}
