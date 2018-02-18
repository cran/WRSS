diversionRouting <-
function(demand  ,
              priority        =NA  ,
              capacity             ,
              inflow               ,
              simulation)
{
   duration<-(simulation$end[1]-simulation$start[1])*12+(simulation$end[2]-simulation$start[2])
   capacity<-capacity*3600*24*30.41/1000000
   R<-ifelse(inflow>capacity,capacity,inflow)
   overflow<-inflow-R

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
    return(list(release=Release,diverted=R+apply(Release,1,sum),overflow=overflow))
}
