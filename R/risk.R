risk<-
function(object , s.const = 0.95)
{
   nDem<-length(object$operation$operation$demands)
   C<-matrix(NA,3,nDem)
   rownames(C)<-c("Vulnerability","Reliability","Resiliency")
   name<-c()
   for(i in 1:nDem){name<-c(name,object$operation$operation$demands[[i]]$operation$name)}
   colnames(C)<-name
   Base<-function(Re,De)
   {
      Criaterian<-rep(0,3)
      names(Criaterian)<-c("Vulnerability","Reliability","Resiliency")
  
      if(sum(De==0)>0)
      {
        De[which(De==0)]<-0.001
      }
      T<-length(Re)
      failure<-rep(NA,T)
      
      # Vulnerability
      Criaterian[1]<-sum((De-Re)/De)
      
      # Reliability
      Criaterian[2]<-1-sum(Re<s.const*De)/T
      
      # Resiliency
      failure[which(Re<s.const*De)]<-0
      failure[which(Re>s.const*De)]<-1
      f<-sum(diff(failure)==1)
      F<-sum(failure==0)
      Criaterian[3]<-ifelse(is.nan(f/F),1,f/F)
      
      return (Criaterian)
   }
   for(i in 1:nDem)
   {
      D<-object$operation$operation$demands[[i]]$operation$demandTS
      R<-apply(object$operation$operation$demands[[i]]$operation$inflow,1,sum)
      inflow<-ifelse(R>D,D,R)
      C[,i]<-Base(Re=R,De=D)
   }
   return(C)
}
