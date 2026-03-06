createJunction.base <-
function(name,
         downstream,
         latlon)
{
   junction<-list(name=name,
                  label=runif(1),
                  downstream=downstream,
                  latlon=latlon)

   return(junction)
}
