createJunction.base <-
function(name,
         label,
         downstream)
{
   junction<-list(name=name,
                  label=label,
                  downstream=downstream)

   return(junction)
}
