createArea.base <-
function(name,location,simulation)
{
   reservoirs        <-list()
   junctions         <-list()
   rivers            <-list()
   diversions        <-list()
   demands           <-list()
   aquifers          <-list()
   area<-list(name=name,
              location  =location,
              reservoirs=reservoirs,
              aquifers=aquifers,
              junctions =junctions,
              rivers    =rivers,
              diversions=diversions,
              demands   =demands,
              simulation=simulation)
   return(area)
}
