library( igraph)
source( "Functions.R")



g <- sample_pa( 10, m = 2, directed = TRUE)
  

HBF <- list( length( V( g)))
HDF <- list( length( V( g)))
LDF <- list( length( V( g)))
PCI <- list( length( V( g)))


allShortestPaths <- list()
allShortestPathsComplete <- list()
for( i in V( g)) {
  for( j in V( g)) {
    # do not compute loops 
    if( i != j ) {
      shortestPaths <- shortest_paths( g, i, j, mode = c("out"))
      for( x in 1:length( shortestPaths[[1]])) {
        listToAdd <- list()
        if( length( shortestPaths[[1]][[x]]) > 0 ) {
          listToAdd <- c( listToAdd, i)
          listToAdd <- c( listToAdd, j)
          allShortestPaths <- c( allShortestPaths, list( listToAdd))
          alist <- list()
          for( ii in 1:length( shortestPaths[[1]][[x]])) {
            alist <- c( alist, shortestPaths[[1]][[x]][[ii]])
          }
          allShortestPathsComplete <- c( allShortestPathsComplete, list( alist))
        }
      }
    }
  }
}
print( length( allShortestPaths))
  
  
        #
        # calculate the cover rate function based on highest betweenness 
        #
betweennessCentrality <- betweenness( g, v = V( g), directed = TRUE)
vectroIds <- vector( mode = "numeric", length = length( betweennessCentrality))
vectorBetweenness <- vector( mode = "numeric", length = length( betweennessCentrality))
for( i in V( g)) {
  vectroIds[i] <- i
  vectorBetweenness[i] <- betweennessCentrality[[i]]
}
HBetweennessDf <- data.frame( vectroIds, vectorBetweenness)
HBetweennessDf <- HBetweennessDf[ order( -HBetweennessDf[,2] ), ]

HBF <- coverRateFunction( g, allShortestPaths, HBetweennessDf)
print( "betweenness ")




  
  
        #
        # calculate the cover rate function based on highest degree 
        #
degreeCentrality <- degree( g, v = V( g), mode = c( "in"))
vectorDegree <- vector( mode = "numeric", length = length( degreeCentrality))
for( i in V( g)) {    vectorDegree[i] <- degreeCentrality[[i]]  }
HDegreeDf <- data.frame( vectroIds, vectorDegree)
HDegreeDf <- HDegreeDf[ order( -HDegreeDf[,2] ), ]

HDF <- coverRateFunction( g, allShortestPaths, HDegreeDf)
print( "high degree")

  
        #
        # calculate the cover rate function based on highest closeness 
        #
closenessCentrality <- closeness( g, v = V( g), mode = c( "in"))
vectorCloseness <- vector( mode = "numeric", length = length( closenessCentrality))
for( i in V( g)) {  vectorCloseness[i] <- closenessCentrality[[i]]}
HClosenessDf <- data.frame( vectroIds, vectorCloseness)
HClosenessDf <- HClosenessDf[ order( -HClosenessDf[,2] ), ]

HCF <- coverRateFunction( g, allShortestPaths, HClosenessDf)
print( "closeness ")
  
  
        #
        # calculate the cover rate function based on highest power community index ( PCI) 
        #
PCI <- calculatePCI( g, "in")
vectorPCI <- vector( mode = "numeric", length = length( PCI))
for( i in V( g)) {    vectorPCI[i] <- PCI[[i]]  }
HPCIDf <- data.frame( vectroIds, vectorPCI)
HPCIDf <- HPCIDf[ order( -HPCIDf[,2] ), ]


PCI <- coverRateFunction( g, allShortestPaths, HPCIDf)
print( "PCI")  


plotTheResults( g, HBF, HDF, HCF, PCI)