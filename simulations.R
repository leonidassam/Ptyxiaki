library( igraph)
source( "Functions.R")

g <- sample_gnm( 30, 50, directed = TRUE, loops = FALSE)

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
betweennessDf <- data.frame( vectroIds, vectorBetweenness)
betweennessDf <- betweennessDf[ order( -betweennessDf[,2] ), ]

HBFcoverRate <- coverRateFunction( g, allShortestPaths, betweennessDf)



#
# calculate the cover rate function based on highest degree 
#
degreeCentrality <- degree( g, v = V( g), mode = c( "total"))
vectroIds <- vector( mode = "numeric", length = length( degreeCentrality))
vectorDegree <- vector( mode = "numeric", length = length( degreeCentrality))
for( i in V( g)) {
  vectroIds[i] <- i
  vectorDegree[i] <- degreeCentrality[[i]]
}
degreeDf <- data.frame( vectroIds, vectorDegree)
HDegreeDf <- degreeDf[ order( -degreeDf[,2] ), ]

HDFcoverRate <- coverRateFunction( g, allShortestPaths, HDegreeDf)



#
# calculate the cover rate function based on lowest degree betweenness 
#
LDegreeDf <- degreeDf[ order( degreeDf[,2] ), ]

LDFcoverRate <- coverRateFunction( g, allShortestPaths, LDegreeDf)



#
# calculate the cover rate function based on highest power community index ( PCI) 
#
PCI <- calculatePCI( g)
vectroIds <- vector( mode = "numeric", length = length( PCI))
vectorPCI <- vector( mode = "numeric", length = length( PCI))
for( i in V( g)) {
  vectroIds[i] <- i
  vectorPCI[i] <- PCI[[i]]
}
PCIDf <- data.frame( vectroIds, vectorPCI)
PCIDf <- PCIDf[ order( -PCIDf[,2] ), ]

PCIcoverRate <- coverRateFunction( g, allShortestPaths, PCIDf)

plot( V( g), HDFcoverRate, type = "o", col = "yellow", ylim = c( 0,1))
lines( V( g), PCIcoverRate, col = "red")
lines( V( g), LDFcoverRate, col = "green")
lines( V( g), HBFcoverRate, col = "black")