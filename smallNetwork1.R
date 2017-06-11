library( igraph )
source( "Functions.R")

# read the data
Data <- read.csv( "./Network.csv", header = FALSE )
DataEdgeList <- Data[,1:2]
g <- graph_from_edgelist( data.matrix( DataEdgeList), directed = TRUE)



# Lists with all shortest paths from network
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
# calculate the cover rate function based on highest power community index ( PCI) 
#
PCI <- calculatePCI( g, "in")
vectroIds <- vector( mode = "numeric", length = length( PCI))
vectorPCI <- vector( mode = "numeric", length = length( PCI))
for( i in V( g)) {
  vectroIds[i] <- i
  vectorPCI[i] <- PCI[[i]]
}
PCIDf <- data.frame( vectroIds, vectorPCI)
PCIDf <- PCIDf[ order( -PCIDf[,2] ), ]

if( FALSE) {
for( i in 1:( nrow( PCIDf) - 1)) {
  for( j in ( i + 1):nrow( PCIDf)) {
    if( PCIDf[[i,2]] == PCIDf[[ j,2]]) {
      last <- j
    }
  }
  first <- i
  
  if( last > first) {
    for( j in last:first) {
      btwFirst <- betweenness( g, PCIDf[[ last - 1,1]], directed = TRUE)
      btwLst <- betweenness( g, PCIDf[[ last,1]], directed = TRUE)
      if( btwLst > btwFirst) {
        temp <- PCIDf[[ last, 1]]
        PCIDf[[ last, 1]] <- PCIDf[[ first, 1]]
        PCIDf[[ first, 1]] <- temp
      }
    }
  }
}
}
PCIcoverRate <- coverRateFunction( g, allShortestPaths, PCIDf)




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





plot( V( g), HDFcoverRate, ylim = c( 0,1))
lines( V( g), PCIcoverRate, col = "red")
lines( V( g), LDFcoverRate, col = "green")
lines( V( g), HBFcoverRate, col = "black")