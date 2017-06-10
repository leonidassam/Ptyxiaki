library( igraph)
source( "Functions.R")



g <- erdos.renyi.game( 30, 70, type = c( "gnm"), directed = FALSE, loops = FALSE)
  

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
betweennessDf <- data.frame( vectroIds, vectorBetweenness)
betweennessDf <- betweennessDf[ order( -betweennessDf[,2] ), ]

HBF <- coverRateFunction( g, allShortestPaths, betweennessDf)
print( "betweenness ")
  
  
        #
        # calculate the cover rate function based on highest degree 
        #
degreeCentrality <- degree( g, v = V( g), mode = c( "total"))
vectorDegree <- vector( mode = "numeric", length = length( degreeCentrality))
for( i in V( g)) {    vectorDegree[i] <- degreeCentrality[[i]]  }
degreeDf <- data.frame( vectroIds, vectorDegree)
HDegreeDf <- degreeDf[ order( -degreeDf[,2] ), ]

HDF <- coverRateFunction( g, allShortestPaths, HDegreeDf)
print( "high degree")

  
        #
        # calculate the cover rate function based on lowest degree betweenness 
        #
LDegreeDf <- degreeDf[ order( degreeDf[,2] ), ]
  
LDF <- coverRateFunction( g, allShortestPaths, LDegreeDf)
print( "low degree")
  
  
  
        #
        # calculate the cover rate function based on highest power community index ( PCI) 
        #
PCI <- calculatePCI( g)
vectorPCI <- vector( mode = "numeric", length = length( PCI))
for( i in V( g)) {    vectorPCI[i] <- PCI[[i]]  }
PCIDf <- data.frame( vectroIds, vectorPCI)
PCIDf <- PCIDf[ order( -PCIDf[,2] ), ]

  
if( FALSE) {
  last <- 0
  for( i in 1:( nrow( PCIDf) - 1)) {
    for( j in ( i + 1):nrow( PCIDf)) {
      if( PCIDf[[i,2]] != PCIDf[[ j,2]]) {
        last <- j - 1
        break
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


PCI <- coverRateFunction( g, allShortestPaths, PCIDf)
print( "PCI")  



pdf( "Random 200nodes 100edges undirected1")
plot( g, vertex.size = 0)
plot( V( g), main = "Cover Rate Function",  xlab = "Vertices", ylab = "Percentage",   ylim = c( 0:1))
lines( V( g), HBF, col = "blue")
lines( V( g), HDF, col = "red")
lines( V( g), LDF, col = "green")
lines( V( g), PCI, col = "black")
legend( "bottomright", c( "HBF", "HDF", "LDF", "PCI"), lty = c( 1, 1), lwd = c( 2, 2),col = c( "blue", "red", "green", "black"))
dev.off()