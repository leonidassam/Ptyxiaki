library( igraph)
source( "Functions.R")


iter <- 3
HBFcoverRate <- list( iter)
HDFcoverRate <- list( iter)
LDFcoverRate <- list( iter)
PCIcoverRate <- list( iter)

for( z in 1:iter) {
  
  g <- erdos.renyi.game( 50, 0.03, type = c( "gnp"), directed = FALSE, loops = FALSE)
  
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
  print( z)
  
  
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
  
  HBFcoverRate[[z]] <- coverRateFunction( g, allShortestPaths, betweennessDf)
  print( "betweenness ")
  
  
        #
        # calculate the cover rate function based on highest degree 
        #
  degreeCentrality <- degree( g, v = V( g), mode = c( "total"))
  vectorDegree <- vector( mode = "numeric", length = length( degreeCentrality))
  for( i in V( g)) {
    vectorDegree[i] <- degreeCentrality[[i]]
  }
  degreeDf <- data.frame( vectroIds, vectorDegree)
  HDegreeDf <- degreeDf[ order( -degreeDf[,2] ), ]
  
  HDFcoverRate[[z]] <- coverRateFunction( g, allShortestPaths, HDegreeDf)
  print( "high degree")

  
        #
        # calculate the cover rate function based on lowest degree betweenness 
        #
  LDegreeDf <- degreeDf[ order( degreeDf[,2] ), ]
  
  LDFcoverRate[[z]] <- coverRateFunction( g, allShortestPaths, LDegreeDf)

  
  
  
        #
        # calculate the cover rate function based on highest power community index ( PCI) 
        #
  PCI <- calculatePCI( g)
  vectorPCI <- vector( mode = "numeric", length = length( PCI))
  for( i in V( g)) {
    vectorPCI[i] <- PCI[[i]]
  }
  PCIDf <- data.frame( vectroIds, vectorPCI)
  PCIDf <- PCIDf[ order( -PCIDf[,2] ), ]
  print( "PCI")
  

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


  
  PCIcoverRate[[z]] <- coverRateFunction( g, allShortestPaths, PCIDf)
}

HBF <- list( length( V( g)))
HDF <- list( length( V( g)))
LDF <- list( length( V( g)))
PCI <- list( length( V( g)))

for( i in 1:length( V( g))) {
  HBF[[i]] <- 0
  HDF[[i]] <- 0
  LDF[[i]] <- 0
  PCI[[i]] <- 0
}
for( i in 1:iter) {
  for( j in 1:length( V( g))) {
    HBF[[ j]] <- HBF[[ j]] + HBFcoverRate[[ i]][[ j]]
    HDF[[ j]] <- HDF[[ j]] + HDFcoverRate[[ i]][[ j]]
    LDF[[ j]] <- LDF[[ j]] + LDFcoverRate[[ i]][[ j]]
    PCI[[ j]] <- PCI[[ j]] + PCIcoverRate[[ i]][[ j]]
  }
}
for( i in 1:length( V( g))) {
  HBF[[i]] <- HBF[[i]] /iter
  HDF[[i]] <- HDF[[i]] /iter
  LDF[[i]] <- LDF[[i]] /iter
  PCI[[i]] <- PCI[[i]] /iter
}

pdf( "Random 100nodes p0.0015 undirected improved")
plot( g, vertex.size = 0)
plot( V( g), main = "Cover Rate Function",  xlab = "Vertices", ylab = "Percentage",   ylim = c( 0:1))
lines( V( g), HBF, col = "blue")
lines( V( g), HDF, col = "red")
lines( V( g), LDF, col = "green")
lines( V( g), PCI, col = "black")
legend( "bottomright", c( "HBF", "HDF", "LDF", "PCI"), lty = c( 1, 1), lwd = c( 2, 2),col = c( "blue", "red", "green", "black"))
dev.off()