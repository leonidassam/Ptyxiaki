library( igraph)
source( "Functions.R")


iter <- 3
HBFcoverRate <- list( iter)
HDFcoverRate <- list( iter)
LDFcoverRate <- list( iter)
PCIcoverRate <- list( iter)

for( z in 1:iter) {
  g <- sample_pa( 50)
  
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
  
  HBFcoverRate[[z]] <- coverRateFunction( g, allShortestPaths, betweennessDf)
  
  
  
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
  
  HDFcoverRate[[z]] <- coverRateFunction( g, allShortestPaths, HDegreeDf)
  
  
  
  #
  # calculate the cover rate function based on lowest degree betweenness 
  #
  LDegreeDf <- degreeDf[ order( degreeDf[,2] ), ]
  
  LDFcoverRate[[z]] <- coverRateFunction( g, allShortestPaths, LDegreeDf)
  
  
  
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
  

  
  PCIcoverRate[[z]] <- coverRateFunction( g, allShortestPaths, PCIDf)
}


for( i in 1:iter) {
  for( j in 1:length( HDFcoverRate[[1]])) {
    HDFcoverRate[[ 1]][[j]] <- HDFcoverRate[[1]][[j]] + HDFcoverRate[[2]][[j]]
    HDFcoverRate[[ 1]][[j]] <- HDFcoverRate[[ 1]][[j]] / 2
  }
}

for( i in 1:iter) {
  for( j in 1:length( PCIcoverRate[[1]])) {
    PCIcoverRate[[ 1]][[j]] <- PCIcoverRate[[1]][[j]] + PCIcoverRate[[2]][[j]]
    PCIcoverRate[[ 1]][[j]] <- PCIcoverRate[[ 1]][[j]] / 2
  }
}

for( i in 1:iter) {
  for( j in 1:length( LDFcoverRate[[1]])) {
    LDFcoverRate[[ 1]][[j]] <- LDFcoverRate[[1]][[j]] + LDFcoverRate[[2]][[j]]
    LDFcoverRate[[ 1]][[j]] <- LDFcoverRate[[  1]][[j]] / 2
  }
}

for( i in 1:iter) {
  for( j in 1:length( HBFcoverRate[[1]])) {
    HBFcoverRate[[ 1]][[j]] <- HBFcoverRate[[1]][[j]] + HBFcoverRate[[2]][[j]]
    HBFcoverRate[[ 1]][[j]] <- HBFcoverRate[[  1]][[j]] / 2
  }
}
plot( V( g), HDFcoverRate[[  1]], type = "o", col = "yellow", ylim = c( 0,1))
lines( V( g), PCIcoverRate[[ 1]], col = "red")
lines( V( g), LDFcoverRate[[  1]], col = "green")
lines( V( g), HBFcoverRate[[  1]], col = "black")