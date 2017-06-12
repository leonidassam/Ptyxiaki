library( igraph )

# calculates the PCI of the network 
# returns a list 
calculatePCI <- function( g, m) {
  PCI = list( )
  
  for( v in V( g)) {
    neighbors <- neighbors( g, v, "out")
    
    Nodedegrees <- list( )
    for( n in neighbors) {      Nodedegrees <-  c( Nodedegrees, degree( g, n, mode = m))    }
    
    if( length( Nodedegrees) > 0) {      sort( Nodedegrees[[1]], decreasing = TRUE)    }
    
    b <- 0
    if( length( Nodedegrees) > 0) {
      for( i in 1:length( Nodedegrees)) {         if( Nodedegrees[[i]] > b ) {    b <- b + 1      }      }
    }
    PCI <- c( PCI, b)
  }
  return( PCI)
}


# split the network
# returns a list
findPositions <- function( data) {
  pos1 <- 0
  pos2 <- 0
  pos3 <- 0
  for( i in data[,1]) {
    
    if(i==1){       pos1 <- pos1 + 1    }
    if(i==2){       pos2 <- pos2 + 1    }
    if(i==3){       pos3 <- pos3 + 1    }
  }
  
  return( list(pos1, pos2, pos3))
}


# calculates the cover rate function based on HBF
# returns a list 
coverRateFunction <- function( g, allShortestPaths, df) {
  
  allShortestPathsCopy <- allShortestPaths
  allShortestPathsCompleteCopy <- allShortestPathsComplete
  partialShortestPaths <- list( length( allShortestPaths))
  percentage <- list( length( V( g)))
  counter <- 1
  for( vector in V( g)) {
    nodeOutShortestPaths <- shortest_paths( g, df[[vector,1]], mode = c( "out"))
    nodeInShortestPaths <- shortest_paths( g, df[[vector,1]], mode = c( "in"))
    
    set <- list()

    if( length( allShortestPathsCompleteCopy) > 0) {
      for( i in 1:length( allShortestPathsCompleteCopy)) {
        path <- list( unlist( allShortestPathsCompleteCopy[[i]]))
        listToAdd <- list()
        if( df[[vector,1]] %in% allShortestPathsCompleteCopy[[i]]) {
          listToAdd <- c( listToAdd, path[[1]][[1]])
          listToAdd <- c( listToAdd, path[[1]][[length( path[[1]])]])
          set <- c( set, list( listToAdd))
        }
      }
    }
    
    
    if( length( set) > 0) {
      for( i in 1:length( set)) {
        pos <- 0 
        if( length( allShortestPathsCopy) > 0) {
          for( ii in 1:length( allShortestPathsCopy)) {
            if( isTRUE( all.equal( list( set[[i]]), list( allShortestPathsCopy[[ii]]))))  { 
              pos <- ii
              break
            }
          }
          if( pos != 0 ) {
            partialShortestPaths[counter] <- allShortestPathsCopy[pos]
            counter <- counter + 1
            allShortestPathsCopy <- allShortestPathsCopy[-pos]
            allShortestPathsCompleteCopy <- allShortestPathsCompleteCopy[-pos]
          }
        }
      }
    }
    # cover rate function
    percentage[[vector]] <- ( counter - 1)/length( allShortestPaths)
    # print( percentage)
  }
  
  return( percentage)
}


# create sorted dataframe based on highest betweenness
# returns a dataframe
createSortedHBFDataframe <- function( g) {
  
  betweennessCentrality <- betweenness( g, v = V( g), directed = TRUE)
  vectroIds <- vector( mode = "numeric", length = length( betweennessCentrality))
  vectorBetweenness <- vector( mode = "numeric", length = length( betweennessCentrality))
  for( i in V( g)) {
    vectroIds[i] <- i
    vectorBetweenness[i] <- betweennessCentrality[[i]]
  }
  df <- data.frame( vectroIds, vectorBetweenness)
  df <- df[ order( -df[,2] ), ]
  
  return( df)
}


# create sorted dataframe based on highest degree
# returns a dataframe
createSortedHDFDataframe <- function( g) {
  
  degreeCentrality <- degree( g, v = V( g), mode = c( "in"))
  vectroIds <- vector( mode = "numeric", length = length( degreeCentrality))
  vectorDegree <- vector( mode = "numeric", length = length( degreeCentrality))
  for( i in V( g)) {
    vectroIds[i] <- i
    vectorDegree[i] <- degreeCentrality[[i]]
  }
  df <- data.frame( vectroIds, vectorDegree)
  df <- df[ order( -df[,2] ), ]
  
  return( df)
}


# create sorted dataframe based on highest closenness
# returns a dataframe
createSortedHCFDataframe <- function( g) {
  
  closenessCentrality <- closeness( g, v = V( g), mode = c( "in"))
  vectroIds <- vector( mode = "numeric", length = length( closenessCentrality))
  vectorCloseness <- vector( mode = "numeric", length = length( closenessCentrality))
  for( i in V( g)) {
    vectroIds[i] <- i
    vectorCloseness[i] <- closenessCentrality[[i]]
  }
  df <- data.frame( vectroIds, vectorCloseness)
  df <- df[ order( -df[,2] ), ]
  
  return( df)
}


# create sorted dataframe based on highest PCI
# returns a dataframe
createSortedHPCIDataframe <- function( g) {
  
  PCI <- calculatePCI( g, "in")
  vectroIds <- vector( mode = "numeric", length = length( PCI))
  vectorPCI <- vector( mode = "numeric", length = length( PCI))
  for( i in V( g)) {
    vectroIds[i] <- i
    vectorPCI[i] <- PCI[[i]]
  }
  df <- data.frame( vectroIds, vectorPCI)
  df <- df[ order( -df[,2] ), ]
  
  return( df)
}


# get the mean results
# return a list of lists HBF - HDF - HCF - PCI
getMeanValues <- function( HBFcoverRate, HDFcoverRate, HCFcoverRate, HPCIDf) {
  
  HBF <- list( length( V( g)))
  HDF <- list( length( V( g)))
  HCF <- list( length( V( g)))
  PCI <- list( length( V( g)))
  
  for( i in 1:length( V( g))) {
    HBF[[i]] <- 0
    HDF[[i]] <- 0
    HCF[[i]] <- 0
    PCI[[i]] <- 0
  }
  for( i in 1:iter) {
    for( j in 1:length( V( g))) {
      HBF[[ j]] <- HBF[[ j]] + HBFcoverRate[[ i]][[ j]]
      HDF[[ j]] <- HDF[[ j]] + HDFcoverRate[[ i]][[ j]]
      HCF[[ j]] <- HCF[[ j]] + HCFcoverRate[[ i]][[ j]]
      PCI[[ j]] <- PCI[[ j]] + PCIcoverRate[[ i]][[ j]]
    }
  }
  for( i in 1:length( V( g))) {
    HBF[[i]] <- HBF[[i]] /iter
    HDF[[i]] <- HDF[[i]] /iter
    HCF[[i]] <- HCF[[i]] /iter
    PCI[[i]] <- PCI[[i]] /iter
  }
  
  return( list( HBF, HDF, HCF, PCI))
}


# plots the results of the cover rate functions
plotCoverRateFunction <- function( g, HBF, HDF, HCF, PCI, fname) {
  png( filename = fname)
  plot( V( g), main = "Cover Rate Function",  xlab = "Vertices", ylab = "Percentage",   ylim = c( 0:1))
  lines( V( g), HBF, col = "blue")
  lines( V( g), HDF, col = "red")
  lines( V( g), HCF, col = "green")
  lines( V( g), PCI, col = "black")
  legend( "bottomright", c( "HBF", "HDF", "HCF", "PCI"), lty = c( 1, 1), lwd = c( 2, 2),col = c( "blue", "red", "green", "black"))
  dev.off()
}


# plots the number of check ing nodes for every centrality
plotNumberOfCheckInNodes <- function ( g, numberOfNodes, CheckInNodesHBF, CheckInNodesHDF, CheckInNodesHCF, CheckInNodesPCI, fname){
                            
  png( filename = fname)
  plot( 0, main = "Number of check in nodes",  xlab = "Vertices", xlim = c( 0,length( V( g))), ylim = c( 0,length( V( g))))
  lines( numberOfNodes, CheckInNodesHBF, col = "blue")
  lines( numberOfNodes, CheckInNodesHDF, col = "red")
  lines( numberOfNodes, CheckInNodesHCF, col = "green")
  lines( numberOfNodes, CheckInNodesPCI, col = "black")
  legend( "bottomright", c( "HBF", "HDF", "HCF", "PCI"), lty = c( 1, 1), lwd = c( 2, 2),col = c( "blue", "red", "green", "black"))
  dev.off()
}