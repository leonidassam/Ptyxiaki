library( igraph )

# calculates the PCI of the network 
# returns a list 
calculatePCI <- function( g) {
  PCI = list( )
  
  for( v in V( g)) {
    neighbors <- neighbors( g, v, "out")
    
    Nodedegrees <- list( )
    for( n in neighbors) {      Nodedegrees <-  c( Nodedegrees, degree( g, n, mode = c( "total")))    }
    
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
  partialShortestPaths <- list( length( allShortestPaths))
  percentage <- list( length( V( g)))
  counter <- 1
  for( vector in V( g)) {
    nodeOutShortestPaths <- shortest_paths( g, df[[vector,1]], mode = c( "out"))
    nodeInShortestPaths <- shortest_paths( g, df[[vector,1]], mode = c( "in"))
    
    set <- list()
    for( i in 1:length( nodeOutShortestPaths[[1]])) {
      path <- list( unlist( nodeOutShortestPaths[[1]][[i]]))
      listToAdd <- list()
      if( length( path[[1]]) > 1) {
        listToAdd <- c( listToAdd, df[[vector,1]])
        listToAdd <- c( listToAdd, path[[1]][[length( path[[1]])]])
        set <- c( set, list( listToAdd))
      }
    }
    
    for( i in 1:length( nodeInShortestPaths[[1]])) {
      path <- list( unlist( nodeInShortestPaths[[1]][[i]]))
      listToAdd <- list()
      if( length( path[[1]]) > 1) {
        listToAdd <- c( listToAdd, path[[1]][[length( path[[1]])]])
        listToAdd <- c( listToAdd, df[[vector,1]])
        set <- c( set, list( listToAdd))
      }
    }
    
    if( length( allShortestPathsCopy) > 0) {
      for( i in 1:length( allShortestPathsComplete)) {
        path <- list( unlist( allShortestPathsComplete[[i]]))
        listToAdd <- list()
        if( df[[vector,1]] %in% allShortestPathsComplete[[i]]) {
          listToAdd <- c( listToAdd, path[[1]][[1]])
          listToAdd <- c( listToAdd, path[[1]][[length( path[[1]])]])
          for( jj in 1:length( set)) {
            exists <- 0
            if( isTRUE( all.equal( list( set[[jj]]), list( listToAdd)))) {
              exists <- 1
              break
            }
          }
          if( exists == 0) { set <- c( set, list( listToAdd))}
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