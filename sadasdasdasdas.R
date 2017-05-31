library( igraph )

setwd("C:/Users/Antonios/Desktop/leonidas/Διπλωματική/Ptyxiaki-master")
Data <- read.csv( "./Network.csv", header = FALSE )
DataEdgeList <- Data[,1:2]
g <- graph_from_edgelist( data.matrix( DataEdgeList), directed = TRUE)

# List with all shortest paths from network
allShortestPaths <- list()
for( i in V(g)) {
  sps <- list()
  for( j in V(g)) {
    if( i != j ) {
      sps <- shortest_paths( g, i, j, mode = c("out"))
      if( length( sps[[1]][[1]]) > 0 ) {
        allShortestPaths <- c( allShortestPaths, sps[[1]])
      }
    }
  }
}

# For each node get every shortest path. Add them to a list ( duplicates exist )
partialShortestPaths <- list()
for( j in V(g)) {
  nodeOut <- shortest_paths( g, j, mode = c( "out"))
  nodeIn <- shortest_paths( g, j, mode = c( "in"))
  
  # out shortest paths of node
  for( i in 1:length( nodeOut[[1]])) {
    first <- list( unlist( nodeOut[[1]][[i]]))
    # shortest paths with more than two nodes
    if( length( first[[1]]) > 1 ) {
      if( length( partialShortestPaths) > 0 ) {
        # check for duplicates
        k <- 0 
        for( x in 1:length( partialShortestPaths)) {
          second <- list( unlist( partialShortestPaths[[x]]))
          if( identical( unlist( first), unlist( second))) {  
            k <- 1
            break
          }
        }
        if( k == 0 ) { partialShortestPaths <- c( partialShortestPaths, first)  }
      }
      else {
        partialShortestPaths <- c( partialShortestPaths, first) 
      }
    }
  }
  
  
  # in shortest paths of node 
  for( i in 1:length( nodeIn[[1]])) {
    first <- list( unlist( nodeIn[[1]][[i]]))
    # shortest paths with more than two nodes
    if( length( first[[1]]) > 1 ) {  
      reverse <- list( rev( first[[1]]))
      # check for duplicates
      k <- 0 
      for( x in 1:length( partialShortestPaths)) {
        second <- list( unlist( partialShortestPaths[[x]]))
        if( identical( unlist( reverse), unlist( second))) {  k <- 1 }
      }
      if( k == 0 ) { partialShortestPaths <- c( partialShortestPaths, reverse) }
      }
  }
  percentage <- length( partialShortestPaths)/length( allShortestPaths)
  print( percentage)
}