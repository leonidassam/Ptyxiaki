library( igraph )

Data <- read.csv( 'Network.csv', header = FALSE )
DataEdgeList <- Data[,1:2]
g <- graph_from_edgelist( data.matrix( DataEdgeList), directed = TRUE)

# get all shortest paths from network
allShortestPaths <- list()
for( i in V(g)) {
  sps <- list()
  for( j in V(g)) {
    if( i != j ) {
      sps[i] <- shortest_paths( g, i, j, mode = c("out"), output = c("vpath"))
      allShortestPaths <- c( allShortestPaths, sps[[i]])
    }
  }
}


partialShortestPaths <- list()
for( j in V(g)) {
  nodeOut <- shortest_paths( g, j, mode = c( "out"))
  nodeIn <- shortest_paths( g, j, mode = c( "in"))
  
  for( i in 1:length( nodeOut[[1]])) {
    first <- list( unlist( nodeOut[[1]][[i]]))
    if( length( first[[1]]) > 1 ) {  partialShortestPaths <- c( partialShortestPaths, first) }
  }
  for( i in 1:length( nodeIn[[1]])) {
    first <- list( unlist( nodeIn[[1]][[i]]))
    if( length( first[[1]]) > 1 ) {  
      reverse <- list( rev( first[[1]]))
      partialShortestPaths <- c( partialShortestPaths, reverse) 
    }
  }
}

partialShortestPathsUnique <- list()
partialShortestPathsUnique <- partialShortestPaths
for( x in 1:length( partialShortestPaths)) {
  firstList <- list( partialShortestPaths[[x]])
  for( y in x:length( partialShortestPaths)) {
    secondList <- list( partialShortestPaths[[y]])
    if( identical( unlist( firstList), unlist( secondList))) {
      partialShortestPathsUnique[y] <- NULL
    }
  }
}

percentage <- length( partialShortestPathsUnique)/length( allShortestPaths)
print( percentage)

node1In <- shortest_paths( g, 1, mode = c( "in"))
node2Out <- shortest_paths( g, 2, mode = c( "out"))
qwer <- list( node1In[[1]][[2]])
re <- list( rev( node2Out[[1]][[1]]))
alist <- list()
alist <- c( alist, qwer)
alist <- c( alist, re)
alistunique <- unlist( alist)
alistunique <- list( unique( alistunique))
print( alistunique)