library( igraph )


# read the data
Data <- read.csv( "./Network.csv", header = FALSE )
DataEdgeList <- Data[,1:2]
g <- graph_from_edgelist( data.matrix( DataEdgeList), directed = TRUE)


node1 <- shortest_paths( g, 1, mode = c( "in"))
betweennessCentrality <- betweenness( g, v = V( g), directed = TRUE)
# List with all shortest paths from network
allShortestPaths <- list()
for( i in V(g)) {
  for( j in V(g)) {
    if( i != j ) {
      shortestPaths <- shortest_paths( g, i, j, mode = c("out"))
      for( x in 1:length( shortestPaths[[1]])) {
        if( length( shortestPaths[[1]][[x]]) > 0 ) {
          allShortestPaths <- c( allShortestPaths, shortestPaths[[1]])
        }
      }
    }
  }
}

# For each node get every shortest path. Add them to a list 
partialShortestPaths <- list()
for( j in V( g)) {
  nodeOutShortestPaths <- shortest_paths( g, nodes[[j]], mode = c( "out"))
  nodeInShortestPaths <- shortest_paths( g, nodes[[j]], mode = c( "in"))
  
  # out shortest paths of node
  for( i in 1:length( nodeOutShortestPaths[[1]])) {
    
    firstList <- list( unlist( nodeOutShortestPaths[[1]][[i]]))
    # shortest paths with more than two nodes
    if( length( firstList[[1]]) > 1 ) {
    
        # check list for duplicates      
        if( length( partialShortestPaths) > 0 ) {
          
          # check for duplicates
          k <- 0 
          for( x in 1:length( partialShortestPaths)) {
            secondList <- list( unlist( partialShortestPaths[[x]]))
            if( identical( unlist( firstList), unlist( secondList))) {  
              k <- 1
              break
            }
          }
          
          # if no duplicates, add the shortest path to "partialShortestPaths" list
          if( k == 0 ) { partialShortestPaths <- c( partialShortestPaths, firstList)  }
        }
      
        # at the beginning the list is empty
        else {    partialShortestPaths <- c( partialShortestPaths, firstList)       }
    }
  }
  
  
  # in shortest paths of node 
  for( i in 1:length( nodeInShortestPaths[[1]])) {
    
    firstList <- list( unlist( nodeInShortestPaths[[1]][[i]]))
    # shortest paths with more than two nodes
    if( length( firstList[[1]]) > 1 ) {  
      
      reverse <- list( rev( firstList[[1]]))
      # check for duplicates
      k <- 0 
      for( x in 1:length( partialShortestPaths)) {
        secondList <- list( unlist( partialShortestPaths[[x]]))
        if( identical( unlist( reverse), unlist( secondList))) {  k <- 1 }
      }
      
      # if no duplicates, add the shortest path to "partialShortestPaths" list
      if( k == 0 ) { partialShortestPaths <- c( partialShortestPaths, reverse) }
      }
  }
  
  # cover rate function
  percentage <- length( partialShortestPaths)/length( allShortestPaths)
  print( percentage)
}