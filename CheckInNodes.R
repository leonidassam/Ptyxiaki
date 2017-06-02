library( igraph )
setwd("C:/Users/Antonios/Desktop/leonidas/Διπλωματική/Ptyxiaki-master")
source( "Functions.R")

# read the data
londonData <- read.table( './London_Multiplex_Transport/Dataset/london_transport_multiplex.edges', header = FALSE )
if( FALSE ){
# split the layers
position1 <- findPositions( londonData)[[1]]
position2 <- findPositions( londonData)[[2]]
position3 <- findPositions( londonData)[[3]]


# create first layer data 
firstLayer <- londonData[ 1:position1, ]
firstLayerEdgelist <- firstLayer[,2:3]
firstLayerEdgelist <- firstLayerEdgelist[,] + 1
firstLayerWeights <- firstLayer[,4]
#firstLayerGraph <- graph_from_edgelist( data.matrix( firstLayerEdgelist), directed = TRUE )
#E( firstLayerGraph)$weights <- firstLayerWeights

# create second layer data 
secondLayer <- londonData[ (position1 + 1):( position1 + position2),]
secondLayerEdgelist <- secondLayer[,2:3]
secondLayerEdgelist <- secondLayerEdgelist[,] + 1
secondLayrWeights <- secondLayer[,4]
#secondLayerGraph <- graph_from_edgelist( data.matrix( secondLayerEdgelist), directed = TRUE )

# create third layer data 
thirdLayer <- londonData[ (position1 + position2 + 1):(position1 + position2 +position3),]
thirdLayerEdgelist <- thirdLayer[,2:3]
thirdLayerEdgelist <- thirdLayerEdgelist[,] + 1
thirdLayerWeights <- thirdLayer[,4]
#thirdLayerGraph <- graph_from_edgelist( data.matrix( thirdLayerEdgelist), directed = TRUE )
}


# create multilayer network
londonDataEdgeList <- londonData[,2:3]
weight <- londonData[,4]
londonDataEdgeList <- londonDataEdgeList[,] + 1
allLayers <- graph_from_edgelist( data.matrix( londonDataEdgeList), directed = TRUE)
E( allLayers)$weights <- weight


# build sorted degrees dataframe 
degrees <- betweenness( allLayers, v = V( allLayers), directed = TRUE )
vectroIds <- vector( mode = "numeric", length = length( degrees))
vectorDegrees <- vector( mode = "numeric", length = length( degrees))
for( i in 1:length( degrees)) {
  if( degrees[i] != 0 ) {
    vectroIds[i] <- i
    vectorDegrees[i] <- degrees[i]
  }
}
degreesDf <- data.frame( vectroIds, vectorDegrees)
degreesDf <- degreesDf[ order( -degreesDf[,2] ), ]



# List with all shortest paths from network
allShortestPaths <- list()
for( i in V( allLayers)) {
  for( j in V( allLayers)) {
    if( i != j ) {
      shortestPaths <- shortest_paths( allLayers, i, j, mode = c("out"))
      for( x in 1:length( shortestPaths[[1]])) {
        if( length( shortestPaths[[1]][[x]]) > 1 ) {
          allShortestPaths <- c( allShortestPaths, shortestPaths[[1]])
        }
      }
    }
  }
}

# For each node get every shortest path. Add them to a list 
partialShortestPaths <- list( )
percentage <- list( length( V( allLayers)))
for( j in 1:nrow( degreesDf)) {
  if( degreesDf[j,1] != 0 ) {
    nodeOutShortestPaths <- shortest_paths( allLayers, degreesDf[j,1], mode = c( "out"))
    nodeInShortestPaths <- shortest_paths( allLayers, degreesDf[j,1], mode = c( "in"))
    # out shortest paths of node
    for( i in 1:length( nodeOutShortestPaths[[1]])) {
      firstList <- list( unlist( nodeOutShortestPaths[[1]][[i]]))
      # shortest paths with more than two nodes
      if( length( firstList[[1]]) > 1) {
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
          if( k == 0 ) { 
            partialShortestPaths <- c( partialShortestPaths, firstList)
          }
        }
        # at the beginning the list is empty
        else {    partialShortestPaths <- c( partialShortestPaths, firstList)       }
      }
    }
    
    
    # in shortest paths of node 
    for( i in 1:length( nodeInShortestPaths[[1]])) {
      firstList <- list( unlist( nodeInShortestPaths[[1]][[i]]))
      # shortest paths with more than two nodes
      if( length( firstList[[1]]) > 1) {  
        reverse <- list( rev( firstList[[1]]))
        # check for duplicates
        k <- 0 
        for( x in 1:length( partialShortestPaths)) {
          secondList <- list( unlist( partialShortestPaths[[x]]))
          if( identical( unlist( reverse), unlist( secondList))) {  k <- 1 }
        }
        # if no duplicates, add the shortest path to "partialShortestPaths" list
        if( k == 0 ) { 
          partialShortestPaths <- c( partialShortestPaths, reverse)
        }
      }
    }
    
  }
  # cover rate function
  percentage[j] <- length( partialShortestPaths)/length( allShortestPaths)
  
}

repetitions <- c( 1:length( percentage))
plot( repetitions, percentage)
# plots
if( TRUE) {
  
  pdf( "Layers.pdf")
  thirdLayerSub <- induced.subgraph( thirdLayerGraph, vids = thirdLayerEdgelist[,1])
  plot( thirdLayerSub, vertex.size = 0.1)
  dev.off()
  #pdf( "London.pdf")
  #plot( g, vertex.size = 0.1, edge.label = E(g)$weights)
  #dev.off()
}