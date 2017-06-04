library( igraph )
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
degrees <- calculatePCI( allLayers)
vectroIds <- vector( mode = "numeric", length = length( degrees))
vectorDegrees <- vector( mode = "numeric", length = length( degrees))
for( i in V( allLayers)) {
  vectroIds[i] <- i
  vectorDegrees[i] <- degrees[i]
}
degreesDf <- data.frame( vectroIds, vectorDegrees)
degreesDf <- degreesDf[ order( degreesDf[,2] ), ]

# List with all shortest paths from network
allShortestPaths <- list()
allShortestPaths1 <- list()
for( i in V( allLayers)) {
  for( j in V( allLayers)) {
    if( i != j ) {
      shortestPaths <- shortest_paths( allLayers, i, j, mode = c("out"))
      for( x in 1:length( shortestPaths[[1]])) {
        listToAdd <- list( )
        if( length( shortestPaths[[1]][[x]]) > 0 ) {
          listToAdd <- c( listToAdd, i)
          listToAdd <- c( listToAdd, j)
          allShortestPaths <- c( allShortestPaths, list( listToAdd))
          alist <- list()
          for( ii in 1:length( shortestPaths[[1]][[x]])) {
            alist <- c( alist, shortestPaths[[1]][[x]][[ii]])
          }
          allShortestPaths1 <- c( allShortestPaths1, list( alist))
        }
      }
    }
  }
}


# For each node get every shortest path. Add them to a list 
allShortestPathsCopy <- allShortestPaths
partialShortestPaths <- list( length( allShortestPaths))
percentage <- list( length( V( allLayers)))
counter <- 1

for( j in V( allLayers)) {
  
  nodeOutShortestPaths <- shortest_paths( allLayers, degreesDf[j,1], mode = c( "out"))
  nodeInShortestPaths <- shortest_paths( allLayers, degreesDf[j,1], mode = c( "in"))
  
  set <- list()
  for( ii in 1:length( nodeOutShortestPaths[[1]])) {
    path <- list( unlist( nodeOutShortestPaths[[1]][[ii]]))
    listToAdd <- list()
    if( length( path[[1]]) > 1) {
      listToAdd <- c( listToAdd, degreesDf[j,1])
      listToAdd <- c( listToAdd, path[[1]][[length( path[[1]])]])
      set <- c( set, list( listToAdd))
    }
  }
  
  for( ii in 1:length( nodeInShortestPaths[[1]])) {
    path <- list( unlist( nodeInShortestPaths[[1]][[ii]]))
    listToAdd <- list()
    if( length( path[[1]]) > 1) {
      listToAdd <- c( listToAdd, path[[1]][[length( path[[1]])]])
      listToAdd <- c( listToAdd, degreesDf[j,1])
      set <- c( set, list( listToAdd))
    }
  }
  
  if( length( allShortestPaths1) > 0) {
    for( ii in 1:length( allShortestPaths1)) {
      path <- list( unlist( allShortestPaths1[[ii]]))
      listToAdd <- list()
      if( degreesDf[j,1] %in% allShortestPaths1[[ii]]) {
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
        allShortestPaths1 <- allShortestPaths1[-pos]
      }
    }
  }
  
  # cover rate function
  percentage[[j]] <- ( counter - 1)/length( allShortestPaths)
  # print( percentage)
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