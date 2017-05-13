library( igraph )
source( "PCI.R")

londonData <- read.table( './London_Multiplex_Transport/Dataset/london_transport_multiplex.edges', header = FALSE )
position1 <- 0
position2 <- 0
position3 <- 0

findPositions <- function( data) {
  pos1 <- 0
  pos2 <- 0
  pos3 <- 0
  for( i in data[,1]) {
    
    if(i==1){ 
      pos1 <- pos1 + 1
    }
    if(i==2){ 
      pos2 <- pos2 + 1
    }
    if(i==3){ 
      pos3 <- pos3 + 1
    }
  }
  
  return( list(pos1, pos2, pos3))
}

position1 <- findPositions( londonData)[[1]]
position2 <- findPositions( londonData)[[2]]
position3 <- findPositions( londonData)[[3]]


firstLayer <- londonData[ 1:position1, ]
firstLayerEdgelist <- firstLayer[,2:3]
firstLayerEdgelist <- firstLayerEdgelist[,] + 1
firstLayerWeights <- firstLayer[,4]

secondLayer <- londonData[ (position1 + 1):( position1 + position2),]
secondLayerEdgelist <- secondLayer[,2:3]
secondLayerEdgelist <- secondLayerEdgelist[,] + 1
secondLayrWeights <- secondLayer[,4]

thirdLayer <- londonData[ (position1 + position2 + 1):(position1 + position2 +position3),]
thirdLayerEdgelist <- thirdLayer[,2:3]
thirdLayerEdgelist <- thirdLayerEdgelist[,] + 1
thirdLayerWeights <- thirdLayer[,4]



firstLayerGraph <- graph_from_edgelist( data.matrix( firstLayerEdgelist), directed = TRUE )
secondLayerGraph <- graph_from_edgelist( data.matrix( secondLayerEdgelist), directed = TRUE )
thirdLayerGraph <- graph_from_edgelist( data.matrix( thirdLayerEdgelist), directed = TRUE )


londonDataEdgeList <- londonData[,2:3]
weight <- londonData[,4]

londonDataEdgeList <- londonDataEdgeList[,] + 1
matrixLondonDataEdgeList <- data.matrix( londonDataEdgeList)
g <- graph_from_edgelist( matrixLondonDataEdgeList, directed = TRUE)

E(g)$weights <- weight
#plot( g, layout = layout_( g, as_star()), edge.label = E(g)$weights)



if( FALSE) {
	data <- read.csv( 'Network.csv', header = FALSE )
	g <- graph.data.frame( data)

	E(g)$weight=as.numeric( data[,3])
	E(g)$curved <- 0.2
	adjacencyMatrix <- get.adjacency( g, attr = "weight")



	PCI <- calculatePCI()

	print( PCI)
	plot( g, edge.label = E(g)$weight)
}