library( igraph )
source( "Functions.R")

londonData <- read.table( './London_Multiplex_Transport/Dataset/london_transport_multiplex.edges', header = FALSE )


position1 <- findPositions( londonData)[[1]]
position2 <- findPositions( londonData)[[2]]
position3 <- findPositions( londonData)[[3]]



firstLayer <- londonData[ 1:position1, ]
firstLayerEdgelist <- firstLayer[,2:3]
firstLayerEdgelist <- firstLayerEdgelist[,] + 1
firstLayerWeights <- firstLayer[,4]
firstLayerGraph <- graph_from_edgelist( data.matrix( firstLayerEdgelist), directed = TRUE )
E( firstLayerGraph)$weights <- firstLayerWeights

secondLayer <- londonData[ (position1 + 1):( position1 + position2),]
secondLayerEdgelist <- secondLayer[,2:3]
secondLayerEdgelist <- secondLayerEdgelist[,] + 1
secondLayrWeights <- secondLayer[,4]
secondLayerGraph <- graph_from_edgelist( data.matrix( secondLayerEdgelist), directed = TRUE )

thirdLayer <- londonData[ (position1 + position2 + 1):(position1 + position2 +position3),]
thirdLayerEdgelist <- thirdLayer[,2:3]
thirdLayerEdgelist <- thirdLayerEdgelist[,] + 1
thirdLayerWeights <- thirdLayer[,4]
thirdLayerGraph <- graph_from_edgelist( data.matrix( thirdLayerEdgelist), directed = TRUE )

londonDataEdgeList <- londonData[,2:3]
weight <- londonData[,4]
londonDataEdgeList <- londonDataEdgeList[,] + 1
g <- graph_from_edgelist( data.matrix( londonDataEdgeList), directed = TRUE)
E(g)$weights <- weight




degrees <- degree( thirdLayerGraph, mode = c( "out"))
vectroIds <- vector()
vectorDegrees <- vector()
for( i in 1:length( degrees)) {
	if( degrees[i] != 0 ) {
		vectroIds <- c( vectroIds, i)
		vectorDegrees <- c( vectorDegrees, degrees[i])
	}
}
degreesDf <- data.frame( vectroIds, vectorDegrees)
degreesDf <- degreesDf[ order(  -degreesDf[,2] ), ]

for( i in 2:3){
  print( shortest_paths( thirdLayerGraph, degreesDf[1,1], i, mode = c( "all"), output = c( "vpath")))
}
# plots
if( FALSE) {
  
  pdf( "Layers.pdf")
  plot( firstLayerGraph, vertex.size = 0.1, edge.label = E( firstLayerGraph)$firstLayerWeights)
  dev.off()
  pdf( "London.pdf")
  plot( g, vertex.size = 0.1, edge.label = E(g)$weights)
  dev.off()
}