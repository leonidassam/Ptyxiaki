library( igraph )
source( "PCI.R")

londonData <- read.table( './London_Multiplex_Transport/Dataset/london_transport_multiplex.edges', header = FALSE )
londonDataEdgeList <- londonData[,2:3]
weight <- londonData[,4]
print( weight)

londonDataEdgeList <- londonDataEdgeList[,] + 1
matrixLondonDataEdgeList <- data.matrix( londonDataEdgeList)
g <- graph_from_edgelist( matrixLondonDataEdgeList, directed = TRUE)

E(g)$weights <- weight
plot( g, layout = layout_( g, as_star()), edge.label = E(g)$weights)



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