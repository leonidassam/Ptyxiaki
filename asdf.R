library( igraph )
source( "PCI.R")

londonData <- read.table( './London_Multiplex_Transport/Dataset/london_transport_multiplex.edges', header = FALSE )
londonDataEdges <- londonData[,2:3]
print( londonDataEdges)


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