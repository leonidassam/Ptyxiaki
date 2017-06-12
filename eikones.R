library( igraph)
source( "Functions.R")

g <- sample_pa( 10, m = 1, directed = FALSE)

png( filename = "C:/Users/Antonios/Desktop/leonidas/Διπλωματική/Ptyxiaki-master/PCI.png")
plot( g, vertex.label = calculatePCI( g, "total") , edge.arrow.size = 0.5)
dev.off()
