library( igraph )
source( "Functions.R")

# read the data
Data <- read.csv( "./Network.csv", header = FALSE )
DataEdgeList <- Data[,1:2]
g <- graph_from_edgelist( data.matrix( DataEdgeList), directed = TRUE)



# Lists with all shortest paths from network
allShortestPathsComplete <- vector( "list",  100)
counter <- 1
for( i in V( g)) {
  for( j in V( g)) {
    # do not compute loops 
    if( i != j ) {
      shortestPaths <- shortest_paths( g, i, j, mode = c("out"))
      for( x in 1:length( shortestPaths[[1]])) {
        if( length( shortestPaths[[1]][[x]]) > 0 ) {
          alist <- list()
          for( ii in 1:length( shortestPaths[[1]][[x]])) {
            alist <- c( alist, shortestPaths[[1]][[x]][[ii]])
          }
          allShortestPathsComplete[counter] <- list( alist)
          counter <- counter + 1
        }
      }
    }
  }
}

allShortestPathsComplete <- allShortestPathsComplete[ !sapply( allShortestPathsComplete, is.null)] 

      #
      # calculate the cover rate function based on highest betweenness 
      #
betweennessCentrality <- betweenness( g, v = V( g), directed = TRUE)
vectroIds <- vector( mode = "numeric", length = length( betweennessCentrality))
vectorBetweenness <- vector( mode = "numeric", length = length( betweennessCentrality))
for( i in V( g)) {
  vectroIds[i] <- i
  vectorBetweenness[i] <- betweennessCentrality[[i]]
}
betweennessDf <- data.frame( vectroIds, vectorBetweenness)
betweennessDf <- betweennessDf[ order( -betweennessDf[,2] ), ]



allShortestPathsCompleteCopy <- allShortestPathsComplete
partialShortestPaths <- vector( "list", length( allShortestPathsComplete))
percentage <- vector( "list", length( V( g)))
counter <- 1

for( vector in V( g)) {
  
  set <- list()
  
  if( length( allShortestPathsCompleteCopy) > 0) {
    for( i in 1:length( allShortestPathsCompleteCopy)) {
        if( betweennessDf[[ vector,1]] %in% allShortestPathsCompleteCopy[[i]]) {
          set <- c( set, i)
        }
     }
  }
  
  
  if( length( set) > 0) {
    for( i in 1:length( set)) {
      partialShortestPaths[[counter]] <- allShortestPathsComplete[[set[[i]]]]
      allShortestPathsCompleteCopy <- allShortestPathsCompleteCopy[ -set[[ length( set) - i + 1]]]
      counter <- counter + 1 
    }
  }
  
  # cover rate function
  percentage[[vector]] <- ( counter - 1)/length( allShortestPathsComplete)
  
}
  
HBFcoverRate <- percentage