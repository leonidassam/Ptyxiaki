library( igraph )
source( "Functions.R")

# read the data
Data <- read.csv( "./Network.csv", header = FALSE )
DataEdgeList <- Data[,1:2]
g <- graph_from_edgelist( data.matrix( DataEdgeList), directed = TRUE)


betweennessCentrality <- betweenness( g, v = V( g), directed = TRUE)
# List with all shortest paths from network
allShortestPaths <- list()
allShortestPaths1 <- list()
for( i in V(g)) {
  for( j in V(g)) {
    if( i != j ) {
      shortestPaths <- shortest_paths( g, i, j, mode = c("out"))
      for( x in 1:length( shortestPaths[[1]])) {
        listToAdd <- list()
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
percentage <- list( length( V( g)))
counter <- 1
nodes <- list(  4,3,1,2,5,6)
for( j in V( g)) {
  nodeOutShortestPaths <- shortest_paths( g, nodes[[j]], mode = c( "out"))
  nodeInShortestPaths <- shortest_paths( g, nodes[[j]], mode = c( "in"))
  
  set <- list()
  for( ii in 1:length( nodeOutShortestPaths[[1]])) {
    path <- list( unlist( nodeOutShortestPaths[[1]][[ii]]))
    listToAdd <- list()
    if( length( path[[1]]) > 1) {
      listToAdd <- c( listToAdd, nodes[[j]])
      listToAdd <- c( listToAdd, path[[1]][[length( path[[1]])]])
      set <- c( set, list( listToAdd))
    }
  }
  
  for( ii in 1:length( nodeInShortestPaths[[1]])) {
    path <- list( unlist( nodeInShortestPaths[[1]][[ii]]))
    listToAdd <- list()
    if( length( path[[1]]) > 1) {
      listToAdd <- c( listToAdd, path[[1]][[length( path[[1]])]])
      listToAdd <- c( listToAdd, nodes[[j]])
      set <- c( set, list( listToAdd))
    }
  }
  
  if( length( allShortestPaths1) > 0) {
    for( ii in 1:length( allShortestPaths1)) {
      path <- list( unlist( allShortestPaths1[[ii]]))
      listToAdd <- list()
      if( nodes[[j]] %in% allShortestPaths1[[ii]]) {
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

# For each node get every shortest path. Add them to a list 
allShortestPathsCopy <- allShortestPaths
partialShortestPaths <- list( length( allShortestPaths))
percentagePCI <- list( length( V( g)))
counter <- 1
PCI <- calculatePCI( g)
vectroIds <- vector( mode = "numeric", length = length( PCI))
vectorDegrees <- vector( mode = "numeric", length = length( PCI))
for( i in V( g)) {
  vectroIds[i] <- i
  vectorDegrees[i] <- PCI[i]
}
degreesDf <- data.frame( vectroIds, vectorDegrees)
degreesDf <- degreesDf[ order( -degreesDf[,2] ), ]

for( j in V( g)) {
  nodeOutShortestPaths <- shortest_paths( g, degreesDf[j,1], mode = c( "out"))
  nodeInShortestPaths <- shortest_paths( g, degreesDf[j,1], mode = c( "in"))
  
  set <- list()
  for( ii in 1:length( nodeOutShortestPaths[[1]])) {
    path <- list( unlist( nodeOutShortestPaths[[1]][[ii]]))
    listToAdd <- list()
    if( length( path[[1]]) > 1) {
      listToAdd <- c( listToAdd, nodes[[j]])
      listToAdd <- c( listToAdd, path[[1]][[length( path[[1]])]])
      set <- c( set, list( listToAdd))
    }
  }
  
  for( ii in 1:length( nodeInShortestPaths[[1]])) {
    path <- list( unlist( nodeInShortestPaths[[1]][[ii]]))
    listToAdd <- list()
    if( length( path[[1]]) > 1) {
      listToAdd <- c( listToAdd, path[[1]][[length( path[[1]])]])
      listToAdd <- c( listToAdd, nodes[[j]])
      set <- c( set, list( listToAdd))
    }
  }
  
  if( length( allShortestPaths1) > 0) {
    for( ii in 1:length( allShortestPaths1)) {
      path <- list( unlist( allShortestPaths1[[ii]]))
      listToAdd <- list()
      if( nodes[[j]] %in% allShortestPaths1[[ii]]) {
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
  percentagePCI[[j]] <- ( counter - 1)/length( allShortestPaths)
  # print( percentage)
}