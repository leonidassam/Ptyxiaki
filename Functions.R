library( igraph )

calculatePCI <- function( g) {
  PCI = list( )
  
  for( v in V( g)) {
    neighbors <- neighbors( g, v, "out")
    
    Nodedegrees <- list( )
    for( n in neighbors) {
      Nodedegrees <-  c( Nodedegrees, ( degree( g, n, "out") +  degree( g, n, "in")))
    }
    if( length( Nodedegrees) > 0) {
      sort( Nodedegrees[[1]], decreasing = TRUE)
    }
    
    b <- 0
    if( length( Nodedegrees) > 0) {
      for( i in 1:length( Nodedegrees)) { 
        if( Nodedegrees[[i]] > b ) {    b <- b + 1      }
      }
    }
    PCI <- c( PCI, b)
  }
  return( PCI )
}


findPositions <- function( data) {
  pos1 <- 0
  pos2 <- 0
  pos3 <- 0
  for( i in data[,1]) {
    
    if(i==1){       pos1 <- pos1 + 1    }
    if(i==2){       pos2 <- pos2 + 1    }
    if(i==3){       pos3 <- pos3 + 1    }
  }
  
  return( list(pos1, pos2, pos3))
}


