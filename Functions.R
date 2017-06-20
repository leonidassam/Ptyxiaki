library( igraph )
library("compiler")
enableJIT( 3)


# calculates the PCI of the network 
# returns a list 
calculatePCI <- function( g, m) {
  PCI = list( )
  
  for( v in V( g)) {
    neighbors <- neighbors( g, v, "all")
    
    Nodedegrees <- list( )
    for( n in neighbors) {      Nodedegrees <-  c( Nodedegrees, degree( g, n, mode = m))    }
    
    if( length( Nodedegrees) > 0) {      sort( Nodedegrees[[1]], decreasing = TRUE)    }
    
    b <- 0
    if( length( Nodedegrees) > 0) {
      for( i in 1:length( Nodedegrees)) {         if( Nodedegrees[[i]] > b ) {    b <- b + 1      }      }
    }
    PCI <- c( PCI, b)
  }
  return( PCI)
}


# split the network
# returns a list
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


# calculates the cover rate function based on HBF
# returns a list 
coverRateFunction <- function( g, allShortestPathsComplete, df) {
  

  allShortestPathsCompleteCopy <- allShortestPathsComplete
  percentage <- vector( "list", length( V( g)))
  counter <- 1
  for( vector in V( g)) {
    
    set <- list()

    if( length( allShortestPathsCompleteCopy) > 0) {
      for( i in 1:length( allShortestPathsCompleteCopy)) {
        if( df[[vector,1]] %in% allShortestPathsCompleteCopy[[i]]) {
          set <- c( set, i)
        }
      }
    }
    
    
    if( length( set) > 0) {
      for( i in 1:length( set)) {
        counter <- counter + 1
        allShortestPathsCompleteCopy <- allShortestPathsCompleteCopy[ -set[[ length( set) - i + 1]]]
      }
    }
    # cover rate function
    percentage[[vector]] <- ( counter - 1)/length( allShortestPathsComplete)
    # print( percentage)
  }
  
  return( percentage)
}
compiledCoverRateFunction <- cmpfun( coverRateFunction)

# create sorted dataframe based on highest betweenness
# returns a dataframe
createSortedHBFDataframe <- function( g, d) {
  
  if( d == "directed") {    m = "directed"  }
  else {    m = "undirected"  }
  
  betweennessCentrality <- betweenness( g, v = V( g), directed = m)
  vectroIds <- vector( mode = "numeric", length = length( betweennessCentrality))
  vectorBetweenness <- vector( mode = "numeric", length = length( betweennessCentrality))
  for( i in V( g)) {
    vectroIds[i] <- i
    vectorBetweenness[i] <- betweennessCentrality[[i]]
  }
  df <- data.frame( vectroIds, vectorBetweenness)
  df <- df[ order( -df[,2] ), ]
  
  return( df)
}


# create sorted dataframe based on highest degree
# returns a dataframe
createSortedHDFDataframe <- function( g, d) {
  
  if( d == "directed" ){    m <- "in"}
  else { m <- "all"}
  
  degreeCentrality <- degree( g, v = V( g), mode = m)
  vectroIds <- vector( mode = "numeric", length = length( degreeCentrality))
  vectorDegree <- vector( mode = "numeric", length = length( degreeCentrality))
  for( i in V( g)) {
    vectroIds[i] <- i
    vectorDegree[i] <- degreeCentrality[[i]]
  }
  df <- data.frame( vectroIds, vectorDegree)
  df <- df[ order( -df[,2] ), ]
  
  return( df)
}


# create sorted dataframe based on highest closenness
# returns a dataframe
createSortedHCFDataframe <- function( g, d) {
  
  if( d == "directed" ){    m <- "in"}
  else { m <- "all"}
  
  closenessCentrality <- closeness( g, v = V( g), mode = m)
  vectroIds <- vector( mode = "numeric", length = length( closenessCentrality))
  vectorCloseness <- vector( mode = "numeric", length = length( closenessCentrality))
  for( i in V( g)) {
    vectroIds[i] <- i
    vectorCloseness[i] <- closenessCentrality[[i]]
  }
  df <- data.frame( vectroIds, vectorCloseness)
  df <- df[ order( -df[,2] ), ]
  
  return( df)
}


# create sorted dataframe based on highest PCI
# returns a dataframe
createSortedHPCIDataframe <- function( g, d) {
  
  if( d == "directed" ){    m <- "in"}
  else { m <- "all"}
  
  PCI <- calculatePCI( g, m)
  vectroIds <- vector( mode = "numeric", length = length( PCI))
  vectorPCI <- vector( mode = "numeric", length = length( PCI))
  for( i in V( g)) {
    vectroIds[i] <- i
    vectorPCI[i] <- PCI[[i]]
  }
  df <- data.frame( vectroIds, vectorPCI)
  df <- df[ order( -df[,2] ), ]
  
  return( df)
}


# create sorted dataframe based on highest betweenness
# returns a dataframe
createSortedHkShellDataframe <- function( g, d) {
  
  if( d == "directed" ){    m <- "in"}
  else { m <- "all"}
  
  kShellCentrality <- coreness( g, mode = m)
  vectroIds <- vector( mode = "numeric", length = length( kShellCentrality))
  vectorBetweenness <- vector( mode = "numeric", length = length( kShellCentrality))
  for( i in V( g)) {
    vectroIds[i] <- i
    vectorBetweenness[i] <- kShellCentrality[[i]]
  }
  df <- data.frame( vectroIds, vectorBetweenness)
  df <- df[ order( -df[,2] ), ]
  
  return( df)
}


# get the mean results
# return a list of lists HBF - HDF - HCF - PCI
getMeanValues <- function( g, HBFcoverRate, HDFcoverRate, HCFcoverRate, PCIcoverRate, kShellcoverRate) {
  
  HBF <- list( length( V( g)))
  HDF <- list( length( V( g)))
  HCF <- list( length( V( g)))
  PCI <- list( length( V( g)))
  kShell  <- list( length( V( g)))
  
  for( i in 1:length( V( g))) {
    HBF[[i]] <- 0
    HDF[[i]] <- 0
    HCF[[i]] <- 0
    PCI[[i]] <- 0
    kShell[[i]] <- 0
  }
  for( i in 1:iter) {
    for( j in 1:length( V( g))) {
      HBF[[ j]] <- HBF[[ j]] + HBFcoverRate[[ i]][[ j]]
      HDF[[ j]] <- HDF[[ j]] + HDFcoverRate[[ i]][[ j]]
      HCF[[ j]] <- HCF[[ j]] + HCFcoverRate[[ i]][[ j]]
      PCI[[ j]] <- PCI[[ j]] + PCIcoverRate[[ i]][[ j]]
      kShell[[ j]] <- kShell[[ j]] + kShellcoverRate[[ i]][[ j]]
    }
  }
  for( i in 1:length( V( g))) {
    HBF[[i]] <- HBF[[i]] /iter
    HDF[[i]] <- HDF[[i]] /iter
    HCF[[i]] <- HCF[[i]] /iter
    PCI[[i]] <- PCI[[i]] /iter
    kShell[[i]] <- kShell[[i]] /iter
  }
  
  return( list( HBF, HDF, HCF, PCI, kShell))
}


# plots the results of the cover rate functions
plotCoverRateFunction <- function( g, HBF, HDF, HCF, PCI, kShell, fname) {
  png( filename = fname)
  plot( V( g), main = "Cover Rate Function",  xlab = "Vertices", ylab = "Percentage",   ylim = c( 0:1))
  lines( V( g), HBF, col = "blue")
  lines( V( g), HDF, col = "red")
  lines( V( g), HCF, col = "green")
  lines( V( g), PCI, col = "black")
  lines( V( g), kShell, col = "yellow")
  legend( "bottomright", c( "HBF", "HDF", "HCF", "PCI", "kShell"), lty = c( 1, 1), lwd = c( 2, 2),col = c( "blue", "red", "green", "black", "yellow"))
  dev.off()
}


# plots the number of check ing nodes for every centrality
plotNumberOfCheckInNodes <- function ( g, numberOfNodes, CheckInNodesHBF, CheckInNodesHDF, CheckInNodesHCF, CheckInNodesPCI, CheckInNodeskShell, fname){
                            
  png( filename = fname)
  plot( 1, main = "Number of check in nodes",  xlab = "Vertices", xlim = c( 0,length( V( g))), ylim = c( 0,length( V( g))))
  lines( numberOfNodes, CheckInNodesHBF, col = "blue")
  lines( numberOfNodes, CheckInNodesHDF, col = "red")
  lines( numberOfNodes, CheckInNodesHCF, col = "green")
  lines( numberOfNodes, CheckInNodesPCI, col = "black")
  lines( numberOfNodes, CheckInNodeskShell, col = "brown")
  legend( "bottomright", c( "HBF", "HDF", "HCF", "PCI", "kShell"), lty = c( 1, 1), lwd = c( 2, 2),col = c( "blue", "red", "green", "black", "brown"))
  dev.off()
}


# extract the results to file
resultsToFile <- function( numberOfNodes, CheckInNodesHBF, CheckInNodesHDF, CheckInNodesHCF, CheckInNodesPCI, CheckInNodeskShell, edges, avDegree, fname) {
  
  lapply( "number Of nodes ", write, toString( fname), append = FALSE)
  lapply( numberOfNodes, write, toString( fname), append = TRUE)
  lapply( "numberOfCheckInNodesHBF", write, toString( fname), append = TRUE)
  lapply( CheckInNodesHBF, write, toString( fname), append = TRUE)
  lapply( "numberOfCheckInNodesHDF", write, toString( fname), append = TRUE)
  lapply( CheckInNodesHDF, write, toString( fname), append = TRUE)
  lapply( "numberOfCheckInNodesHCF", write, toString( fname), append = TRUE)
  lapply( CheckInNodesHCF, write, toString( fname), append = TRUE)
  lapply( "numberOfCheckInNodesPCI", write, toString( fname), append = TRUE)
  lapply( CheckInNodesPCI, write, toString( fname), append = TRUE)
  lapply( "numberOfCheckInNodeskShell", write, toString( fname), append = TRUE)
  lapply( CheckInNodeskShell, write, toString( fname), append = TRUE)
  lapply( "Number of edges", write, toString( fname), append = TRUE)
  lapply( edges, write, toString( fname), append = TRUE)
  lapply( "Average Degree", write, toString( fname), append = TRUE)
  lapply( avDegree, write, toString( fname), append = TRUE)
}


# plot the cover rate function for each centrality
plotTheCoverRateForEachCentrality <- function( g, centrality, BA, RA, title, fname) {
  
  png( filename = fname)
  plot( V( g), main = title,  xlab = "Vertices", ylab = "Percentage",   ylim = c( 0:1))
  lines( V( g), BA, col = "blue")
  lines( V( g), RA, col = "red")
  legend( "bottomright", c( "BA", "RA"), lty = c( 1, 1), lwd = c( 2, 2),col = c( "blue", "red"))
  dev.off()
}