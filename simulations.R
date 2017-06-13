library( igraph)
source( "Functions.R")

# network size
numberOfNodes <- c( 10, 15)
# number of simulations for networks of size N
iter <- 3



# Lists to keep the results of the simulations. Will use them to get the mean values
BAHBFcoverRate <- list( )
BAHDFcoverRate <- list( )
BAHCFcoverRate <- list( )
BAPCIcoverRate <- list( )

# List to remember the number of ckeck in nodes for each network size
BAnumberOfCheckInNodesHBF <- list( )
BAnumberOfCheckInNodesHDF <- list( )
BAnumberOfCheckInNodesHCF <- list( )
BAnumberOfCheckInNodesPCI <- list( )


# Lists to keep the results of the simulations. Will use them to get the mean values
RAHBFcoverRate <- list( )
RAHDFcoverRate <- list( )
RAHCFcoverRate <- list( )
RAPCIcoverRate <- list( )

# List to remember the number of ckeck in nodes for each network size
RAnumberOfCheckInNodesHBF <- list( )
RAnumberOfCheckInNodesHDF <- list( )
RAnumberOfCheckInNodesHCF <- list( )
RAnumberOfCheckInNodesPCI <- list( )



              
counter <- 1
for( n in numberOfNodes) {

  print( n)
  
  
  
                                      # 
                                      #  BA NETWORKS
                                      #
  
  for( z in 1:iter) {
    
    BAgraph <- sample_pa( n, m = n/10, directed = TRUE)
    
    allShortestPaths <- list()
    allShortestPathsComplete <- list()
    for( i in V( BAgraph)) {
      for( j in V( BAgraph)) {
        # do not compute loops 
        if( i != j ) {
          shortestPaths <- shortest_paths( BAgraph, i, j, mode = c("out"))
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
              allShortestPathsComplete <- c( allShortestPathsComplete, list( alist))
            }
          }
        }
      }
    }
    
          # calculate the cover rate function based on highest betweenness 
    HBetweennessDf <- createSortedHBFDataframe( BAgraph )
    BAHBFcoverRate[[z]] <- coverRateFunction( BAgraph, allShortestPaths, HBetweennessDf)
    
    
          # calculate the cover rate function based on highest degree 
    HDegreeDf <- createSortedHDFDataframe( BAgraph)
    BAHDFcoverRate[[z]] <- coverRateFunction( BAgraph, allShortestPaths, HDegreeDf)
  
    
          # calculate the cover rate function based on highest closeness 
    HClosenessDf <- createSortedHCFDataframe( BAgraph)
    BAHCFcoverRate[[z]] <- coverRateFunction( BAgraph, allShortestPaths, HClosenessDf)
    
  
          # calculate the cover rate function based on highest power community index ( PCI) 
    HPCIDf <- createSortedHPCIDataframe( BAgraph)
    BAPCIcoverRate[[z]] <- coverRateFunction( BAgraph, allShortestPaths, HPCIDf)
  }
  
  # get the mean values of the simulations
  # HBF - HDF - HCF - PCI the order of the results
  results <- getMeanValues( BAgraph, BAHBFcoverRate, BAHDFcoverRate, BAHCFcoverRate, BAPCIcoverRate)
  
  BAHBF <- results[[1]]
  BAHDF <- results[[2]]
  BAHCF <- results[[3]]
  BAPCI <- results[[4]]
  
  
  for( i in 1:length( results[[1]])) {
    if( results[[1]][[i]] == 1) {
      BAnumberOfCheckInNodesHBF <- c( BAnumberOfCheckInNodesHBF, i)
      break
    }
  }
  for( i in 1:length( results[[2]])) {
    if( results[[2]][[i]] == 1) {
      BAnumberOfCheckInNodesHDF <- c( BAnumberOfCheckInNodesHDF, i)
      break
    }
  }
  for( i in 1:length( results[[3]])) {
    if( results[[3]][[i]] == 1) {
      BAnumberOfCheckInNodesHCF <- c( BAnumberOfCheckInNodesHCF, i)
      break
    }
  }
  for( i in 1:length( results[[4]])) {
    if( results[[4]][[i]] == 1) {
      BAnumberOfCheckInNodesPCI <- c( BAnumberOfCheckInNodesPCI, i)
      break
    }
  }
  
  # plot the cover rate function
  # result[[1]] = HBF, result[[2]] = HDF, result[[3]] = HCF, result[[4]] = PCI
  image <- c( "Cover Rate Function", "BA network", "directed", n, "nodes", ".png")
  plotCoverRateFunction( BAgraph, BAHBF, BAHDF, BAHCF, BAPCI, toString( image))

  
  
  
  
                                # 
                                #  RA NETWORKS
                                #
  
  for( z in 1:iter) {
    
    RAgraph <- erdos.renyi.game( n, length( E( BAgraph)), type = c( "gnm"), directed = TRUE)
    
    allShortestPaths <- list()
    allShortestPathsComplete <- list()
    for( i in V( RAgraph)) {
      for( j in V( RAgraph)) {
        # do not compute loops 
        if( i != j ) {
          shortestPaths <- shortest_paths( RAgraph, i, j, mode = c("out"))
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
              allShortestPathsComplete <- c( allShortestPathsComplete, list( alist))
            }
          }
        }
      }
    }
    
    # calculate the cover rate function based on highest betweenness 
    HBetweennessDf <- createSortedHBFDataframe( RAgraph )
    RAHBFcoverRate[[z]] <- coverRateFunction( RAgraph, allShortestPaths, HBetweennessDf)
    
    
    # calculate the cover rate function based on highest degree 
    HDegreeDf <- createSortedHDFDataframe( RAgraph)
    RAHDFcoverRate[[z]] <- coverRateFunction( RAgraph, allShortestPaths, HDegreeDf)
    
    
    # calculate the cover rate function based on highest closeness 
    HClosenessDf <- createSortedHCFDataframe( RAgraph)
    RAHCFcoverRate[[z]] <- coverRateFunction( RAgraph, allShortestPaths, HClosenessDf)
    
    
    # calculate the cover rate function based on highest power community index ( PCI) 
    HPCIDf <- createSortedHPCIDataframe( RAgraph)
    RAPCIcoverRate[[z]] <- coverRateFunction( RAgraph, allShortestPaths, HPCIDf)
  }
  
  # get the mean values of the simulations
  # HBF - HDF - HCF - PCI the order of the results
  results <- getMeanValues( RAgraph, RAHBFcoverRate, RAHDFcoverRate, RAHCFcoverRate, RAPCIcoverRate)
  
  RAHBF <- results[[1]]
  RAHDF <- results[[2]]
  RAHCF <- results[[3]]
  RAPCI <- results[[4]]
  
  for( i in 1:length( results[[1]])) {
    if( results[[1]][[i]] == 1) {
      RAnumberOfCheckInNodesHBF <- c( RAnumberOfCheckInNodesHBF, i)
      break
    }
  }
  for( i in 1:length( results[[2]])) {
    if( results[[2]][[i]] == 1) {
      RAnumberOfCheckInNodesHDF <- c( RAnumberOfCheckInNodesHDF, i)
      break
    }
  }
  for( i in 1:length( results[[3]])) {
    if( results[[3]][[i]] == 1) {
      RAnumberOfCheckInNodesHCF <- c( RAnumberOfCheckInNodesHCF, i)
      break
    }
  }
  for( i in 1:length( results[[4]])) {
    if( results[[4]][[i]] == 1) {
      RAnumberOfCheckInNodesPCI <- c( RAnumberOfCheckInNodesPCI, i)
      break
    }
  }
  
  # plot the cover rate function
  # result[[1]] = HBF, result[[2]] = HDF, result[[3]] = HCF, result[[4]] = PCI
  image <- c( "Cover Rate Function", "RA network", "directed", n, "nodes", ".png")
  plotCoverRateFunction( RAgraph, RAHBF, RAHDF, RAHCF, RAPCI, toString( image))

  
  image <- c( "Cover rate functions for HBF", n, "nodes", ".png")
  plotTheCoverRateForEachCentrality( RAgraph, "HBF", BAHBF, RAHBF, toString( image)) 
  
  image <- c( "Cover rate functions for HDF", n, "nodes", ".png")
  plotTheCoverRateForEachCentrality( RAgraph, "HDF", BAHDF, RAHDF, toString( image)) 
  
  image <- c( "Cover rate functions for HCF", n, "nodes", ".png")
  plotTheCoverRateForEachCentrality( RAgraph, "HCF", BAHCF, RAHCF, toString( image)) 
  
  image <- c( "Cover rate functions for PCI", n, "nodes", ".png")
  plotTheCoverRateForEachCentrality( RAgraph, "PCI", BAPCI, RAPCI, toString( image)) 

  
}


# results to file
outputFile <- c( "BA Number Of CheckIn Nodes", ".txt")
resultsToFile( numberOfNodes, BAnumberOfCheckInNodesHBF, BAnumberOfCheckInNodesHDF, BAnumberOfCheckInNodesHCF, BAnumberOfCheckInNodesPCI, toString( outputFile))


# plot the number of chceck in nodes
fname <- c( "Number of check in nodes", "BA network", "undirected", n, "nodes", ".png")
plotNumberOfCheckInNodes( BAgraph, numberOfNodes, BAnumberOfCheckInNodesHBF, BAnumberOfCheckInNodesHDF, BAnumberOfCheckInNodesHCF, BAnumberOfCheckInNodesPCI, toString( fname))




# results to file
outputFile <- c( "RA Number Of CheckIn Nodes", ".txt")
resultsToFile( numberOfNodes, RAnumberOfCheckInNodesHBF, RAnumberOfCheckInNodesHDF, RAnumberOfCheckInNodesHCF, RAnumberOfCheckInNodesPCI, toString( outputFile))


# plot the number of chceck in nodes
fname <- c( "Number of check in nodes", "RA network", "undirected", n, "nodes", ".png")
plotNumberOfCheckInNodes( RAgraph, numberOfNodes, RAnumberOfCheckInNodesHBF, RAnumberOfCheckInNodesHDF, RAnumberOfCheckInNodesHCF, RAnumberOfCheckInNodesPCI, toString( fname))
