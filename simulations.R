print( getwd())
setwd( "C:/CheckInNodes")
print( getwd())

library( igraph)
library("compiler")
source( "Functions.R")

# network size
numberOfNodes <- c( 220)

# number of simulations for networks of size N
iter <- 3

# List for directed / undirected networks
directedList <- c( "directed", "undirected")

# dense or not network
# e.g. BA networks: For network size n, each new node will add n/20 or n/5 edges
dense <- c( 5, 20)






for( d in directedList) {
  
  print( d)

  for( dd in dense) {
    
    print( dd)
    
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
                                          # 
                                          #  BA NETWORKS
                                          #
      
      for( z in 1:iter) {
        
        BAgraph <- sample_pa( n, m = n/dd, directed = d)
        
        allShortestPathsComplete <- vector( "list",  2*n^2)
        k <- 1
        for( i in V( BAgraph)) {
          for( j in V( BAgraph)) {
            # do not compute loops 
            if( i != j ) {
              shortestPaths <- shortest_paths( BAgraph, i, j, mode = c("out"))
              for( x in 1:length( shortestPaths[[1]])) {
                if( length( shortestPaths[[1]][[x]]) > 0 ) {
                  alist <- list()
                  for( ii in 1:length( shortestPaths[[1]][[x]])) {
                    alist <- c( alist, shortestPaths[[1]][[x]][[ii]])
                  }
                  allShortestPathsComplete[k] <- list( alist)
                  k <- k + 1
                }
              }
            }
          }
        }
        allShortestPathsComplete <- allShortestPathsComplete[ !sapply( allShortestPathsComplete, is.null)] 
        
        
              # calculate the cover rate function based on highest betweenness 
        HBetweennessDf <- createSortedHBFDataframe( BAgraph, d)
        BAHBFcoverRate[[z]] <- compiledCoverRateFunction( BAgraph, allShortestPathsComplete, HBetweennessDf)
        
        
              # calculate the cover rate function based on highest degree 
        HDegreeDf <- createSortedHDFDataframe( BAgraph, d)
        BAHDFcoverRate[[z]] <- compiledCoverRateFunction( BAgraph, allShortestPathsComplete, HDegreeDf)
      
        
              # calculate the cover rate function based on highest closeness 
        HClosenessDf <- createSortedHCFDataframe( BAgraph, d)
        BAHCFcoverRate[[z]] <- compiledCoverRateFunction( BAgraph, allShortestPathsComplete, HClosenessDf)
        
      
              # calculate the cover rate function based on highest power community index ( PCI) 
        HPCIDf <- createSortedHPCIDataframe( BAgraph, d)
        BAPCIcoverRate[[z]] <- compiledCoverRateFunction( BAgraph, allShortestPathsComplete, HPCIDf)
      }
      
        # get the mean values of the simulations
        # HBF - HDF - HCF - PCI the order of the results
      results <- getMeanValues( BAgraph, BAHBFcoverRate, BAHDFcoverRate, BAHCFcoverRate, BAPCIcoverRate)
      
      
      BAHBF <- results[[1]]
      BAHDF <- results[[2]]
      BAHCF <- results[[3]]
      BAPCI <- results[[4]]
      
      
        # count the number of check in nodes
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
      image <- c( "Cover Rate Function", "BA network", d, dd, "edges", n, "nodes", ".png")
      plotCoverRateFunction( BAgraph, BAHBF, BAHDF, BAHCF, BAPCI, toString( image))
    
      
    
      
                                    # 
                                    #  RA NETWORKS
                                    #
      
      for( z in 1:iter) {
        
        RAgraph <- erdos.renyi.game( n, length( E( BAgraph)), type = c( "gnm"), directed = d)
        
    
        allShortestPathsComplete <- vector( "list",  2*n^2)
        k <- 1
        for( i in V( RAgraph)) {
          for( j in V( RAgraph)) {
            # do not compute loops 
            if( i != j ) {
              shortestPaths <- shortest_paths( RAgraph, i, j, mode = c("out"))
              for( x in 1:length( shortestPaths[[1]])) {
                if( length( shortestPaths[[1]][[x]]) > 0 ) {
                  alist <- list()
                  for( ii in 1:length( shortestPaths[[1]][[x]])) {
                    alist <- c( alist, shortestPaths[[1]][[x]][[ii]])
                  }
                  allShortestPathsComplete[k] <- list( alist)
                  k <- k + 1
                }
              }
            }
          }
        }
        allShortestPathsComplete <- allShortestPathsComplete[ !sapply( allShortestPathsComplete, is.null)] 
        
        
        # calculate the cover rate function based on highest betweenness 
        HBetweennessDf <- createSortedHBFDataframe( RAgraph, d)
        RAHBFcoverRate[[z]] <- compiledCoverRateFunction( RAgraph, allShortestPathsComplete, HBetweennessDf)
        
        
        # calculate the cover rate function based on highest degree 
        HDegreeDf <- createSortedHDFDataframe( RAgraph, d)
        RAHDFcoverRate[[z]] <- compiledCoverRateFunction( RAgraph, allShortestPathsComplete, HDegreeDf)
        
        
        # calculate the cover rate function based on highest closeness 
        HClosenessDf <- createSortedHCFDataframe( RAgraph, d)
        RAHCFcoverRate[[z]] <- compiledCoverRateFunction( RAgraph, allShortestPathsComplete, HClosenessDf)
        
        
        # calculate the cover rate function based on highest power community index ( PCI) 
        HPCIDf <- createSortedHPCIDataframe( RAgraph, d)
        RAPCIcoverRate[[z]] <- compiledCoverRateFunction( RAgraph, allShortestPathsComplete, HPCIDf)
      }
      
        # get the mean values of the simulations
        # HBF - HDF - HCF - PCI the order of the results
      results <- getMeanValues( RAgraph, RAHBFcoverRate, RAHDFcoverRate, RAHCFcoverRate, RAPCIcoverRate)
      
      RAHBF <- results[[1]]
      RAHDF <- results[[2]]
      RAHCF <- results[[3]]
      RAPCI <- results[[4]]
      
      
      # counte the number of check in nodes
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
      image <- c( "Cover Rate Function", "RA network", d, dd, "edges", n, "nodes", ".png")
      plotCoverRateFunction( RAgraph, RAHBF, RAHDF, RAHCF, RAPCI, toString( image))
    
    
        #
        # plot the cover rate functions for BA and RA networks for each centrality
      image <- c( "Cover rate functions for HBF", d, dd, "edges", n, "nodes", ".png")
      title <- c( "Cover rate function for HBF")
      plotTheCoverRateForEachCentrality( RAgraph, "HBF", BAHBF, RAHBF, title, toString( image)) 
      image <- c( "Cover rate functions for HDF", d, dd, "edges", n, "nodes", ".png")
      title <- c( "Cover rate function for HDF")
      plotTheCoverRateForEachCentrality( RAgraph, "HDF", BAHDF, RAHDF, title, toString( image)) 
      image <- c( "Cover rate functions for HCF", d, dd, "edges", n, "nodes", ".png")
      title <- c( "Cover rate function for HCF")
      plotTheCoverRateForEachCentrality( RAgraph, "HCF", BAHCF, RAHCF, title, toString( image)) 
      image <- c( "Cover rate functions for PCI", d, dd, "edges", n, "nodes", ".png")
      title <- c( "Cover rate function for PCI")
      plotTheCoverRateForEachCentrality( RAgraph, "PCI", BAPCI, RAPCI, title, toString( image)) 
    
      
      # results to file
      outputFile <- c( "BA Number Of CheckIn Nodes", d, dd, "edges", n, "nodes", ".txt")
      resultsToFile( numberOfNodes, BAnumberOfCheckInNodesHBF, BAnumberOfCheckInNodesHDF, BAnumberOfCheckInNodesHCF, BAnumberOfCheckInNodesPCI, toString( outputFile))
      # results to file
      outputFile <- c( "RA Number Of CheckIn Nodes", d, dd, "edges", n, "nodes", ".txt")
      resultsToFile( numberOfNodes, RAnumberOfCheckInNodesHBF, RAnumberOfCheckInNodesHDF, RAnumberOfCheckInNodesHCF, RAnumberOfCheckInNodesPCI, toString( outputFile))
      
    }
    
        # plot the number of chceck in nodes
    fname <- c( "Number of check in nodes", "BA network", d, dd, "edges", n, "nodes", ".png")
    plotNumberOfCheckInNodes( BAgraph, numberOfNodes, BAnumberOfCheckInNodesHBF, BAnumberOfCheckInNodesHDF, BAnumberOfCheckInNodesHCF, BAnumberOfCheckInNodesPCI, toString( fname))
        # plot the number of chceck in nodes
    fname <- c( "Number of check in nodes", "RA network", d, dd, "edges", n, "nodes", ".png")
    plotNumberOfCheckInNodes( RAgraph, numberOfNodes, RAnumberOfCheckInNodesHBF, RAnumberOfCheckInNodesHDF, RAnumberOfCheckInNodesHCF, RAnumberOfCheckInNodesPCI, toString( fname))
  }
}