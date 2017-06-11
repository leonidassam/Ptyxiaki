library( igraph)
source( "Functions.R")


iter <- 3
HBFcoverRate <- list( iter)
HDFcoverRate <- list( iter)
HCFcoverRate <- list( iter)
PCIcoverRate <- list( iter)

for( z in 1:iter) {
  
  g <- sample_pa( 50, m = 2, directed = TRUE)
  
  allShortestPaths <- list()
  allShortestPathsComplete <- list()
  for( i in V( g)) {
    for( j in V( g)) {
      # do not compute loops 
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
            allShortestPathsComplete <- c( allShortestPathsComplete, list( alist))
          }
        }
      }
    }
  }
  print( length( allShortestPaths))
  
  
        # calculate the cover rate function based on highest betweenness 
  HBetweennessDf <- createSortedHBFDataframe( g )
  HBFcoverRate[[z]] <- coverRateFunction( g, allShortestPaths, HBetweennessDf)
  print( "betweenness ")
  
  
        # calculate the cover rate function based on highest degree 
  HDegreeDf <- createSortedHDFDataframe( g)
  HDFcoverRate[[z]] <- coverRateFunction( g, allShortestPaths, HDegreeDf)
  print( "high degree")

  
        # calculate the cover rate function based on highest closeness 
  HClosenessDf <- createSortedHCFDataframe( g)
  HCFcoverRate[[z]] <- coverRateFunction( g, allShortestPaths, HClosenessDf)
  print( "closeness ")
  

        # calculate the cover rate function based on highest power community index ( PCI) 
  HPCIDf <- createSortedHPCIDataframe( g)
  PCIcoverRate[[z]] <- coverRateFunction( g, allShortestPaths, HPCIDf)
  print( "PCI")
}

# get the mean values of the simulations
# HBF - HDF - HCF - PCI the order of the results
results <- getMeanValues( HBFcoverRate, HDFcoverRate, HCFcoverRate, HPCIDf)


# plot the results
# result[[1]] = HBF, result[[2]] = HDF, result[[3]] = HCF, result[[4]] = PCI
plotTheResults( g, results[[1]], results[[2]], results[[3]], results[[4]])