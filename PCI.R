library( igraph )

calculatePCI <- function() {
	PCI = numeric( )
	for( v in V( g)) {
		neighbors <- neighbors( g, v, "out")
		degrees <- numeric( )
		for( n in neighbors) {
			degrees <-  c( degrees, ( degree( g, n, "out") +  degree( g, n, "in")))
		}
		sort( degrees)
		
		b <- 0
		for( i in 1:length( degrees)) { 
			if( degrees[i] > b ) {
				b <- b + 1
			}
		}
		PCI <- c( PCI, b)
	}
	return( PCI )
}