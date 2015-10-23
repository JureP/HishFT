zamik <- function(
	## funkcija zamakne zamakne donose vzame zgolj tisti del donosov, ki je tudi v feature matrix
	## funkcija vrne donose 
	featureMatrix, 
	donosi,
	n = 2 ## zamik
	){
	
	cas <- featureMatrix$timestamp
	
	### zamik donosov za n
	zamikDonos <- lag(donosi,-n)
	donos <- zamikDonos[cas]
	return(donos)
	}
	