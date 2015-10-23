odstopajoci <- function(
	## vrne seznam feature matrike (brez symbol in timestamp) in zamaknjenih donosov za q1 del dni z najvecim padcem in q2 del najbolj donosnih dni
	featureMatrix, # data frame feature matrika (s symbol in timestmp) (z odstranjenimi prvimi minutami)
	donosi, # vektor donosov (nezamaknjen, z prvimi minutami)
	spodnjiKvantil = 0.2, # delez najnizjih donosov, ki jih vzamemo
	zgornjiKvantil = 0.2,# delez najvisjih donosov, ki jih vzamemo
	n  = 2  # zamik s katerim napovedujemo (v casu 0 napovedujemo za cas n)
	
	){
	### cas
	cas <- featureMatrix$timestamp
	
	### zamik donosov za n
	zamikDonos <- lag(donosi,-n)
	donosiOdst <- zamikDonos[cas]
	
	y <- as.vector(donosiOdst)
	y <- na.omit(y)
	#x <- featureMatrix[1:length(y),3:ncol(featureMatrix)]
	x <- featureMatrix[1:length(y),1:ncol(featureMatrix)]
	### izbor izstopajocih
	
	d <- spodnjiKvantil
	u <- 1-zgornjiKvantil
	
	q1 <- quantile(y, d)
	q2 <- quantile(y, u)
	
	xb <- x[y > q2 | y < q1,]
	yb <- y[y > q2 | y < q1]
	izstopajoci <- list('xb' = xb, 'yb' = yb)
	return(izstopajoci)	
	}





