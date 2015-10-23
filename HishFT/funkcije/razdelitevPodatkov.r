razdelitevPodatkov <- function(
	## vrne seznam, ki vsebuje: xTrain, yTrain (podatki za train);
	## xValid, yValid (podatki za validation); xTest, yTest (podatki za test)
	##[x... feature matrika, y... donosi]
	## podatke razdeli glede na st dni (ne st podatkov)
	featureMatrix, # data frame feature matrika (z odstranjenimi prvimi minutami)
	donosi, # vektor donosov (nezamaknjen, z prvimi minutami)
	n = 2, # zamik s katerim napovedujemo (v casu 0 napovedujemo za cas n)
	zacetek = as.Date('2011-09-22'), # zacetek train mnozive
	konecTrain = as.Date('2014-08-13'), # konec train, zacetek validation mnozice
	konecValid = as.Date('2015-01-02'), # konec validation mnozice, zacetek test mnozice
	konec = as.Date('2015-05-15') # konec test mnozice
	){
	### cas
	cas <- featureMatrix$timestamp
	
	#datumi <- unique(as.Date(cas))
	
	## zamik donosov za n
	zamikDonos <- lag(donosi,-n)
	### odstranitev prvih minut iz zamaknjenih donosov
	donosiOdst <- zamikDonos[cas]
		
	#delTrain <- 1 - delVal-delTest
	#delVal <- delTrain+delVal
	
	#stDni <- length(datumi)
	
	#stDniTrain <- round(stDni*delTrain)
	#stDniValid <- round(stDni*delVal)
	
	#zacetek <- datumi[1]
	#konecTrain <- datumi[stDniTrain]
	#konecValid <- datumi[stDniValid]
	#konec <- datumi[stDni]
	
	
	xTrain <- featureMatrix[as.Date(featureMatrix$timestamp) >= zacetek & as.Date(featureMatrix$timestamp)  <= konecTrain, ]
	xValid <- featureMatrix[as.Date(featureMatrix$timestamp) > konecTrain & as.Date(featureMatrix$timestamp)  <= konecValid, ]
	xTest <- featureMatrix[as.Date(featureMatrix$timestamp) > konecValid & as.Date(featureMatrix$timestamp)  <= konec, ]
	
		
	yTrain <- donosiOdst[as.Date(featureMatrix$timestamp) >= zacetek & as.Date(featureMatrix$timestamp)  <= konecTrain, ]
	yValid <- donosiOdst[as.Date(featureMatrix$timestamp) > konecTrain & as.Date(featureMatrix$timestamp)  <= konecValid, ]
	yTest <- donosiOdst[as.Date(featureMatrix$timestamp) > konecValid & as.Date(featureMatrix$timestamp)  <= konec, ]
	
	# xTrain <- featureMatrix[1:round(0.8*stObdobji),] 
	# xValid <- featureMatrix[(round(0.8*stObdobji)+1):(round(0.9*stObdobji)-1),] 
	# xTest <- featureMatrix[(round(0.9*stObdobji)):stObdobji,] 
	
	# yTrain <- donosiOdst[1:round(0.8*stObdobji),] 
	# yValid <- donosiOdst[(round(0.8*stObdobji)+1):(round(0.9*stObdobji)-1),] 
	# yTest <- donosiOdst[(round(0.9*stObdobji)):stObdobji,] 
	
	loceni <- list('xTrain' = xTrain,'yTrain' =  yTrain,'xValid' = xValid, 'yValid' = yValid,'xTest' = xTest,'yTest' = yTest)
	return(loceni)
	}

	