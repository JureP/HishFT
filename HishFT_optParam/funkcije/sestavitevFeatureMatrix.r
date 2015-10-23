sestaviFeatureMatrix <- function(	ticker, ## tiker delnice
									okoljeOHLC, ## okolje kjer je shranjen OHLC
									normalizacija = FALSE, ## ali normalizira (nekatere) featurje
									rank = TRUE, ## ali doda rangirje featurjev
									zacetek = as.Date('2011-09-22'), # zacetek train mnozive
									konecTrain = as.Date('2014-08-13'), # konec train, zacetek validation mnozice
									konecValid = as.Date('2015-01-02'), # konec validation mnozice, zacetek test mnozice
									konec = as.Date('2015-05-15') # konec test mnozice
	## sestavi feature matriko 
	##(po clanku Machine-learning classification techniques for the analysis and prediction of high-frequency stock direction),
 								){
									
	## loadanje potrebnih podatkov in sestavljanje imen
	setwd(okoljeOHLC)
	ime = paste0(ticker, '.csv')
	asset <- read.csv(ime, sep=";", dec=",")
	asset$timestamp <- as.POSIXct(asset$timestamp, format="%Y-%m-%d %H:%M:%S")
	stock <- asset[,-2]
	imeA = paste0(ticker, '.rds')
	rm(asset)
	
	## sestavi osnovno feature matriko 
	fm <- featureMatrix(stock)
	## normalizacija (nekaterih) indikatorjev
	if (normalizacija){
		fm <- normalizacijaFM(stock, fm)
	}
	## odstranitev prvih 44 (uporablja podatke prejsnega dne) in zadjnih 
	## 2 podatkov (napovedije naslednji dan) v trgovalnem dnevu
	fm <- odstranitevMinut(fm, 44,2)
	fm <- na.omit(fm)
	##
	if (rank){
		fm <- rangiranje(fm, n = 600)
	}
	## izracun donosov
	pxts <- xts(stock$close, stock$timestamp)
	donosi <- ROC(pxts, type = 'discrete')
	
	## locitev podatkov
	podLoceni <- razdelitevPodatkov(fm, donosi, n = 2, zacetek = zacetek,
						konecTrain = konecTrain, konecValid = konecValid, konec = konec)

	fm_donosi <- list(fm = fm, donosi = donosi, podLoceni = podLoceni)
	return(fm_donosi)
}


