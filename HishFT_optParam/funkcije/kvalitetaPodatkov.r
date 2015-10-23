kvalitetaPodatkov <- function(tick, okolje)
	## izracuna kvaliteto podatkov za funkcje iz tick,
	## podatki so v datkoteki okolje v obliki Ticker.csv
	{
	setwd(okolje)
	podatki  <- data.frame(tick, matrix(NA, length(tick),6))
	names(podatki) <- c('tick', 'lq', 'uq', 'dolzina', 'st razlic', 'first day', 'last day')
	j = 1
	for (i in tick){
		print(i)
		ime = paste0(i, '.csv')
		asset <- read.csv(ime, sep=";", dec=",")
		asset$timestamp <- as.POSIXct(asset$timestamp, format="%Y-%m-%d %H:%M:%S")
		stock <- asset[,-2]
		
		podatkiCas <- stock[,2]
		zakljuckiTrgovanja <- podatkiCas[endpoints(podatkiCas, on = 'days', k=1)]
		poVrsti <- xts(1:length(podatkiCas), podatkiCas)
		lastMin <- as.vector(poVrsti[zakljuckiTrgovanja])
		lastMin <- c(0,lastMin)
		
		stMin <- diff(lastMin)
		#plot(stMin)
		#hist(stMin)
		podatki[j,2:3] <- (quantile(stMin, c(0.1, 0.9)))
		podatki[j,4] <- length(stock$close)
		podatki[j,5] <- length(unique(stock$close))
		podatki[j,6] <- as.character(as.Date(podatkiCas[1]))
		podatki[j,7] <- as.character(as.Date(podatkiCas[length(podatkiCas)]))
		j = j+1
	}
	return(podatki)
}

