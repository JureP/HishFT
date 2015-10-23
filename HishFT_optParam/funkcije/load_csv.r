load_csv <- function(tick ## tikerji zeljenih delnic
					){
	## nalozi podatke in jih shrani	 kot ticker.csv			
	for (i in tick){
		kje = paste0("http://www.thebonnotgang.com/quotes/q.php?timeframe=1m&dayFrom=2011-07-01&dayTo=&symbol=",i)
		ime = paste0(i, '.csv')
		download.file(kje, destfile = ime)
	}
}


