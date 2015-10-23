odstranitevMinut <- function(	podatki, ## feature matrix: data.frame oblike [ticker, time, features]
								n, ## stevilo prvih odstranjenih vnosov
								k ## stevilo odstranjenih zadnjih vnosov
	){
	## namesto prvih n podatkov in zadnjih k podatkov v trgovalnem dnevu zapise NA 
	podatkiCas <- podatki[,2]
	zakljuckiTrgovanja <- podatkiCas[endpoints(podatkiCas, on = 'days', k=1)]
	poVrsti <- xts(1:length(podatkiCas), podatkiCas)
	
	for(i in -(k-1):n){
		odstranitev <- poVrsti[zakljuckiTrgovanja]+i
		odstranitev <- odstranitev[-length(odstranitev)]
		podatki[odstranitev, 3:ncol(podatki)] <- NA
	}
	return(podatki)
}
