rangiranje <- function(	featureMatrix, ## data frame, ki mu zelimo dodati rangirane podatke
						n = 600, ## za koliko podatkov nazaj naj funkcija primerja vrednost indikatorjev
						lenT = 100, ## koliko podatkov naj vzame funkcija za dolocanje ali je indikator smiselno rangirati
						difT = 10 ## koliko razlicnih vrednosti mora biti v prvih lenT vrednostih indikatorja, da ga funkcija vzame kot smiselnegega za rangiranje
	){
	## funkcija feature matriki doda rangirane vrednosti pri indikatorjih kjer je to smiselno
	
	## sezam imen kjer je rangiranje smiselno
	rangiranje <- NULL
	for(i in names(featureMatrix)){
		#i = names(featureMatrix)[1]
		stRazEl <- length(unique(featureMatrix[1:lenT,i]))
		if(stRazEl > difT & is.numeric(featureMatrix[1:3,i])){	rangiranje <- c(rangiranje,i)}
	}
	
	
	#n = 600 # za koliko minut nazaj primerjamo
	for(i in rangiranje){
		ime <- paste0(i,'rang')
		A <- featureMatrix[,i]
		A <- fts(featureMatrix[,2],A)
		featureMatrix[,ime] <- c(rep(NA, n-1),	moving.rank(A,n))
		}
	featureMatrix <- na.omit(featureMatrix)
	return(featureMatrix)
}