normalizacijaFM <- function(stock, ## OHLC delnice
							Features ## osnovna feature matrika
							){
				## noramlizira featurese Aroon, BBand, TDI kot v clanku:
				## Machine-learning classification techniques for the analysis and prediction of high-frequency stock direction

	# cene v obliki xts
	pxts <- xts(stock$close, stock$timestamp)

	NormFeatures <- Features


	normAroon <- function(
		# vrne 1 za nakup (Up > 100-e & Down < 0 + e), -1 za prodajo (Up < 0+e & Down > 1000 - e), 0 sicer
		aroonUp, # vrednosti Aroon Up indikatorja
		aroonDown, # vrednosti Aroon Down indikatorja
		e # dovoljeno odstopanje od meje 100 in 0
		){
		normInd <- rep(NA, length(aroonUp))
		normInd[aroonUp >= 100-e & aroonDown <= e] <- 1
		normInd[aroonUp <= e & aroonDown >= 100-e] <- -1
		normInd[!(aroonUp >= 100-e & aroonDown <= e)& !(aroonUp <= e & aroonDown >= 100-e)] <- 0
		return(normInd)
		
		}
		
		
	normBBand <- function(
		## vrne 1 za nakup (close < lower bound), -1 za prodajo (clsoe > upper bound), 0 sicer
		BBandUp, # vrednost BBand up
		BBandDown,	# vrednost BBand down
		closeP # close cena delnice
		){
		normInd <- rep(NA, length(BBandUp))
		normInd[closeP <=  BBandDown] <- 1
		normInd[closeP >= BBandUp] <- -1
		normInd[closeP > BBandDown & closeP < BBandUp] <- 0
		return(normInd)
		}

	####

	normTDI <- function(
		## vrne 1 za nakup (TDI in DI sta > 0 ), vrne -1 za prodajo (TDI in DI sta <0)
		TDI, # vrednost tdi
		DI # vrednost di
		){
		normInd <- rep(NA, length(TDI))
		normInd[TDI>0 & DI >0] <- 1
		normInd[TDI<0 & DI <0] <- -1
		normInd[TDI<=0 & DI >=0] <- 0
		normInd[TDI>=0 & DI <=0] <- 0
		return(normInd)
		}

		
		
	## Aroon inciator 
		# buy: Up close to 100 & Down close to 0
		# sell: Up close to 0 & Down close to 100
	n = c(5,10,20)



	for (i in n){
		imeUp <- paste0('AroonUp',toString(i))
		imeDn <- paste0('AroonDn',toString(i))
		imeNorm <- paste0('AroonNorm',toString(i))
		NormFeatures[,imeUp] <- NULL
		NormFeatures[,imeDn] <- NULL
		NormFeatures[,imeNorm] <- normAroon(Features[,imeUp],Features[,imeDn],2)
		
	}



	## Bollinger bands
		# buy: close < lower bound
		# sell: clsoe > upper bound

	dn <- list(c(2,5),c(2,10))

	for (i in dn){
		imeUp <- paste0('B_UpBand',toString(i))
		imeDn <- paste0('B_DnBand',toString(i))
		imeNorm <- paste0('BBand',toString(i))
		
		NormFeatures[,imeUp] <- NULL
		NormFeatures[,imeDn] <- NULL
		NormFeatures[,imeNorm] <- normBBand(Features[,imeUp], Features[,imeDn],pxts)
		
	}
		

		
		
	## Trend detection index
		## TDI > 0 in DI > 0: buy
		## TDI < 0 in DI < 0: sell
		
	n <- c(5,10,15)
	k <- c(1,2,3)
		
	for(j in k){
		for(i in n){
			ime1 <- paste0("TDI_",toString(i),"_",toString(j))
			imeNorm <- paste0("TDIinDI_",toString(i),"_",toString(j))
			ime2 <- paste0("DI_",toString(i),"_",toString(j))
			NormFeatures[,imeNorm]<- normTDI(Features[,ime1],Features[,ime2])
			NormFeatures[,ime2] <- NULL
		}
	}
		
	return(NormFeatures)

}

