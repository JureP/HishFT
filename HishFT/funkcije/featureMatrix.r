featureMatrixPOPOLNA <- function(stockOHLC)
		
		{
		## sprejme OHLC in vrne osnovno feature matrix po clanki
		## Machine-learning classification techniques for the analysis and prediction of high-frequency stock direction
	featureMatrix <- stockOHLC[,1:2]

	# cene v obliki xts
	pxts <- xts(stockOHLC$close, stockOHLC$timestamp)
	# volume
	vol <- 	xts(stockOHLC$vol, stockOHLC$timestamp)
	# high, low , close
	HLC <- xts(cbind(stockOHLC$high,stockOHLC$low, stockOHLC$close), stockOHLC$timestamp)
	names(HLC) <- c('high', 'low', 'close')
	# high, low
	HL <- xts(cbind(stockOHLC$high,stockOHLC$low), stockOHLC$timestamp)
	names(HL) <- c('high', 'low')

	print('zacetek sestavljanja FM')


	## ROC n = (1,2,3,4,5,10,20)

	n = c(1,2,3,4,5,10,20)

	for(i in n){
		ime <- paste0('ROC',toString(i))
		featureMatrix[,ime] <- ROC(pxts,n =i , type = 'discrete')
	}



	## SMA % change n = (3,4,5,10,20)
	n = c(3,4,5,10,20)

	for(i in n){
		ime <- paste0('SMApC',toString(i))
		featureMatrix[,ime] <- SMApCobdobje(pxts,n =i)
	}

	## EMA % change n = (3,4,5,10,20)
	n = c(3,4,5,10,20)



	for(i in n){
		ime <- paste0('EMApC',toString(i))
		featureMatrix[,ime] <- EMApCobdobje(pxts,n =i)
	}


	## EMA volume weigthed % change n = (3,4,5,10,20)
	n = c(3,4,5,10,20)


	for(i in n){
		ime <- paste0('WMApC',toString(i))
		featureMatrix[,ime] <- WMApCobdobje(pxts,vol,n =i)
	}
		
	## Regression n = (2,5,10,20)
	## prepocasna 
	
	n = c(3,5,10,20)
	  


	for(i in n){
		reg <- rep(NA,i-1)
		for (j in 1:(length(pxts)-i+1)){
			podatki <- pxts[j:(j+i-1)]
			rg <- regression(podatki, n = i)
			reg <- c(reg, rg)
			if(j%%10000 == 0){print(c(i,j))}
		}
			
		ime <- paste0('Regression',toString(i))
		featureMatrix[,ime] <- reg
	}

	print('Konec izracuna regres featurja')

	## Moving average of variance ratio (n1 = 5, n2 = 10), (n1 = 5, n2 = 20)
	n = list(c(5,10),c(5,20))


	for(i in n){
		ime <- paste0('MAvarRatio',toString(i))
		featureMatrix[,ime] <- MAVarianceRatioObdobje(pxts, i[1],i[2])
	}


	## RSI n = (5,10,20)
	n = c(5,10,20)

	for (i in n){
		ime <- paste0('RSI',toString(i))
		featureMatrix[,ime] <- RSI(pxts, i)
	}


	## Chande momentum oscillator n = (5,10,20)
	n = c(5,10,20)

	for (i in n){
		ime <- paste0('CMO',toString(i))
		featureMatrix[,ime] <- CMO(pxts, i)
	}


	## Aroon indicator n = (5,10,20)
	n = c(5,10,20)



	for (i in n){
		imeUp <- paste0('AroonUp',toString(i))
		imeDn <- paste0('AroonDn',toString(i))
		Ar <- aroon(HL, i)
		featureMatrix[,imeUp] <- Ar$aroonUp
		featureMatrix[,imeDn] <- Ar$aroonDn
		## brez oscillator
	}


	## Bollinger Bands (d,n) = ((2,5),(2,10)) 
	dn <- list(c(2,5),c(2,10))

	for (i in dn){
		imeUp <- paste0('B_UpBand',toString(i))
		imeDn <- paste0('B_DnBand',toString(i))
		Bollinger <- BBands(HLC, n = i[2], sd = i[1])
		featureMatrix[,imeUp] <- Bollinger$up
		featureMatrix[,imeDn] <- Bollinger$dn
	}

	
	## Commodity channel index n = (5,10,20)
	n = c(5,10,20)

	for (i in n){
		ime <- paste0('CCI',toString(i))
		featureMatrix[,ime] <- CCI(HLC, n = i)
	}



	## Chaikin volatility (s,n) = ((5,5),(10,10),(20,20))
	n <- c(5,10,20)

	for (i in n){
		ime <- paste0('ChaikinVolatility',toString(i))
		featureMatrix[,ime] <- chaikinVolatility(HL, n=i)
	}

	## Chaikin money flow
	n = c(5,10,20)

	for(i in n){
	  ime <- paste0("chaMF_",toString(i))
	  featureMatrix[,ime] <- CMF(stockOHLC[,c('high','low','close')], stockOHLC[,'volume'], n = i)
	}


	## Chaikin Accumulation/Distribution

	featureMatrix[,"chaAD"] <- chaikinAD(HLC, vol)

	## Close location value 
	featureMatrix[,"CLV"] <- CLV(HLC)


	## Moving average convergence divergence oscillator (MACD)
	n <- list(c(12,26,9), c(6,13,4))

	for(i in n){
	  ime <- paste0("diffMACDsignal_",toString(i))
	  A <- MACD(pxts, i[1],i[2],i[3])
	  featureMatrix[,ime] <- A[,1] - A[,2]
	}
	rm(A)

	## Money flow index

	n <- c(5,10,15)

	for(i in n){
	  ime <- paste0("MFI_",toString(i))
	  featureMatrix[,ime] <- MFI(pxts, vol, n = i)
	}


	## Trend detection index

	n <- c(5,10,15)
	k <- c(1,2,3)

	for(j in k){
	  for(i in n){
		ime1 <- paste0("TDI_",toString(i),"_",toString(j))
		ime2 <- paste0("DI_",toString(i),"_",toString(j))
		featureMatrix[,ime1] <- TDI(pxts, n = i, multiple = j)$tdi
		featureMatrix[,ime2] <- TDI(pxts, n = i, multiple = j)$di
	  }
	}


	##Williams %R Relative strength index
	n <- c(5,10,20)

	for(i in n){
	  ime <- paste0("WRSI_",toString(i))
	  featureMatrix[,ime] <- WPR(HLC, n = i)
	}


	## Stochastic momentum oscillator

	featureMatrix[,"StochMomOsc"] <- stoch(HLC)$fastD

	## Corr
	## HighLow

	n <- c(5,10,20)

	for(i in n){
		ime <- paste0("CorrHighLow_",toString(i))
		high <- HL$high
		low <- HL$low
		featureMatrix[,ime] <- runCor(low, high,i)
	}



	## Close
	l <- c(1,2,3)
	n <- c(5,10,20)

	for(k in l){
	  for(i in n){
		ime <- paste0("CorrClose_",toString(i),"_",toString(k))
		featureMatrix[,ime] <- runCor(pxts, lag(pxts, k),i)
	  }
	}
	return(featureMatrix)
}

