library(TTR)
library(xts)
library(quantmod)


## ROC iz TTR 
	## ROC(price, n=1, type = 'discrete')
	## ROC(price, n=1, type = 'continitous') (log-returns)

SMApC <- function(
  #izracuna simple moving average % change (samo zadnji)
  close, # xts closing price
  n = 3 # st obdobji za izracun SMA
  ){
  sma <- mean(close[(length(close)-n+1):(length(close))])
  pC <- 100*(close[length(close)] - sma)/sma
  return(pC)
}



SMApCobdobje <- function(
  #izracuna simple moving average % change (vse, ki jih lahko)
  close, # xts closing price
  n = 3 # st obdobji za izracun SMA
){
  
  pC <- 100*(close - SMA(close,n))/SMA(close,n)
  return(pC)
}

EMApC <- function(
  #izracuna exponential moving average % change (samo zanji)
  close, # xts closing price
  n = 3 # st obdobji za izracun SMA
){
  pC <- 100*(close - EMA(close,n))/EMA(close,n)
  return(pC[length(pC)])
}


EMApCobdobje <- function(
  #izracuna simple moving average % change (vse, ki jih lahko)
  close, # xts closing price
  n = 3 # st obdobji za izracun SMA
){
  
  pC <- 100*(close - EMA(close,n))/EMA(close,n)
  return(pC)
}


## WMA za izracun uporablja vse pretekle podatke??
# kako deluje WMA
WMApCobdobje <- function(
  #izracuna exponential moving average volume weighted % change (samo zanji)
  close,# xts closing price
  volume,# xts volumen
  n = 3 # st obdobji za izracun SMA
){
  pC <- 100*(close - VWAP(close,volume, n))/VWAP(close,volume, n)
  return(pC)
}



regression <- function(
  # % razlika med regresijsko premico in zadnje cene
  closeP, # xts cen
  n = 10 #st obdboji za izracun regresijske premice
  ){
	podatki <- closeP[(length(closeP)-n+1):length(closeP)]
	y <- as.vector(podatki)
	rp <- lsfit(1:n,y)
	return(rp$res[n]/y[n])
}



## mnogo moznih majhnih (a ne neznemarljivih) variacij
MAVarianceRatio <- function(
	# izracuna moving average of variance ratio
	closeP, # xts cen
	n1, # za izracun prvega SMA
	n2 # za izracun drugega SMA
	){
	podatki1 <- closeP[(length(closeP)+1-n1):length(closeP)]
	#var(podatki1)
	#sum(((podatki1-mean(podatki1))^2))/(length(podatki1)-1)
	MAV1 <- mean((podatki1-mean(podatki1))^2)
	podatki2 <- closeP[(length(closeP)+1-n2):length(closeP)]
	MAV2 <- mean((podatki2-mean(podatki2))^2)
	
	
	MovAvgVar = MAV1/MAV2
	return(MovAvgVar)
	}

MAVarianceRatioObdobje <- function(
	# izracuna moving average of variance ratio
	closeP, # xts cen
	n1, # za izracun prvega SMA
	n2 # za izracun drugega SMA
	){
	
	## Poskusi z:
	## rollapply
	## runMean (TTR)
	podatki <- closeP
	for(i in 1:(max(n1,n2)-1)){
			podatki <- cbind(podatki, lag(closeP,i))
	}
	
	podatki1 <- podatki[,1:n1]
	matrika <- sweep(podatki1, 1, SMA(closeP,n1))^2
	vsota <- rowSums(matrika)
	MAVs1 <- vsota/n1
	
	podatki2 <- podatki[,1:n2]
	matrika <- sweep(podatki2, 1, SMA(closeP,n2))^2
	vsota2 <- rowSums(matrika)
	MAVs2 <- vsota2/n2
		
	MovAvgVar = MAVs1/MAVs2
	return(MovAvgVar)
	}
	
	
## RSI iz TTR
