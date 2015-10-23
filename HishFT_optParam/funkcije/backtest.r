backtest <- function(predDir, # predicted direction "probability"
					obsYield, # observed yield	(zamaknjen)
					upThrsh = 0.5, # threshold for predicting up
					dnThrsh = 0.5 # treshold for predicting down
					)
	## izracuna donose za napovedi predDir.
	{
	buySell <- rep(NA,length(predDir))
	## buy
	buySell[predDir >= upThrsh] <- 1
	## sell
	buySell[predDir <= dnThrsh] <- -1
	## do nothing 
	buySell[predDir < upThrsh & predDir > dnThrsh] <- 0
	#print(buySell)
	profit <- cumprod(1+buySell*obsYield)


	return(profit)
}