## Author: Jure Podlogar
library(TTR)
library(xts)
library(quantmod)
library(fts)
library(gbm)
library(SDMTools)

#############################################################################################
#############################################################################################
## IZBIRA PARAMETROV
Okolje <- 'C:/Users/Podlogar/Documents/HFT/HishFT' ## okolje kjer je Ticker, mapa 
													## funkcije in kjer se ustvarijo
													## nadaljna okolja
Okolje1 <- paste0(Okolje, '/Podatki') ## mapa v katero naj shrani podateke
OkoljeFun <- paste0(Okolje, '/funkcije') ## okolje kjer so funkcije
dir.create(Okolje1, showWarnings = TRUE, recursive = FALSE, mode = "0777")
## razdelitev podatkov
	zacetek = as.Date('2011-09-22') # zacetek train mnozice
	konecTrain = as.Date('2014-08-13') # konec train, zacetek validation mnozice
	konecValid = as.Date('2015-01-02') # konec validation mnozice, zacetek test mnozice
	konec = as.Date('2015-05-15') # konec test mnozice
## izbira parametrov za ucenje
	stDreves = 200 ## stevilo dreves pri ucenju
	##stDreves = 20 ## stevilo dreves pri ucenju
	shrink = 0.01 ## shrinkage, ki ga uporablja gbm
	int.depth = 2 # interaction.depth, ki ga uporablja gbm
	min.node = 20 #n.minobsinnode, ki ga uporablja gbm
	zgDel = 0.05 # koliksen delez najdonosenejsih minut vzamemo za ucenje
	spDel = 0.05 # koliksen delez minut z najvecjo izgubo vzamemo za ucenje
## izbira za validacijo 
	mejaValid = 5*10^(-5) ## meja od katere morajo biti podatki v validaciji 
							## vecji () po abs da se jih uporabi v validaciji
## backtest:
	delezNapovedi = 0.2 ## delez napovedi, ki naj jih uporablja trgovalna strategija
						## (delezNapovedi/2 najvecjih in delezNapovedi/2 najmanjsih)

#############################################################################################
#############################################################################################

## imena okolji
	OkoljeOHLC <- paste0(Okolje1, '/OHLC') ## mapa (ki jo ustvari skripta) kamor se shrani OHLC
	OkoljeFM <- paste0(Okolje1, '/featureMatrix') ## mapa (ki jo ustvari skripta) kamor se shrani FM
	OkoljeClose <- paste0(Okolje1, '/Close price') ## mapa (ki jo ustvari skripta) kamor se shrani close price
	OkoljeROC <- paste0(Okolje1, '/donosi') ## mapa (ki jo ustvari skripta) kamor se shrani donosi
	OkoljeLoc <- paste0(Okolje1, '/podatkiLoc') ## mapa (ki jo ustvari skripta) kamor se shranijo loceni podatki
	OkoljeModel <- paste0(Okolje1, '/Model') ## mapa (ki jo ustvari skripta) kamor se shraniji naucen model
	OkoljeValid <- paste0(Okolje1, '/Validacija') ## mapa (ki jo ustvari skripta) kamor se shranijo rezultati na
													## validacijski mnozici
	OkoljeBT <- paste0(Okolje1, '/Backtest') ## mapa (ki jo ustvari skripta) kamor se shranijo rezultati backtesta

setwd(Okolje)

## imena delnic
tick <- read.delim("Ticker.txt", header=FALSE, quote="")
tick <- toString(tick[,3])
tick <- strsplit(tick, ', ')[[1]] 

#tick <- tick[1:3]


## loadanje funkcij
setwd(OkoljeFun)
funkcije <- list.files(pattern = "\\.r$")
for (i in funkcije){
	source(i)
}

setwd(Okolje1)

############################################################################################
##PRIPRAVA PODATKOV#########################################################################
############################################################################################

## loadanje podatkov 
	## ustveri mapo OHLC
	dir.create(OkoljeOHLC, showWarnings = TRUE, recursive = FALSE, mode = "0777")
	setwd(OkoljeOHLC)
	## shrani OHLC csv v mapo OHLC
	load_csv(tick)
## Izpise tabelo kvalitete podatkov
	#View(kvalitetaPodatkov(tick, OkoljeOHLC))

## Feature matrix, donosi in close price 
	## okolje kamor se shrani feature matrika
	dir.create(OkoljeFM, showWarnings = TRUE, recursive = FALSE, mode = "0777")
	## okolje kamor se shranijo donosi
	dir.create(OkoljeROC, showWarnings = TRUE, recursive = FALSE, mode = "0777")
	## okolje kamor se shranijo loceni podatki
	dir.create(OkoljeLoc, showWarnings = TRUE, recursive = FALSE, mode = "0777")
	for (i in tick){
		print(paste0('Priprava podatkov ', i))
		## sestavi feature matriko (z rangiranjem in brez normalizacije) za ticker i in jo shrani v mapo featureMatrix
		## featurji po clanku: Machine-learning classification techniques for the analysis and prediction of high-frequency stock direction 
		imeFM <- paste0(i,'fm.rds')
		imeROC <- paste0(i,'_donosi.rds')
		imeLoc <- paste0(i,'_loceni.rds')
		fm_donos <- sestaviFeatureMatrix(ticker = i, okoljeOHLC = OkoljeOHLC, normalizacija = FALSE, rank = TRUE,
										zacetek = zacetek, konecTrain = konecTrain, konecValid = konecValid, 
										konec = konec)
		## shranjevanje feature matrike
		setwd(OkoljeFM)
		saveRDS(fm_donos$fm, imeFM)
		## shranjevanje donosov
		setwd(OkoljeROC)
		saveRDS(fm_donos$donosi, imeROC)
		## shranjevanje locenih podatkov
		setwd(OkoljeLoc)
		saveRDS(fm_donos$podLoceni, imeLoc)
		rm(fm_donos)
		gc()
	}

	## okolje kamor se shranijo close price
	dir.create(OkoljeClose, showWarnings = TRUE, recursive = FALSE, mode = "0777")
	st = 0
	print('shranjevanje close price za potrebe backtesta')
	for (i in tick){
		setwd(OkoljeOHLC)
		ime <- paste0(i, '.csv')
		asset <- read.csv(ime, sep=";", dec=",")
		imeClose <- paste0(i,'closePrice.rds')
		setwd(OkoljeClose)
		## shranjevanje samo close price
		saveRDS(asset[, 'close'], imeClose)
		st <- st + 1 
		print(paste0('Potek: ',round(100*st/length(tick),2),'%'))
	}

	

##UCENJE####################################################################################
############################################################################################

	## okolje kamor se shranijo nauceni modeli
	dir.create(OkoljeModel, showWarnings = TRUE, recursive = FALSE, mode = "0777")


	for (i in tick){
		print(paste0('ucenje ', i))
		imeROC <- paste0(i,'_donosi.rds')
		imeLoc <- paste0(i,'_loceni.rds')
		setwd(OkoljeLoc)
		## loceni podatki
		stock <- readRDS(imeLoc)
		setwd(OkoljeROC)
		## donosi
		donosi <- readRDS(imeROC)
		## train del podatkov
		xTrain <- stock$xTrain
		xValid <- stock$xValid
		rm(stock)
		## ucenje modela
		gbm1 <- gbmFM(	xTrain,	## feature matrika za train del podatkov
						xValid, ## feature matrika za validation del podatkov 
						donosi, ## celotni donosi (funkcija izbere potrebne minute)
						n = 2,	## zamik napovedovanja
						stTree = stDreves, skrcitev = shrink, intDep = int.depth, 
						node = min.node, zgKvan = zgDel, spKvan = spDel)
		imeGBM <- paste0(i, 'gbm',stDreves ,'.rds')
		setwd(OkoljeModel)
		saveRDS(gbm1, imeGBM)
		rm(gbm1)
	}


##VALIDACIJA################################################################################
############################################################################################

dir.create(OkoljeValid, showWarnings = TRUE, recursive = FALSE, mode = "0777")


## model1: Uporaba modela za napovedovanje na delnici na kateri je bil model naucen

	## delezi napovedi ki naj bodo prevejeni
	deleziNap <- c(1,0.5, 0.20, 0.1)
	## kamor se shranijo rezultati (sensitivity, specificity, prop. correct)
	rezValid <- array(NA, c(length(deleziNap),3, length(tick)))

	## imena vrstic, stolpcev,...
	delNapovedi <- NULL
	for(d in deleziNap){
		delNapovedi <- c(delNapovedi, paste0(d*100, '% napovedi, mediana'))
	}
	delNapovedi <- c('Vsi, mediana', '50%, mediana', '20%, mediana', '10%, mediana')
	dimnames(rezValid) <- list(delNapovedi, names(accuracy(0,0,0))[4:6], tick)


	for (i in tick){
		print(paste0('validacija ', i))
		## model za napoved
		setwd(OkoljeModel)
		imeGBM <- paste0(i, 'gbm',stDreves ,'.rds')
		model <- readRDS(imeGBM)
		
		## podatki za vlidacijo
		setwd(OkoljeLoc)
		imeLoc <- paste0(i,'_loceni.rds')
		stock <- readRDS(imeLoc)
		xValid <- stock$xValid
		yValid <- stock$yValid
		rm(stock)
		
		## izbor samo vecjih sprememb cene
		xValid <- xValid[abs(yValid) >= mejaValid,]
		print(paste0('delez podatkov po abs vecjih od ', mejaValid,
						' pri ', i, ': ', round(nrow(xValid)/length(yValid),3)))
		yValid <- yValid[abs(yValid) >= mejaValid,]
		yValid[yValid <= -mejaValid] <- 0
		yValid[yValid >= mejaValid] <- 1
		
		## napovedi modela
		napoved <- predict(model,xValid, type = 'response')
		
		
		## rezultati modela
		count <- 0
		for(d in deleziNap){
			count <- count + 1
			## q1 in q2: kvantila od katerih napovemo gor/dol		
			q1 <- quantile(napoved, d/2)
			q2 <- quantile(napoved, 1 - d/2)
			## del napovedi ki jih pregledamo v testu (glede na q1, q2)
			odstNap <- napoved[napoved >= q2 | napoved <= q1]
			yOdst <- yValid[napoved >= q2 | napoved <= q1]
			rezValid[paste(delNapovedi[count]),,paste(i)] <- as.double(accuracy(as.vector(yOdst),
													odstNap, threshold = median(napoved)))[4:6]
		}
	}


	## shranitev arraya rezultatov model1
	setwd(OkoljeValid)
	saveRDS(rezValid, 'rezultatiValidacija_model1.rds')

	rm(delNapovedi)

## Izbor najboljsih modelov 
	setwd(OkoljeModel)

	tickB <- NULL
	for(i in tick){
		## kriterij izbire modela
		if(rezValid[4,1,i]> 0.5 & rezValid[4,2,i] > 0.5 & rezValid[4,3,i] > 0.51) {tickB <- c(tickB,i)}
	}
	
	if (length(tickB) == 0){
		print('Noben model ne ustreza pogojem za vkljucitev v model2')
	}

	## shranitev arraya rezultatov model1
	setwd(OkoljeValid)
	saveRDS(tickB, 'izbraniModeli_zaModel2.rds')


## model2: Uporaba povprecja napovedi najboljsih modelov na vseh delnicah.
	
	## najboljsi modeli, ki so uporabljenbi v model2
	setwd(OkoljeValid)
	tickB <- readRDS('izbraniModeli_zaModel2.rds')
	## delezi napovedi ki naj bodo prevejeni
	deleziNap <- c(1,0.5, 0.20, 0.1)
	## kamor se shranijo rezultati (sensitivity, specificity, prop. correct)
	rezValidG <- array(NA, c(length(deleziNap),3, length(tick)))

	## imena vrstic, stolpcev,...
	delNapovedi <- NULL
	for(d in deleziNap){
		delNapovedi <- c(delNapovedi, paste0(d*100, '% napovedi, mediana'))
	}
	delNapovedi <- c('Vsi, mediana', '50%, mediana', '20%, mediana', '10%, mediana')
	dimnames(rezValidG) <- list(delNapovedi, names(accuracy(0,0,0))[4:6], tick)

	for (i in tick){
		print(paste0('validacija ', i, ' z uporabo modelov ', paste0(tickB, collapse = ', ')))

		setwd(OkoljeLoc)
		imeLoc <- paste0(i,'_loceni.rds')
		stock <- readRDS(imeLoc)
		xValid <- stock$xValid
		yValid <- stock$yValid
		rm(stock)
		
		## izbor samo vecjih sprememb cene
		xValid <- xValid[abs(yValid) >= mejaValid,]
		print(paste0('delez podatkov po abs vecjih od ', mejaValid,
						' pri ', i, ': ', round(nrow(xValid)/length(yValid),3)))
		yValid <- yValid[abs(yValid) >= mejaValid,]
		yValid[yValid <= -mejaValid] <- 0
		yValid[yValid >= mejaValid] <- 1
		
		## napovedi modela
		setwd(OkoljeModel)
		napovedi <- matrix(NA, length(yValid), length(tickB))
		j <- 1
		for (tb in tickB){
			imeGBM <- paste0(tb, 'gbm',stDreves ,'.rds')
			model <- readRDS(imeGBM)
			
			napovedi[,j] <- predict(model,xValid, type = 'response')
			j <- j+1
		}
		## povprecje napovedi vecih modelov se uporabi kot napoved
		napoved <- rowMeans(napovedi)
		
		## rezultati modela (na razlicnih delih napovedi )
		count <- 0
		for(d in deleziNap){
			count <- count + 1
			## q1 in q2: kvantila od katerih napovemo gor/dol
			q1 <- quantile(napoved, d/2)
			q2 <- quantile(napoved, 1 - d/2)
			## del napovedi ki jih pregledamo v testu (glede na q1, q2)
			odstNap <- napoved[napoved >= q2 | napoved <= q1]
			yOdst <- yValid[napoved >= q2 | napoved <= q1]
			rezValidG[paste(delNapovedi[count]),,paste(i)] <- as.double(accuracy(as.vector(yOdst),
													odstNap, threshold = median(napoved)))[4:6]
		}
	}

	## shranitev arraya rezultatov model1
	setwd(OkoljeValid)
	saveRDS(rezValidG, 'rezultatiValidacija_model2.rds')

	rm(delNapovedi)

	
## primerjava rezultatov med model1 in model2
	setwd(OkoljeValid)
	rezValidG <- readRDS('rezultatiValidacija_model2.rds') 
	rezValid <- readRDS('rezultatiValidacija_model1.rds')
	
	razlike <- rezValidG - rezValid
	#plot(razlike[4,3,])
	print(paste0('Povprecno izobljsanje z uporabo model2: ', mean(razlike[4,3,]), ' (>0 bolje, <0 slabse)'))

	paste(paste0('pravilnost napovedi model1: prvi kvanitl ', round(quantile(rezValid[4,3,], 0.25),4)))
	paste(paste0('pravilnost napovedi model2: prvi kvanitl ', round(quantile(rezValidG[4,3,], 0.25),4)))

	paste(paste0('pravilnost napovedi model1: tretji kvanitl ', round(quantile(rezValid[4,3,], 0.75),4)))
	paste(paste0('pravilnost napovedi model2: tretji kvanitl ', round(quantile(rezValidG[4,3,], 0.75),4)))

	paste(paste0('pravilnost napovedi model1: tretji kvanitl ', round(quantile(rezValid[4,3,], 0.85),4)))
	paste(paste0('pravilnost napovedi model2: tretji kvanitl ', round(quantile(rezValidG[4,3,], 0.85),4)))


##BACKTEST##################################################################################
############################################################################################

dir.create(OkoljeBT, showWarnings = TRUE, recursive = FALSE, mode = "0777")

## strategija: kupi ko napoved med 10% zgornjih napovedi (glede na napovedi na valid)
## 				prodaj ko napoved med 10% spodnjih napovedi (glede na napovedi an valid)
## benchmark: buy and hold 

## backtest model1 
rezultatiBT_model1 <- list()

	for (i in tick){
		print(paste0('validacija ', i))
		setwd(OkoljeModel)
		imeGBM <- paste0(i, 'gbm',stDreves ,'.rds')
		model <- readRDS(imeGBM)
		
		setwd(OkoljeLoc)
		imeLoc <- paste0(i,'_loceni.rds')
		stock <- readRDS(imeLoc)
		xValid <- stock$xValid
		yValid <- stock$yValid
		xTest <- stock$xTest
		yTest <- stock$yTest
		rm(stock)
			
		## napovedi modela na vali (za potrebe dolocanja kvantilov)
		napoved <- predict(model,xValid, type = 'response')
		
		## meja nad katero kupimo
		qU <- quantile(napoved, 1 - delezNapovedi/2)
		## meja pod katero prodamo
		qD <- quantile(napoved, delezNapovedi/2)
		
		napoved <- predict(model,xTest, type = 'response')
		
		## rezultati strategije
		valueP1 <- backtest(napoved, yTest, upThrsh = qU, dnThrsh = qD)
		## rezultati benchmark
		bnmrk <- backtest(napoved, yTest, 0, 0)
		
		strat_bnmrk <- cbind(valueP1, bnmrk)
		names(strat_bnmrk) <- c('rezultati strategije', 'benchmark')
		rezultatiBT_model1[[i]] <- strat_bnmrk
		
	}
	
	
	setwd(OkoljeBT)
	saveRDS(rezultatiBT_model1, 'backtestModel1_list.rds')
	
	## prikaz grafov
	setwd(OkoljeBT)
	backTest1 <- readRDS('backtestModel1_list.rds')
	for(i in tick){
		A <- backTest1[[i]]
		yMin <- min(A[ , 1], A[ , 2])
		yMax <- max(A[ , 1], A[ , 2])
		plot(A[,1], ylim = c(yMin, yMax), main = i)
		legend('topleft', c("strategija", "benchmark"), col = c(1,2),
				lty = c(1, 1), merge = TRUE, bty = 'n')
		lines(A[,2], col = 2)
		Sys.sleep(5)
	}

## backtest model2 

	## load tickB
	setwd(OkoljeValid)
	tickB <- readRDS('izbraniModeli_zaModel2.rds')


	rezultatiBT_model2 <- list()
	
	for (i in tick){
		print(paste0('validacija ', i))
		
		setwd(OkoljeLoc)
		imeLoc <- paste0(i,'_loceni.rds')
		stock <- readRDS(imeLoc)
		xValid <- stock$xValid
		yValid <- stock$yValid
		xTest <- stock$xTest
		yTest <- stock$yTest
		rm(stock)
			
		## napovedi modela na vali (za potrebe dolocanja kvantilov)
		setwd(OkoljeModel)
		napovedi <- matrix(NA, length(yValid), length(tickB))
		j <- 1
		for (tb in tickB){
			imeGBM <- paste0(tb, 'gbm',stDreves ,'.rds')
			model <- readRDS(imeGBM)
			
			napovedi[,j] <- predict(model,xValid, type = 'response')
			j <- j+1
		}
		napoved <- rowMeans(napovedi)
		
		## meja nad katero kupimo
		qU <- quantile(napoved, 1 - delezNapovedi/2)
		## meja pod katero prodamo
		qD <- quantile(napoved, delezNapovedi/2)
		
		## napovedi modela na testni mnozici
		setwd(OkoljeModel)
		napovedi <- matrix(NA, length(yTest), length(tickB))
		j <- 1
		for (tb in tickB){
			imeGBM <- paste0(tb, 'gbm',stDreves ,'.rds')
			model <- readRDS(imeGBM)
			
			napovedi[,j] <- predict(model, xTest, type = 'response')
			j <- j+1
		}
		napoved <- rowMeans(napovedi)
		
		## rezultati strategije
		valueP <- backtest(napoved, yTest, upThrsh = qU, dnThrsh = qD)
		## rezultati benchmarka
		bnmrk <- backtest(napoved, yTest, 0, 0)
		
		strat_bnmrk <- cbind(valueP, bnmrk)
		names(strat_bnmrk) <- c('rezultati strategije', 'benchmark')
		rezultatiBT_model2[[i]] <- strat_bnmrk
		
	}
	
	
	## shranjevanje rezultata
	setwd(OkoljeBT)
	saveRDS(rezultatiBT_model2, 'backtestModel2_list.rds')
	
	## prikaz grafov rezultatov
	setwd(OkoljeBT)
	backTest2 <- readRDS('backtestModel2_list.rds')
	for(i in tick){
		A <- backTest2[[i]]
		yMin <- min(A[ , 1], A[ , 2])
		yMax <- max(A[ , 1], A[ , 2])
		plot(A[,1], ylim = c(yMin, yMax), main = i)
		legend('topleft', c("strategija", "benchmark"), col = c(1,2),
				lty = c(1, 1), merge = TRUE, bty = 'n')
		lines(A[,2], col = 2)
		Sys.sleep(2)
	}

	
## primerjava rezultatov obeh strategij


	setwd(OkoljeBT)
	backTest1 <- readRDS('backtestModel1_list.rds')
	backTest2 <- readRDS('backtestModel2_list.rds')
	koncneVrednosti <- data.frame(matrix(NA, length(tick), 3))
	rownames(koncneVrednosti) <- tick
	colnames(koncneVrednosti) <- c('model1', 'model2', 'benchmark')
	for(i in tick){
		A <- tail(backTest1[[i]],1)
		B <- tail(backTest2[[i]],1)
		koncneVrednosti[i,] <- 	cbind(A[,1],B)
		
	}
	
	print('Povprecni donosi razlicnih strategij na vseh delicah:')
	print(colMeans(koncneVrednosti))
	
	
	
	

	
