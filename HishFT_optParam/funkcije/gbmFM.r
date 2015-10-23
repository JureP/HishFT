gbmFM <- function(
	## izracuna gbm na podatkih (train in valdiacijskih)
	xTrain,	## feature matrika za train del podatkov
	xValid, ## feature matrika za validation del podatkov 
	donosi, ## celotni donosi (funkcija izbere potrebne minute)
	n = 2,	## zamik napovedovanja
	stTree = 100, ## stevilo dreves, ki jih uporablja gbm
	skrcitev = 0.01, ## shrinkage, ki ga uporablja gbm
	intDep = 3, # interaction.depth, ki ga uporablja gbm
	node = 10, #n.minobsinnode, ki ga uporablja gbm
	zgKvan = 0.3, # koliksen delez najdonosenejsih minut vzamemo za ucenje
	spKvan = 0.3 # koliksen delez minut z najvecjo izgubo vzamemo za ucenje
	){
	
	# izbor odstopajocih (morda prirpavi vnaprej)
	odstTrain <- odstopajoci(xTrain,donosi, n = n, zgornjiKvantil = zgKvan, spodnjiKvantil = spKvan)
	#rm(xTrain)
	x1 <- odstTrain$xb
	y1 <- odstTrain$yb
	
	yValid <- zamik(xValid,donosi, n = n)
	
	x1 <- rbind(x1, xValid)
	delezVal <- length(y1)/(length(y1)+length(yValid)) ## delez za locitev train in validac mnozice (cela*delezVal = Train)
	y1 <- c(y1,yValid)
	
	x1 <- x1[,3:ncol(x1)]
	y1 <- as.vector(y1)

	y1[y1> 0] = 1
	y1[y1<= 0] = 0
	
	gbm1 <- gbm.fit(
		x1,
		y1,
		distribution = 'bernoulli', #  'adaboost'
		n.trees = stTree,
		shrinkage = skrcitev,
		interaction.depth = intDep, # 5
		n.minobsinnode = node,
		bag.fraction = 0.5,
		nTrain = round(nrow(x1)*delezVal) #,cv.folds = 4
	)
	
	return(gbm1)
	}
