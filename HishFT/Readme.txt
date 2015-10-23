hft(nastavi okolje, izberi parametre):
	1) nalozi podatke 
	2) loci podatke, pripravi feature matriko
	3) nauci modele za vsako delnico v Ticker
	4) preveri rezultate na validacijski mnozici
	(model1)
	5) izbere najboljse modele na validacijski 
	mnozici	in uporabi najboljse modele za napoved 
	na vseh delnicah (model2)
	6) naredi backtest za model1 in model2

	-> v mapi Podatki ustvari mape:
		donosi: donosi delnic
		featureMatrix: feature matrike delnic
		OHLC: OHLC podatki delnic
		podatkiLoc: podatki loceni na train, valid, test
		Close price: close price delnic
		Model: nauceni modeli
		Validacija: rezultati validacije modelov
		Backtest: rezultati backtesta

funkcije: 
	funkcije potrebne za delovanje skripte hft

Ticker:	
	Vsebuje tickerje delnic.

		
		

	
	
