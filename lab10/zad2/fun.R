fct.act <- function(x){
	return(1/(1+exp(-x)))
	}

fct.siatk <- function(wiek,waga,wzrost){
	return(fct.act(wiek*(-0.46122)+waga*0.97314+wzrost*(-0.39203)+0.80109)*(-0.81546)+fct.act(wiek*0.78548+waga*2.10584+wzrost*(-0.57847)+0.43529)*(1.03775)+(-0.2368))
}
