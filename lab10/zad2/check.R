check<- function(tab){
counter <- 0
rowNbr <- nrow(tab)
for(i in 1:rowNbr){
test <- tab[i,5]
predicted <- tab[i,6]
if(test == predicted){
counter <- counter + 1
}
}
percent <- (counter/rowNbr)*100
result <- data.frame(equal=counter, percent=percent, max=rowNbr)
return(result)
}

