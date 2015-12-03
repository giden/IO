normalize <- function(x) {
num <- x - min(x)
denom <- max(x) - min(x)
return (num/denom)
}

check<- function(tab){
counter <- 0
rowNbr <- nrow(tab)
for(i in 1:rowNbr){
testName <- tab[i,1]
realName <- tab[i,2]
if(testName == realName){
counter <- counter + 1
}
}
percent <- (counter/rowNbr)*100
result <- data.frame(total=counter, percent=percent, max=rowNbr)
return(result)
}

checkTPR <- function(tab){
pos <- 0
neg <- 0
rowNbr <- nrow(tab)
for(i in 1:rowNbr){
predictedName <- tab[i,1]
testName <- tab[i,2]
if(testName=='tested_positive'){
if(predictedName == 'tested_positive'){
pos <- pos + 1
}
else{ neg <- neg +1 }
}}
percent <- (pos/(neg+pos))*100
result <- data.frame(TPR=pos/(neg+pos), percent=percent, total=neg+pos, trueP=pos, falseN=neg)
return(pos/(neg+pos))
}

checkFPR <- function(tab){
pos <- 0
neg <- 0
rowNbr <- nrow(tab)
for(i in 1:rowNbr){
predictedName <- tab[i,1]
testName <- tab[i,2]
if(testName=='tested_negative'){
if(predictedName == 'tested_positive'){
pos <- pos + 1
}
else{ neg <- neg +1 }
}
}
percent <- (pos/(neg+pos))*100
result <- data.frame(FPR=pos/(neg+pos), percent=percent, total=neg+pos, falseP=pos, trueN=neg)
return(pos/(neg+pos))
}

library(class)
library(gmodels)
set.seed(1234)
diab <- read.csv("diabetes.csv", header=TRUE)
diab_norm <- as.data.frame(lapply(diab[1:8], normalize))
ind <- sample(2, nrow(diab_norm), replace=TRUE, prob=c(0.7, 0.3))
diab.training <- diab_norm[ind==1, 1:8]
diab.test <- diab_norm[ind==2, 1:8]
diab.trainLabels <- diab[ind==1, 9]
diab.testLabels <- diab[ind==2, 9]

print("kNN 1")
diab_pred <- knn(train=diab.training, test=diab.test, cl=diab.trainLabels, k=1)
print(check(cbind.data.frame(diab_pred, diab.testLabels)))
TPR <- checkTPR(cbind.data.frame(diab_pred, diab.testLabels))
print(TPR)
FPR <- checkFPR(cbind.data.frame(diab_pred, diab.testLabels))
print(FPR)
plot(FPR, TPR, xlim=range(0:1), ylim=range(0:1), col='red')
CrossTable(x=diab.testLabels, y=diab_pred, prop.chisq=FALSE)
print("kNN 3")
diab_pred <- knn(train=diab.training, test=diab.test, cl=diab.trainLabels, k=3)
print(check(cbind.data.frame(diab_pred, diab.testLabels)))
TPR <- checkTPR(cbind.data.frame(diab_pred, diab.testLabels))
print(TPR)
FPR <- checkFPR(cbind.data.frame(diab_pred, diab.testLabels))
print(FPR)
points(FPR, TPR, col='blue')
CrossTable(x=diab.testLabels, y=diab_pred, prop.chisq=FALSE)
print("kNN 7")
diab_pred <- knn(train=diab.training, test=diab.test, cl=diab.trainLabels, k=7)
print(check(cbind.data.frame(diab_pred, diab.testLabels)))
TPR <- checkTPR(cbind.data.frame(diab_pred, diab.testLabels))
print(TPR)
FPR <- checkFPR(cbind.data.frame(diab_pred, diab.testLabels))
print(FPR)
points(FPR, TPR,col='purple')
CrossTable(x=diab.testLabels, y=diab_pred, prop.chisq=FALSE)
print("naiveBayes")
library(e1071)
m <- naiveBayes(diab.training, diab.trainLabels)
diab_pred <-predict(m, diab.test)
print(check(cbind.data.frame(diab_pred, diab.testLabels)))
TPR <- checkTPR(cbind.data.frame(diab_pred, diab.testLabels))
print(TPR)
FPR <- checkFPR(cbind.data.frame(diab_pred, diab.testLabels))
print(FPR)
points(FPR, TPR,col='orange')
CrossTable(y=predict(m, diab.test), x=diab.testLabels, prop.chisq=FALSE)
print("ctree")
 library("party")
tree__ <- ctree(diab.trainLabels ~ pregnant.times + glucose.concentr + blood.pressure + skin.thickness + insulin + mass.index + pedigree.func + age, data=cbind(diab.training,diab.trainLabels))
diab_pred <- predict(tree__, diab.test)
print(check(cbind.data.frame(diab_pred, diab.testLabels)))
TPR <- checkTPR(cbind.data.frame(diab_pred, diab.testLabels))
print(TPR)
FPR <- checkFPR(cbind.data.frame(diab_pred, diab.testLabels))
print(FPR)
points(FPR, TPR, col='green')
names <- c('knn1','knn3','knn7','naiveBayes','party')
lege <- c('red','blue','purple','orange','green')
legend(x=0.75,y=1,legend=names	,pch=16,col=lege)
CrossTable(y=predict(tree__, newdata=diab.test), x=diab.testLabels, prop.chisq=FALSE)
