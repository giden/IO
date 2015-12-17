library(e1071)
library(class)
library(gmodels)
library("party")
normalize <- function(x) {
num <- x - min(x)
denom <- max(x) - min(x)
return (num/denom)
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
print(result)
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
print(result)
return(pos/(neg+pos))
}


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
TPR <- checkTPR(cbind.data.frame(diab_pred, diab.testLabels))
FPR <- checkFPR(cbind.data.frame(diab_pred, diab.testLabels))
plot(FPR, TPR, xlim=range(0:1), ylim=range(0:1), col='red')
print("kNN 3")
diab_pred <- knn(train=diab.training, test=diab.test, cl=diab.trainLabels, k=3)
TPR <- checkTPR(cbind.data.frame(diab_pred, diab.testLabels))
FPR <- checkFPR(cbind.data.frame(diab_pred, diab.testLabels))
points(FPR, TPR, col='blue')
print("kNN 7")
diab_pred <- knn(train=diab.training, test=diab.test, cl=diab.trainLabels, k=7)
TPR <- checkTPR(cbind.data.frame(diab_pred, diab.testLabels))
FPR <- checkFPR(cbind.data.frame(diab_pred, diab.testLabels))
points(FPR, TPR,col='purple')
print("naiveBayes")
m <- naiveBayes(diab.training, diab.trainLabels)
diab_pred <-predict(m, diab.test)
TPR <- checkTPR(cbind.data.frame(diab_pred, diab.testLabels))
FPR <- checkFPR(cbind.data.frame(diab_pred, diab.testLabels))
points(FPR, TPR,col='orange')
print("ctree")
tree__ <- ctree(diab.trainLabels ~ pregnant.times + glucose.concentr + blood.pressure + skin.thickness + insulin + mass.index + pedigree.func + age, data=cbind(diab.training,diab.trainLabels))
diab_pred <- predict(tree__, diab.test)
TPR <- checkTPR(cbind.data.frame(diab_pred, diab.testLabels))
FPR <- checkFPR(cbind.data.frame(diab_pred, diab.testLabels))
points(FPR, TPR, col='green')
names <- c('knn1','knn3','knn7','naiveBayes','party')
lege <- c('red','blue','purple','orange','green')
legend(x=0.75,y=1,legend=names	,pch=16,col=lege)

