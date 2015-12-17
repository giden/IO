library(neuralnet)
siatk.dane = read.csv("dat.csv")
siatk.nn <- neuralnet(gra~wiek+waga+wzrost, siatk.dane, hidden=2, lifesign="full")
plot(siatk.nn)
siatk.predict <- compute(siatk.nn, siatk.dane[1:3])
siatk.nn <- neuralnet(gra~wiek+waga+wzrost, siatk.dane, hidden=2, lifesign="full")
source("fun.R")
fck.siatk(23,75,176)
fct.siatk(48,97,178)

library(neuralnet)
normalize <- function(x) {
num <- x - min(x)
denom <- max(x) - min(x)
return (num/denom)
}
iris <- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"), header = FALSE) 
names(iris) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
iris_norm <- as.data.frame(lapply(iris[1:4], normalize))
set.seed(1234)
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))
iris.training <- iris_norm[ind==1, 1:4]
iris.test <- iris_norm[ind==2, 1:4]
iris.trainLabels <- iris[ind==1, 5]
iris.testLabels <- iris[ind==2, 5]
iris.training$setosa <- c(iris.trainLabels =='Iris-setosa')
iris.training$versicolor <- c(iris.trainLabels =='Iris-versicolor')
iris.training$virginica <- c(iris.trainLabels =='Iris-virginica')
inet <- neuralnet(setosa + versicolor + virginica ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, iris.training, hidden=3, lifesign='full')
plot(inet)
predict <- compute(inet, iris.test)
result<-0
for (i in 1:38) { result[i] <- which.max(predict$net.result[i,]) }
for (i in 1:38) { if (result[i]==3) {result[i] = 'Iris-virginica'} }
for (i in 1:38) { if (result[i]==2) {result[i] = 'Iris-versicolor'} }
for (i in 1:38) { if (result[i]==1) {result[i] = 'Iris-setosa'} }
comparison <- iris.test
comparison$Actual <- iris.testLabels
comparison$Predicted <- result
comparison
source("check.R")
check(comparison)

q()
