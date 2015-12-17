source("fun.r")

library(neuralnet)
diab.training$positive <- c(diab.trainLabels=='tested_positive')
diab.training$negative <- c(diab.trainLabels=='tested_negative')
dnet <- neuralnet(positive+negative ~ pregnant.times+glucose.concentr+blood.pressure+skin.thickness+insulin+mass.index+pedigree.func+age,diab.training,hidden=2,lifesign='full')

predict <- compute(dnet, diab.test)
result <-0
for (i in 1:234) { result[i] <- which.max(predict$net.result[i,]) }
for (i in 1:234) { if (result[i]==1) {result[i] = 'tested_positive'}
}
for (i in 1:234) { if (result[i]==2) {result[i] = 'tested_negative'}
}
comp <- diab.test
comp$Actual <- diab.testLabels
comp$Predicted <- result
comp
source("check.R")
check(comp)

TPR <- checkTPR(cbind.data.frame(comp$Predicted, comp$Actual))

FPR <- checkFPR(cbind.data.frame(comp$Predicted, comp$Actual))

points(FPR, TPR, col='brown')
plot(dnet)

