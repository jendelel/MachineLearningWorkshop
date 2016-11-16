results <- function(confTable) {
  acc = sum(diag(confTable)) / sum(confTable)
  TP = confTable[2,2]
  TN = confTable[1,1]
  FP = confTable[2,1]
  FN = confTable[1,2]
  
  P = TP / (TP+FP)
  R = TP / (TP+FN)
  F = 2 *(P*R)/(P+R)
  return(c(acc, P, R, F))
}
results(matrix(c(2703,180,22,6), 2))

library(ISLR)
str(Caravan)

set.seed(1234)
Purchase = Caravan$Purchase
data = data.frame(scale(Caravan[, -86]), Purchase)
train = data[sample(row.names(data), size=round(nrow(data) * 0.5)),]
test = data[!(row.names(data) %in% row.names(train)), ]

# Evaluate MFC on test data
table((factor(rep(0, nrow(test)), 0:1)), test$Purchase)

# Accuracy of MFC on test data
results(table((factor(rep(0, nrow(test)), 0:1)), test$Purchase))

m.glm = glm(Purchase ~ ., data = train, family = binomial)
predicted.values.glm = predict(m.glm, test, type='response')
predicted.glm = sapply(predicted.values.glm, FUN=round)
t.glm = table(predicted.glm, test$Purchase)
results(t.glm)

# KNN classifier
library(class)
knn.1 = knn(train[, -86], test[,-86], train[,86], k=1)
results(table(knn.1, test$Purchase))
knn.15 = knn(train[, -86], test[,-86], train[,86], k=15)
results(table(knn.15, test$Purchase))

# Naive Bayer
#install.packages('e1071')
library(e1071)
nb = naiveBayes(Purchase ~ ., train)
nb.predict = predict(nb, test, type='class')
t.nb = table(nb.predict, test$Purchase)
results(t.nb)

