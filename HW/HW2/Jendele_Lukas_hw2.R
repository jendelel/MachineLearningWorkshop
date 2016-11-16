# 2. HW Intro to ML, Lukas Jendele, Charles University
# Instructions: http://ufal.mff.cuni.cz/~hladka/2016/docs/hw2.pdf

# load data and set fixed seed
library(ISLR)
set.seed(42)

# Task 1
# Subtask a)
# Produce scatter plot with all attributes in the data 
# and compute matrix of correlations between quantitative attributes.
#png('Scatter_plot_matrix.png')
plot(Auto, main="All attributes in Auto dataset")
#dev.off()

# select all attributes except for name (string) and origin (categorical)
Auto.quant = Auto[, 1:7]
cor(Auto.quant)

# Subtask b)
# Make origin categorial
origin.str = rep("1", nrow(Auto))
origin.str[Auto$origin == 2] = "EUR"
origin.str[Auto$origin == 3] = "JPN"
examples.m1 = data.frame(Auto, origin.str)
m.1 <- lm( mpg ~ cylinders+displacement+horsepower+weight+acceleration+year+origin.str, as.data.frame(Auto) )

summary(m.1)
coef(m.1)

# Each coeficient states the change of mpg for unit increment of the attribute while other attributes are fixed.
# Detailed explanation in the document

# Subtask c)	
# Sort the data
# 
examples.sorted = Auto[order(Auto$acceleration),]
attach(examples.sorted)
fit1 <- lm(mpg ~ poly(acceleration, 1), data=examples.sorted) 
fit2 <- lm(mpg ~ poly(acceleration, 2), data=examples.sorted) 
fit3 <- lm(mpg ~ poly(acceleration, 3), data=examples.sorted) 
fit4 <- lm(mpg ~ poly(acceleration, 4), data=examples.sorted) 
fit5 <- lm(mpg ~ poly(acceleration, 5), data=examples.sorted) 

plot(acceleration, mpg, main="ISLR: Auto data set", 
	xlab = "Acceleration",
	ylab = "Miles Per Gallon",
	pch = 19,
	col = "red")
 
points(acceleration, predict(fit1), type="l", lwd=5, col="blue") 
points(acceleration, predict(fit2), type="l", lwd=5, col="black") 
points(acceleration, predict(fit3), type="l", lwd=5, col="orange") 
points(acceleration, predict(fit4), type="l", lwd=5, col="purple") 
points(acceleration, predict(fit5), type="l", lwd=5, col="green") 

lbs = c(sprintf("Linear, R^2=%f", summary(fit1)$r.squared), 
	sprintf("Degree 2, R^2=%f", summary(fit2)$r.squared),
	sprintf("Degree 3, R^2=%f", summary(fit3)$r.squared),
	sprintf("Degree 4, R^2=%f", summary(fit4)$r.squared),
	sprintf("Degree 5, R^2=%f", summary(fit5)$r.squared))

legend("topleft",
	lbs,
	col = c("blue", "black", "orange", "purple", "green"),
	lty = c(1,1,1),
	lwd=c(5,5,5))

detach()

# Task 2:
# Convert origin to binary features
origin.str = rep("1", nrow(Auto))
origin.str[Auto$origin == 2] = "EUR"
origin.str[Auto$origin == 3] = "JPN"

# Subtask a)
set.seed(1995)
mpg01 = rep(0, nrow(Auto))
mpg01[Auto$mpg >= median(Auto$mpg)] = 1
d = data.frame(mpg01, Auto[,-1], origin.str)

# Subtask b)
rnd = sample(nrow(d))
train.num = round(nrow(d)*0.8)
test.num = round(nrow(d)*0.2)

# generate indices
train.indices = rnd[1:train.num]
test.indices = rnd[(train.num+1):nrow(d)]

train = d[train.indices,]
test = d[test.indices,]

# Subtask c) 
print("Train mpg01 frequency: ")
addmargins(table(train$mpg01))

trivialClassifier <- function(dataframe) {
	constVal = which.max(as.vector(table(train$mpg01))) -1
	return ( rep(constVal, nrow(dataframe) ))
}
print(sprintf("Trivial classifier always classifies as: %d", 
	which.max(as.vector(table(train$mpg01))) -1 ))

# Run trivial classifier on test
predicted.trivial = trivialClassifier(test)

# Accuracy:  (TP+TN)/(TP+FP+TN+FN)
# calculate the confucion matrix
cm.trivial = table(factor(predicted.trivial, 0:1), test$mpg01)
acc.trivial = sum(diag(cm.trivial)) / sum(cm.trivial)
print(sprintf("Accuracy of trivial classifier on test dataset is: %f%%", acc.trivial * 100))

# Compute the entropy of mpg01
entropy <- function(x){
  # computes H(x)
  # expects a factor
  
  p <- table(x) / NROW(x)
  return( -sum(p * log2(p)) )
} 

# Compute entropy of mpg01 on train/test dataset
print(sprintf(
	"The entropy of mpg01 on train dataset is: %f", entropy(train$mpg01)))
print(sprintf(
	"The entropy of mpg01 on test dataset is: %f", entropy(test$mpg01)))

# Subtask d)
m.glm = glm(mpg01 ~ cylinders+displacement+horsepower+weight+acceleration+year+origin.str, data=train, family=binomial(link='logit'))

# Subtask d.1)
#training error rate
out.train = predict.glm(m.glm, train, type='response')
predicted.train.glm = sapply(out.train, FUN=round)
t.train.glm = table(factor(predicted.train.glm, 0:1), factor(train$mpg01, 0:1))
acc.train.glm = sum(diag(t.train.glm)) / sum(t.train.glm)
print(sprintf(
	"Train error rate of logistic regression is: %f%%", 100 - 100 * acc.train.glm))

# confusion matrix for test dataset
out.test = predict.glm(m.glm, test, type='response')
predicted.test.glm = sapply(out.test, FUN=round)
t.test.glm = table(factor(predicted.test.glm, 0:1), factor(test$mpg01, 0:1))
print("Confusion matrix for test dataset:")
addmargins(t.test.glm)

# test error rate
acc.test.glm = sum(diag(t.test.glm)) / sum(t.test.glm)
print(sprintf(
	"Test error rate of logistic regression is: %f%%", 100 - 100 * acc.test.glm))

# Provide an interpretation of each hypothesis parameter
print("TODO: Hypothesis parameter, watch out, this is logistic regression")
coef(m.glm)
round(exp(coef(m.glm)), 3)

# Subtask e) 
library(rpart)
m.rpart = rpart(mpg01 ~ cylinders+displacement+horsepower+weight+acceleration+year+origin, train)
summary(m.rpart)
# Subtask e.1)
# Plot of the tree
library(rpart.plot)
rpart.plot(m.rpart)
# Train error rate
predicted.train.rpart = sapply(predict(m.rpart, train[, -1], type="vector"), FUN=round)
t.train.rpart = table(factor(predicted.train.rpart, 0:1), factor(train$mpg01, 0:1))
acc.train.rpart = sum(diag(t.train.rpart)) / sum(t.train.rpart)
print(sprintf(
	"Train error rate of decision tree is: %f%%", 100 - 100 * acc.train.rpart))

# Test error rate
predicted.test.rpart = sapply(predict(m.rpart, test[, -1], type="vector"), FUN=round)
t.test.rpart = table(factor(predicted.test.rpart, 0:1), factor(test$mpg01, 0:1))
acc.test.rpart = sum(diag(t.test.rpart)) / sum(t.test.rpart)
print(sprintf(
	"Test error rate of decision tree is: %f%%", 100 - 100 * acc.test.rpart))

# Subtask e.2)
library(rpart)
m.rpart = rpart(mpg01 ~ cylinders+displacement+horsepower+weight+acceleration+year+origin, data = train, cp=0)
printcp(m.rpart)
plotcp(m.rpart)
summary(m.rpart)
# Subtask e.1)
# Plot of the tree
library(rpart.plot)
rpart.plot(m.rpart)
# Train error rate
predicted.train.rpart = sapply(predict(m.rpart, train[, -1], type="vector"), FUN=round)
t.train.rpart = table(factor(predicted.train.rpart, 0:1), factor(train$mpg01, 0:1))
acc.train.rpart = sum(diag(t.train.rpart)) / sum(t.train.rpart)
print(sprintf(
	"Train error rate of decision tree is: %f%%", 100 - 100 * acc.train.rpart))

# Test error rate
predicted.test.rpart = sapply(predict(m.rpart, test[, -1], type="vector"), FUN=round)
t.test.rpart = table(factor(predicted.test.rpart, 0:1), factor(test$mpg01, 0:1))
acc.test.rpart = sum(diag(t.test.rpart)) / sum(t.test.rpart)
print(sprintf(
	"Test error rate of decision tree is: %f%%", 100 - 100 * acc.test.rpart))