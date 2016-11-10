# 2. HW Intro to ML, Lukas Jendele, Charles University
# Instructions: http://ufal.mff.cuni.cz/~hladka/2016/docs/hw2.pdf

# load data and set fixed seed
library(ISLR)
set.seed(42)

# Task 1
# Subtask a)
# Produce scatter plot with all attributes in the data 
# and compute matrix of correlations between quantitative attributes.
plot(Auto, main="All attributes in Auto dataset")

# select all attributes except for name
Auto.quant = Auto[, 1:8]
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

# TODO: Add explanation

# Subtask c)	
# Sort the data
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
# Subtask a)
set.seed(42)
mpg01 = rep(0, nrow(Auto))
mpg01[Auto$mpg >= median(Auto$mpg)] = 1
d = data.frame(mpg01, Auto[,-1])

# Subtask b)
rnd = sample(nrow(d))
train.num = round(nrow(d)*0.8)
test.num = round(nrow(d)*0.2)

# generate indices
train.indices = rnd[1:train.num]
test.indices = rnd[(train.num+1):nrow(d)]

train = d[train.indices,]
test = d[test.indices,]