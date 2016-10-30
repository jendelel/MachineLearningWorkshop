library(HSAUR) # contains dataset FORBES2000
library(rpart) # package for decision trees (reccursive partitioning)

# 1.quartile is value of quantile function at 25%
# 2.quartile (median) is value of quantile function at 50%
# 3.quartile is value of quantile function at 75%
summary(Forbes2000$profits)

# What is distribution of profit? 
hist(Forbes2000$profits, breaks=100)
# or we can use function cut to break the value to intervals
barplot(table(cut(Forbes2000$profits, 100)))

forbes = Forbes2000
forbes$profits[is.na(forbes$profits)] = 0				# replace NAs by 0
forbes$profits = factor(forbes$profits > 0.2)			# make binary category

set.seed(123); s = sample(1:2000)
sample
train = s[1:1000]
test = s[1001:2000]

# write.table(forbes())
forbes.train = forbes[train, 2:8]
forbes.test = forbes[test, 2:8]

# train the decision tree model/hypothesis
m = rpart(profits ~ category + sales +assets + marketvalue, forbes.train)
library(rpart.plot)
rpart.plot(m);


# making predicions
prediction  = predict(m, forbes.test[,-5], type="class")
table(prediction)

# confusion matrix 
table(prediction, forbes.test$profits)
