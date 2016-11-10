#setwd('e:/School/MFF/MachineLearning/Workshops')
# load data set
examples <- read.csv("mov.development.csv", sep="\t")

# Calculate the mutual information (information gain) IG(ranking;occupation)
#H(ranking) - H(ranking|occupation)

# code for entropy
entropy <- function(x) {
	p = table(x) / nrow(x)
	return( -sum (p *log(p)))
}

# code for conditional entropy(X, Y)
con.entropy <- function(x,y) {
	# - \sum p(x, y) * log2(p(x, y) / p(x))
	n = nrow(x) # should be equal to nrow(y)
	p.y = table(y) / n # p(y)
	p.joint <- as.vector(table(y, x)) / N # p(y,x)
	p.cond = p.joint / rep(p.y, nrow(table(x))) # p(x,y) = p(y,x) / p(y)
	H.cond = - sum( p.joint[p.joint > 0] * log2(p.cond[p.cond > 0]))
	return ( H.cond)
}

con.entropy(examples$ranking, examples$occupation)


#################################
d <- USArrests

# some useful info about dataset
attributes(d)
dim(USArrests)

attach(d)
hist(Rape, 
	breaks=10,
	col='blue', 
	xlab='Number of rapes', 
	main='Histogram of rape rate in the United States in 1973')

#Assuals and rapes
examples = d[, c(2,4)]

#clustering k = 3
set.seed(1234)
km.3.20 = kmeans(examples, centers = 3, nstart=20)
km.3.20$tot.withinss
km.3.20$tot.withinss

detach()
