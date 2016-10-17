setwd("e:/School/MFF/MachineLearning/Intro")

cry.A <- read.csv("cry-A.csv", header = FALSE, sep = ";", col.names = c("id", "class", "void"))

str(cry.A)

# remove the void column
cry.A$void = NULL

# check the number of unique sentences
length(unique(cry.A$id))

# the 'class' distribution
table(cry.A$class)

barplot(table(cry.A$class), main="CRY\n annotated class histogram -- group A",
			col = rainbow(5),
			ylim = c(0, max(table(cry.A$class))+10))

library(data.table)

cry.A = data.table(cry.A)

tables()

cry.B = fread("cry-B.csv", header=F, sep=";", select = c(1,2), col.names = c("id", "class"))

setkey(cry.A, "id")
setkey(cry.B, "id")

tables()

# join tables using common index
cry.AB = cry.A[cry.B]
setnames(cry.AB, c("id", "A", "B"))

# now simple make a table with the IAA
table(cry.AB[, c(2,3), with=FALSE])

# Part III Exercises

#    Read annotation data and compute the Cohen's kappa value between groups A and B.
#    Help: Use table(cry.A$class) to get frequencies of the labels used by group A.

cry.AB.table <- table(cry.AB[, c(2,3), with=FALSE])

total.AB <- sum(cry.AB.table)
agree.AB <- sum(diag(cry.AB.table))
prA.AB <- agree.AB / total.AB
print(sprintf("Pr(A) = %f", prA.AB))

# dot product of sum of classes in A and B
prE.AB <- (table(cry.A$class) %*% table(cry.B$class)) / total.AB /total.AB
print(sprintf("Pr(E) = %f", prE.AB))

kappa.AB <- (prA.AB - prE.AB) / (1 - prE.AB)
print(sprintf("kappa = %f", kappa.AB))


#    Read the gold-standard data (= 250 examples) and the output of the automatic
#    classifier F1 (= the same 250 instances with labels assigned by F1 predictor).
#      What is the classifier accuracy?
#      Display the confusion matrix.
#      Then transform the confusion matrix into percentages.
#    Help: Confusion matrix is like the IAA matrix for F1 and GS.
#          Using table(., .) you can easily get it.
#          To do an operation with every column of a matrix, learn the apply() function.

# load data from csv as dataframes
cry.F1 <- fread("cry-F1.csv", header=F, sep=";", select = c(1,2), col.names = c("id", "class"))
cry.gs <- fread("cry-gs.csv", header=F, sep=";", select = c(1,2), col.names = c("id", "class"))

# convert to a table
setkey(cry.F1, "id")
setkey(cry.gs, "id")

tables()

# join tables using common index
cry.gsF1 = cry.gs[cry.F1]
setnames(cry.gsF1, c("id", "gs", "F1"))

cry.gsF1.table <- table(cry.gsF1[, c(2,3), with=FALSE])
print("confusion matrix gsF1:"); print(cry.gsF1.table)

# classifier accuracy
acc.gsF1 <- sum(diag(cry.gsF1.table)) / sum(cry.gsF1.table)
print(sprintf("classifier accuracy: %f", acc.gsF1))

# probability of errors in cols
round((cry.gsF1.table / as.vector(table(cry.gs$class))) * 100, 1)

