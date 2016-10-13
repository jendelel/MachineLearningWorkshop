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

cry.C = fread("cry-C.csv", header=F, sep=";", select = c(1,2), col.names = c("id", "class"))

setkey(cry.A, "id")
setkey(cry.C, "id")

tables()

# join tables using common index
cry.AC = cry.A[cry.C]
setnames(cry.AC, c("id", "A", "C"))

# now simple make a table with the IAA
table(cry.AC[, c(2,3), with=FALSE])

# Path III Exercises
HW
