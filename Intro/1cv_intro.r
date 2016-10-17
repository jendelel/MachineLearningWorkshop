examples <- read.csv("mov.development.csv", sep="\t")

# number of rows
nrow(examples)

# what kind of data are we looking at?
str(examples)

# length - vector length, unique - removes duplicates, $ - get col from dataset
# get number of users
length(unique(examples$user))

# get number of movies
length(unique(examples$movie))

# get number of ratings for each user
t <- table(examples$user)
t

# how many movies has the user with the highest number of ratings seen?
max(t)

# which user(s)?
ratings <- as.data.frame(t)
length(which(ratings$Freq == max(t)))
which(ratings$Freq == max(t))

# make a plot of rating vs. number of ratings
r <- table(examples$rating)
pdf("ratings.pdf")
plot(r, main = "MovieLens: frequency",
	xlab = "Rating (1-5)",
	ylab = "Number of ratings",
	type = "bar",
	col = "red",
	pch = "o")
dev.off()

# get movies rated at least 3 times
m <- as.data.frame(table(examples$movie))
m.1 <- which(m$Freq >=3)

# get average rating of each movie rated at least 3 times.
m.3 <-subset(examples, examples#movie %in% m.1)
mean <- tapply(m.3$rating, m.3$movie, mean)

# sort them according to their average rating in descending order
# focus on the Top 5
sort(mean, decreasing=TRUE)[1:5]

# compute  standard deviations
sd <- tapply(m.3$rating, m.3$movie, sd)

movies <- unique(examples[,c(1,9:33)])
movies[408,]
