#setwd("e:/School/MFF/MachineLearning/HW")

data <- read.csv("mov.development.csv", sep="\t")

# Task 1: 
# 3 different histograms with different numbers of bins for the IMDB rating:
png('hist1.png')
hist(data$imdb_rating, breaks=5, main="Histogram of IMDB rating", xlab="IMDB rating", xlim=c(0,10))
dev.off()
png('hist2.png')
hist(data$imdb_rating, breaks=10, main="Histogram of IMDB rating", xlab="IMDB rating", xlim=c(0,10))
dev.off()
png('hist3.png')
hist(data$imdb_rating, breaks=100, main="Histogram of IMDB rating", xlab="IMDB rating", xlim=c(0,10))
dev.off()

# Task 2: 
# Produce scatter plot of the IMDB rating and average rating (separately for men and women)
library(data.table)

table_data <- as.data.table(data)
xy_rating_gender = table_data[,list(avgRating= mean(rating)),c('movie', 'imdb_rating', 'gender')]
xy_rating_men = xy_rating_gender[xy_rating_gender$gender == 'M', c("imdb_rating","avgRating"), with=F]
xy_rating_women = xy_rating_gender[xy_rating_gender$gender == 'F', c("imdb_rating","avgRating"), with=F]
png('scatter_ratings_men.png')
plot(xy_rating_men, main="Average rating vs. IMDB rating by men", xlab="IMDB rating", ylab="Average rating")
dev.off()
png('scatter_ratings_women.png')
plot(xy_rating_women, main="Average rating vs. IMDB rating by women", xlab="IMDB rating", ylab="Average rating")
dev.off()

# Task 3:
# Produce side-by-side boxplots of the ratins of the movies rated 67 times.
# For each movie, draw a point for its average rating in the corresponding boxplot.

# Create a table with num of rating and average rating for each movie
xy_rating_count = table_data[,list(ratingCount=length(rating), ratingAvg=mean(rating)),c('movie')]
movies.67 = xy_rating_count[xy_rating_count$ratingCount == 67]
movie_ids = as.vector(movies.67[,'movie',with=F])
ratingsData.67 = table_data[table_data$movie %in% movie_ids$movie, c('movie', 'title', 'rating'), with=F]

# Movie titles cannot be inserted automatically. We would have to do it manually.
png('boxplot_rating_movie_67.png')
boxplot(formula=rating ~ movie, data=data, subset = movie %in% movie_ids$movie, main="Ratings of movies rated 67 times", xlab="Movie ID", ylab="Rating score")
points(1:nrow(movies.67), movies.67$ratingAvg, col='red', pch=21, bg='red')
dev.off()

# Task 4:
# The data set was collected during the period from 1997-09-20 05:05:10 through 1998-04-23
# 01:10:38.  Split this period into 10 intervals with the identical number of votes (i.e.  10,000)
data.sorted.bytime = data[order(data$timestamp, decreasing=F), ]
factor4 = ceiling(seq(nrow(data))/10000)
ds = split(data.sorted.bytime, factor4)

# Subtask a)
my_min <- function(dataframe) { return(min(dataframe$timestamp)); }
my_max <- function(dataframe) { return(max(dataframe$timestamp)); }
mins = lapply(ds, FUN=my_min)
maxs = lapply(ds, FUN=my_max)

# Subtask b)
for(i in 1:10) {
	cat(sprintf("Interval: %d\tDuration: %4.2f days\tNumber of movies rated: %d\n", i, (maxs[[i]] - mins[[i]])/3600/24, length(table(ds[[i]]$movie))))
}

# Subtask c)
# Custom functions that return average rating for given dataframe and occupation.
techRating <- function(dataframe) { 
tbl = as.data.table(dataframe)
	return (tbl[tbl$occupation == 'technician',list(avgRating=mean(rating)), 'occupation']$avgRating)
}
artistRating <- function(dataframe) { 
tbl = as.data.table(dataframe)
	return(tbl[tbl$occupation == 'artist',list(avgRating=mean(rating)), 'occupation']$avgRating)
}
marketRating <- function(dataframe) { 
tbl = as.data.table(dataframe)
	return (tbl[tbl$occupation == 'marketing',list(avgRating=mean(rating)), 'occupation']$avgRating)
}
# Apply custom functions on the list of intervals
tech = lapply(ds, FUN=techRating)
art = lapply(ds, FUN=artistRating)
market = lapply(ds, FUN=marketRating)

# Plot the data
png('scatter_occupation.png')
plot(0,0, xlim=c(1, 10), ylim=c(3, 4.2), main="Average ratings throughout time", xlab="Time interval", ylab="Average rating")
legend(x='topleft', y=0, c('technician', 'artist', 'marketing'), lty=c(1,1,1),, col=c('green', 'red', 'blue'))
lines(1:10, tech, type='o', pch=21, col='green')
lines(1:10, art, type='o', pch=21, col='red')
lines(1:10, market, type='o', pch=21, col='blue')
dev.off()

# Task 5 - group of users 18-30, 31-40, 41-50:
# Subtask a), most frequent profession in each group
group.1830 = data[data$age %in% 18:30,]
group.3140 = data[data$age %in% 31:40,]
group.4150 = data[data$age %in% 41:50,]
getMaxCol <- function(tbl) { 
	res <- max(table(tbl$occupation))
	names(res) <- names(which.max(table(tbl$occupation)))
	return(res) 
}
getMaxCol(group.1830)
getMaxCol(group.3140)
getMaxCol(group.4150)

# Subtask b)
# create names list of vectors containing ratings of the age group.
rating.groups = list(group.1830$rating, group.3140$rating, group.4150$rating)
names(rating.groups) <- c("18-30", "31-40", "41-50")
png('boxplot_ratings_groups.png')
boxplot(rating.groups, main="Ratings of different age groups", xlab="Age group", ylab="Rating")
dev.off()

# Task 6:
# Since genre are either 1 or 0, all we need to do is sum up these columns.
colSums(data[,11:28])
