seq(1, 10, 3)
seq(1, 100, 2)
vec <- c(36, 23, 20, 29)
vec - 5
# vec <- (36, 23, 20, 29)
sizes <- c("small", "medium", "large")
popcorn <- sample(sizes, 25, replace=TRUE, prob=c(0.2, 0.5, 0.3))
popcorn_counts <- table(popcorn)
popcorn_counts
barplot(popcorn_counts, main='Frequency of Popcorn Size', 
        xlab='popcorn size', ylab='frequency')
ph <- read.csv('~/Desktop/Iowa/Regression/president_heights.csv')
colnames(ph) <- c("President", "Height_in")
head(ph)
hist(ph$Height_in, main="President Height", xlab="Height(in)", ylab="Frequency")
summary(ph$Height_in)
#Idk 