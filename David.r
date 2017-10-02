# Load the matrix for patient one (adapted from flowframe)
load('~/Desktop/patient1matrix.Rdata')

# list activated data
ls()

svd1 <- svd(patient1)
svd1$d

kmtest <- kmeans(patient1, 1)

# scale the data
patient1scaled <- scale(patient1)

# for loop attempt
kmbss <- c()
kmwss <- c()
kmtss <- c()
for (i in 1:20) {
	km <- kmeans(patient1scaled, i)
	kmbss <- c(kmbss, km$betweenss)
	kmwss <- c(kmwss, mean(km$withinss))
	kmtss <- c(kmtss, km$totss)
}
kmscore <- kmwss / kmtss
kmscore
plot(kmscore)