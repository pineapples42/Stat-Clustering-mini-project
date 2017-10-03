# load the patient1 datframe first

# THE ELBOW METHOD
patient1scaled <- scale(patient1)

kmscore <- c()

for (i in 1:30) {
	km <- kmeans(patient1scaled, i, iter.max = 20)
	kmscore <- c(kmscore, km$betweenss / km$totss)
}

# the kmscore is a vector of the f-test results 
# (group variation / total variation)
# we plot it to look for a drop in the marginal gain

plot(kmscore)





# BELOW THIS BE OLD THINGS
# scaled data elbow hunt
patient1scaled <- scale(patient1)
kmbss <- c()
kmwss <- c()
kmtss <- c()
for (i in 2:30) {
	km <- kmeans(patient1scaled, i)
	kmbss <- c(kmbss, km$betweenss)
	kmwss <- c(kmwss, mean(km$withinss))
	kmtss <- c(kmtss, km$totss)
}
kmscore <- kmbss / kmtss
kmscorescaled
plot(kmscorescaled)

# unscaled data elbow hunt
kmbss <- c()
kmwss <- c()
kmtss <- c()
for (i in 2:30) {
	km <- kmeans(patient1, i)
	kmbss <- c(kmbss, km$betweenss)
	kmwss <- c(kmwss, mean(km$withinss))
	kmtss <- c(kmtss, km$totss)
}
kmscore <- kmbss / kmtss
kmscore
plot(kmscore)

# FOR LOOP WITH EXTRA STUFF
patient1scaled <- scale(patient1)

kmscore <- c()
kmbetweenss <- c()
kmtotss <- c()
kmmeanwithinss <- c()

for (i in 1:30) {
	km <- kmeans(patient1scaled, i, iter.max = 20)
	kmscore <- c(kmscore, km$betweenss / km$totss)
	kmbetweenss <- c(kmbetweenss, km$betweenss)
	kmtotss <- c(kmtotss, km$totss)
	kmmeanwithinss <- c(kmmeanwithinss, mean(km$withinss))
}

