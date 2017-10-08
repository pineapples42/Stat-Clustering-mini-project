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



# CLUSTERING FOR LOOPS STUFF
# Create a list of the number of levels for each patient to be used as k
nlevels <- c()
for (i in 1:12) {
  obj <- factor(people[[i]]$Targets)
  nlevels <- c(nlevels, nlevels(obj))
}

# SCALING DOESN'T WORK YET OOPS USE THE BLOCK BELOW THIS
# for (i in 1:12) {
patient <- people[[i]]
patient.clean <- subset(patient, Targets > 0)
patient.raw <-
  patient.clean[c("FSC.H", "SSC.H", "FL1.H", "FL2.H", "FL3.H", "FL4.H")]
patient.scaled <- scale(patient.raw)
km <- kmeans(patient.scaled, nlevels[i] - 1)
patient.scaled <- cbind(patient.scaled, "Predict" = km$cluster)
patient.scaled <- cbind(patient.scaled, patient.clean$Targets)
conMatrix <- table(patient.scaled["Predict"], patient.scaled["V1"])
retrieved <- sum(patient.scaled["Predict"])
precision <-
  sum(patient.scaled["Predict"] & patient.scaled["V1"]) / retrieved
recall <-
  sum(patient.scaled["Predict"] &
        patient.scaled["V1"]) / sum(patient.scaled["V1"])
Fmeasure <- 2 * precision * recall / (precision + recall)
print(conMatrix)
print(Fmeasure)
nmi <-
  external_validation(patient.scaled["V1"], patient.scaled["Predict"], method = "nmi")
print(nmi)
}


# THIS IS THE BLOCK THAT WORKS
par(mfrow = c(3, 4))
# For each patient:
for (i in 1:12) {
  # Create a subset for the patient
  patient <- people[[i]]
  # Clean the zero rows from the patient
  patient.clean <- subset(patient, Targets > 0)
  # Subset for use in kmeans
  patient.raw <-
    patient.clean[c("FSC.H", "SSC.H", "FL1.H", "FL2.H", "FL3.H", "FL4.H")]
  # Run kmeans using nlevels vector from earlier
  km <- kmeans(patient.raw, nlevels[i] - 1)
  # Add kmeans predictions to scaled dataframe
  patient.raw$Predict <- km$cluster
  # Return actual values to scaled dataframe
  patient.raw$Actual <- patient.clean$Targets
  # Create a confusion matrix for the dataframe & print it
  conMatrix <- table(patient.raw$Predict, patient.raw$Actual)
  retrieved <- sum(patient.raw$Predict)
  precision <-
    sum(patient.raw$Predict & patient.raw$Actual) / retrieved
  recall <-
    sum(patient.raw$Predict &
          patient.raw$Actual) / sum(patient.raw$Actual)
  Fmeasure <- 2 * precision * recall / (precision + recall)
  print(conMatrix)
  print(Fmeasure)
  nmi <-
    external_validation(patient.raw$Actual, patient.raw$Predict, method = "nmi")
  print(nmi)
  scatterplot3d(
    patient.raw$FSC.H,
    patient.raw$SSC.H,
    patient.raw$FL1.H,
    color = patient.raw$Predict,
    pch = patient.raw$Actual
  )
  # plot(patient.raw$FSC.H, patient.raw$SSC.H, col=patient.raw$Predict, pch=patient.raw$Actual)
}


