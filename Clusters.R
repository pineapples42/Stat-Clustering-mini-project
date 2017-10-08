load("/Users/david/Documents/GitHub/Stat-Clustering-mini-project/dataframe.rdata")
library(ClusterR)

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
	patient.raw <- patient.clean[c("FSC.H", "SSC.H", "FL1.H", "FL2.H", "FL3.H", "FL4.H")]
	patient.scaled <- scale(patient.raw)
	km <- kmeans(patient.scaled, nlevels[i] - 1)
	patient.scaled <- cbind(patient.scaled, "Predict" = km$cluster)
	patient.scaled <- cbind(patient.scaled, patient.clean$Targets))
	conMatrix <- table(patient.scaled["Predict"], patient.scaled["V1"])
	retrieved <- sum(patient.scaled["Predict"])
	precision <- sum(patient.scaled["Predict"] & patient.scaled["V1"])/retrieved
	recall <- sum(patient.scaled["Predict"] & patient.scaled["V1"])/sum(patient.scaled["V1"])
	Fmeasure <- 2 * precision * recall/(precision + recall)
	print(conMatrix)
	print(Fmeasure)
	nmi <- external_validation(patient.scaled["V1"], patient.scaled["Predict"], method = "nmi")
	print(nmi)
}

par(mfrow = c(3,4))
# For each patient:
for (i in 1:12) {
	# Create a subset for the patient
	patient <- people[[i]]
	# Clean the zero rows from the patient
	patient.clean <- subset(patient, Targets > 0)
	# Subset for use in kmeans
	patient.raw <- patient.clean[c("FSC.H", "SSC.H", "FL1.H", "FL2.H", "FL3.H", "FL4.H")]
	# Run kmeans using nlevels vector from earlier
	km <- kmeans(patient.raw, nlevels[i] - 1)
	# Add kmeans predictions to scaled dataframe
	patient.raw$Predict <- km$cluster
	# Return actual values to scaled dataframe
	patient.raw$Actual <- patient.clean$Targets
	# Create a confusion matrix for the dataframe & print it
	conMatrix <- table(patient.raw$Predict, patient.raw$Actual)
	retrieved <- sum(patient.raw$Predict)
	precision <- sum(patient.raw$Predict & patient.raw$Actual)/retrieved
	recall <- sum(patient.raw$Predict & patient.raw$Actual)/sum(patient.raw$Actual)
	Fmeasure <- 2 * precision * recall/(precision + recall)
	print(conMatrix)
	print(Fmeasure)
	nmi <- external_validation(patient.raw$Actual, patient.raw$Predict, method = "nmi")
	print(nmi)
	scatterplot3d(patient.raw$FSC.H, patient.raw$SSC.H, patient.raw$FL1.H, color=patient.raw$Predict, pch=patient.raw$Actual)
	# plot(patient.raw$FSC.H, patient.raw$SSC.H, col=patient.raw$Predict, pch=patient.raw$Actual)
}