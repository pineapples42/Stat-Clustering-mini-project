load("/Users/david/Documents/GitHub/Stat-Clustering-mini-project/dataframe.rdata")

# Create a list of the number of levels for each patient to be used as k
nlevels <- c()
for(i in 1:12){
	obj <- factor(people[[i]]$Targets)
	nlevels <- c(nlevels, nlevels(obj))
	}

# For each patient:
for(i in 1:12){
	# Create a subset for the patient
	patient <- people[[i]]
	# Clean the zero rows from the patient
	patient.clean <- subset(patient, Targets > 0)
	# Subset for use in kmeans
	patient.raw <- patient.clean[c("FSC.H", "SSC.H", "FL1.H", "FL2.H", "FL3.H", "FL4.H")]
	# Scale the data for cleaner results
	patient.scaled <- scale(patient.raw)
	# Run kmeans using nlevels vector from earlier
	km <- kmeans(patient.scaled, nlevels[i]-1)
	# Add kmeans predictions to scaled dataframe
	patient.scaled$Predicted <- km$cluster
	# Return actual values to scaled dataframe
	patient.scaled$Targets <- patient.clean$Targets
	# Create a confusion matrix for the dataframe & print it
	conMatrix <- table(patient.scaled$Predicted, patient.scaled$Targets)
	print(conMatrix)
	}