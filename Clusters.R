# Clean the zeros and extract the relevant vectors for Kmeans
patient <- people[[i]]
patient.clean <- subset(patient, Targets > 0)
patient.raw <-
  patient.clean[c("FSC.H", "SSC.H", "FL1.H", "FL2.H", "FL3.H", "FL4.H")]

# Run kmeans with a k given by the manual gates
k <- nlevels(factor(patient.clean$Targets))
km <- kmeans(patient.raw, k)

# Create new dataframe with raw data and new vectors
patient.new <- patient.raw
patient.new$Predict <- km$cluster
patient.new$Actual <- as.integer(patient.clean$Targets)

# Creare dataframe of zero rows to bind with new
patient.ztemp <- subset(patient, Targets == 0)
patient.z <-
  patient.ztemp[c("FSC.H", "SSC.H", "FL1.H", "FL2.H", "FL3.H", "FL4.H")]
patient.z$Actual <- as.integer(patient.ztemp$Targets) # the best way to rename afaik
patient.z$Predict <- integer(length(patient.z$Actual))

# And combine...
patient.new <- rbind(patient.new, patient.z)

# Build a confusion matrix
conMatrix <- table(patient.new$Predict, patient.new$Actual)
print(conMatrix)

# Calculate the F-Measure
f.measure <- function(predicted, true) {
	retrieved <- sum(predicted)
	precision <- sum(predicted & true)/retrieved
	recall <- sum(predicted & true)/sum(true)
	fmeasure <- 2 * precision * recall/(precision + recall)
	return(fmeasure)
}
fmeasure <- f.measure(patient.new$Predict, patient.new$Actual)
print(fmeasure)

# Calculate the NMI
nmi <- external_validation(patient.new$Actual, patient.new$Predict, method = "nmi")
print(nmi)

# Plot in three dimensions
scatterplot3d(
  patient.new$FSC.H,
  patient.new$SSC.H,
  patient.new$FL1.H,
  color = patient.new$Predict,
  pch = patient.new$Actual,
  main = paste("Patient", i, sep = " "),
  xlab = "FSC.H",
  ylab = "SSC.H",
  zlab = "FL1.H"
)