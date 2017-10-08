# Clusters with measures of accuracy, for all the patients
# Load the dataframe.dataframe file first so that you have the people object

f.measure <- function(predicted, true) {
	retrieved <- sum(predicted)
	precision <- sum(predicted & true)/retrieved
	recall <- sum(predicted & true)/sum(true)
	fmeasure <- 2 * precision * recall/(precision + recall)
	return(fmeasure)
}


par(mfrow = c(3,4))
for (i in 1:12){
	patient <- people[[i]]
	patient.clean <- subset(patient, Targets > 0)
	patient.raw <-
	  patient.clean[c("FSC.H", "SSC.H", "FL1.H", "FL2.H", "FL3.H", "FL4.H")]
	
	k <- nlevels(factor(patient.clean$Targets))
	km <- kmeans(patient.raw, k)
	
	patient.new <- patient.raw
	patient.new$Predict <- km$cluster
	patient.new$Actual <- as.integer(patient.clean$Targets)
	
	patient.ztemp <- subset(patient, Targets == 0)
	patient.z <-
	  patient.ztemp[c("FSC.H", "SSC.H", "FL1.H", "FL2.H", "FL3.H", "FL4.H")]
	patient.z$Actual <- as.integer(patient.ztemp$Targets)
	patient.z$Predict <- integer(length(patient.z$Actual))
	
	patient.new <- rbind(patient.new, patient.z)
	
	print(paste("Patient", i, sep = " "))
	
	conMatrix <- table(patient.new$Predict, patient.new$Actual)
	print(conMatrix)
	
	fmeasure <- f.measure(patient.new$Predict, patient.new$Actual)
	print(fmeasure)
	
	nmi <- external_validation(patient.new$Actual, patient.new$Predict, method = "nmi")
	print(nmi)
	
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
}