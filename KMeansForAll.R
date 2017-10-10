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
for (count in 1:12){
	patient <- people[[count]]
	patient.clean <- subset(patient, Targets > 0)
	patient.raw <-
	  patient.clean[c("FSC.H", "SSC.H", "FL1.H", "FL2.H", "FL3.H", "FL4.H")]
	
	c = 7
	for(i in 1:6){
		for(j in 1:6){
			patient.raw[,c] <- patient.raw[,j] * patient.raw[,i]
			c = c+1
		}
	}
	
	patient.raw <- as.data.frame(scale(patient.raw))
	
	
	k <- nlevels(factor(patient.clean$Targets))
	km <- kmeans(patient.raw, k)
	
	patient.new <- as.data.frame(patient.raw[,1:6])
	patient.new$Predict <- km$cluster
	patient.new$Actual <- as.integer(patient.clean$Targets)
	
    print(paste("Patient", count, sep = " "))

    conMatrix <- table(patient.new$Predict, patient.new$Actual)
    print(conMatrix)

    #fmeasure <- f.measure(patient.new$Predict, patient.new$Actual)
    #print(fmeasure)

	nmi <- external_validation(patient.new$Actual, patient.new$Predict, method = "nmi")
	print(nmi)
	
    scatterplot3d(
      patient.new$FSC.H,
      patient.new$SSC.H,
      patient.new$FL1.H,
      color = patient.new$Predict,
      pch = patient.new$Actual,
      main = paste("Patient", count, sep = " "),
      xlab = "FSC.H",
      ylab = "SSC.H",
      zlab = "FL1.H"
    )
}
