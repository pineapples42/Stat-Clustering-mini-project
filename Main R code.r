library(flowCore)
# Info for all 12 people will be stored in the 'people' variable in dataframe format
# EXAMPLES: 
#     instead of summary(person5), use summary(people[[5]]) (double square brackets are neccesary)
#     people[[5]]$FSC.H
#     kmeans(people[[5]], 3)
#     plot(people[[1]]$FSC.H, people[[1]]$SSC.H)
# Add your own folder path in folder:
folder = '/Users/david/Documents/GitHub/Stat-Clustering-mini-project/FlowCore Data/'
people = list()
file = '1'
ext = '.fcs'
for(i in 1:9){
  people = append(people, list(as.data.frame(read.FCS(as.character(paste(c(folder,'00',file,ext), collapse = '')))@exprs)))
  file =gsub(as.character(i), as.character(i+1), file)
             }
people = append(people, list(as.data.frame(read.FCS(as.character(paste(c(folder,'010.fcs'), collapse = '')))@exprs)))
people = append(people, list(as.data.frame(read.FCS(as.character(paste(c(folder,'011.fcs'), collapse = '')))@exprs)))
people = append(people, list(as.data.frame(read.FCS(as.character(paste(c(folder,'012.fcs'), collapse = '')))@exprs)))

# Elbow Plots (Determining K if we didn't have the manual gates)
scores = list()
clusters = list()
for(person in people){
    kmscore <- c()
    for (k in 1:30) {
        km <- kmeans(person, k, iter.max = 20)
        kmscore <- c(kmscore, km$betweenss / km$totss)
	}
    scores <- c(list(kmscore), scores)
}
par(mfrow = c(3,4))
for(i in 1:12){
	plot(scores[[i]])
}

#Adding manual gated results to dataframe
for(i in 1:9){
  targets = as.matrix(read.csv(paste(c(folder,'00',as.character(i),'.csv'), collapse = '')))
  people[[i]]$Targets = targets
}
targets = as.matrix(read.csv(paste(c(folder,'010.csv'), collapse = '')))
people[[10]]$Targets = targets
targets = as.matrix(read.csv(paste(c(folder,'011.csv'), collapse = '')))
people[[11]]$Targets = targets
targets = as.matrix(read.csv(paste(c(folder,'012.csv'), collapse = '')))
people[[12]]$Targets = targets

# Running Kmeans with the factor levels given by the manual gates.
# Updates people dataframe with new predicted and actual scores.
f.measure <- function(predicted, true) {
	retrieved <- sum(predicted)
	precision <- sum(predicted & true)/retrieved
	recall <- sum(predicted & true)/sum(true)
	fmeasure <- 2 * precision * recall/(precision + recall)
	return(fmeasure)
}

library(ClusterR)
library(scatterplot3d)
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
	
	people[[i]] <- patient.new
	
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
