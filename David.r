poly = function(a) {
  c = 7
  for (i in 1:6) {
    for (j in 1:6) {
      a[, c] = a[, j] * a[, i]
      c = c + 1
    }
  }
  return(a)
}

global.pfeatures <- c()
for(i in 1:12){
  pfeatures <- c()
  for (variable in 1:20){
    k <- nlevels(factor(people[[i]]$Targets))
    people.poly <- poly(people[[i]])
    km <- kmeans(people.poly, k)
    nmi <- external_validation(people[[i]]$Targets, km$cluster, method = 'nmi')
    pfeatures <- c(pfeatures, nmi)
  }
  global.pfeatures <- c(global.pfeatures, mean(pfeatures))
}

global.nopfeatures <- c()
for(i in 1:12){
  nopfeatures <- c()
  for (variable in 1:20){
    k <- nlevels(factor(people[[i]]$Targets))
    people.poly <- people[[i]][,1:6]
    km <- kmeans(people.poly, k)
    nmi <- external_validation(people[[i]]$Targets, km$cluster, method = 'nmi')
    nopfeatures <- c(nopfeatures, nmi)
  }
  global.nopfeatures <- c(global.nopfeatures, mean(nopfeatures))
}

height.pf <- rbind(global.nopfeatures, global.pfeatures)

pf <-
  barplot(
    height.pf,
    beside = TRUE,
    col = c(4,2),
    names.arg = 1:12,
    main = "Polynomial Features Comparison",
    ylab = "NMI",
    xlab = "Patient",
    ylim = c(0, 1),
    legend.text = c(
      paste("No Polynomial Features, mean: ", round(mean(global.nopfeatures), 2), sep = ""),
      paste("With Polynomial Features, mean: ", round(mean(global.pfeatures), 2), sep = "")
    ),
    args.legend = list(x = "top", cex = 0.75)
  )

text(
  pf,
  height.pf,
  labels = format(100* round(height.pf, 2)),
  pos = 3,
  cex = 0.6
)

######
global.not.scaled.nmi <- c()
for (i in 1:12) {
  not_scaled_nmi = c()
  for (variable in 1:20) {
    k <- nlevels(factor(people[[i]]$Targets))
    km <- kmeans((people[[i]][, 1:6]), k)
    nmi <-
      external_validation(people[[i]]$Targets, km$cluster, method = 'nmi')
    not_scaled_nmi = c(not_scaled_nmi, nmi)
  }
  global.not.scaled.nmi <- c(global.not.scaled.nmi, mean(not_scaled_nmi))
}

global.scaled.nmi <- c()
for (i in 1:12) {
  scaled_nmi = c()
  for (variable in 1:20) {
    k <- nlevels(factor(people[[i]]$Targets))
    km <- kmeans(scale(people[[i]][, 1:6]), k)
    nmi <-
      external_validation(people[[i]]$Targets, km$cluster, method = 'nmi')
    scaled_nmi = c(scaled_nmi, nmi)
  }
  global.scaled.nmi <- c(global.scaled.nmi, mean(scaled_nmi))
}

height.s <- rbind(global.not.scaled.nmi, global.scaled.nmi)

colours <- c(4, 2)
mp <-
  barplot(
    height.s,
    beside = TRUE,
    col = colours,
    names.arg = 1:12,
    main = "Scaled vs. Unscaled Comparison",
    ylab = "NMI",
    xlab = "Patient",
    ylim = c(0, 1),
    legend.text = c(
      paste("Unscaled, mean: ", round(mean(global.not.scaled.nmi), 2), sep = ""),
      paste("Scaled, mean: ", round(mean(global.scaled.nmi), 2), sep = "")
    ),
    args.legend = list(x = "top", cex = 0.75)
  )

text(
  mp,
  height.s,
  labels = format(100 * round(height.s, 2)),
  pos = 3,
  cex = 0.6
  
)

####
# NMI for patients when clustered individually
global_nmi <- c()
for (index in 1:12) {
  local_nmi <- c()
  for (count in 1:20) {
    person.raw <- people[[index]][, 1:6]
    k <- nlevels(factor(people[[index]]$Targets))
    km <- kmeans(person.raw, k)
    nmi <-
      external_validation(people[[index]]$Targets, km$cluster, method = "nmi")
    local_nmi <- c(local_nmi, nmi)
  }
  global_nmi <- c(global_nmi, mean(local_nmi))
}

# Binding everyone and then dividing
everyone = people[[1]]
for (i in 2:12) {
  everyone = rbind(everyone, people[[i]])
}

ranges <-
  list(
    1:13831,
    13832:28767,
    28768:43994,
    43995:56745,
    56746:73753,
    73754:88559,
    88560:102332,
    102333:117119,
    117120:134759,
    134760:158136,
    158137:190836,
    190837:207171
  )
everyone_nmi <- c()

km.all <- kmeans(everyone[, 1:6], 5)
for (range in ranges) {
  nmi <-
    external_validation(km.all$cluster[range], everyone$Targets[range], method = "nmi")
  everyone_nmi <- c(everyone_nmi, nmi)
}


height <- rbind(global_nmi, everyone_nmi)

colours <- c(4, 2)
mp <-
  barplot(
    height,
    beside = TRUE,
    col = colours,
    names.arg = 1:12,
    main = "Grouped v. Individually Clustered",
    ylab = "NMI",
    xlab = "Patient",
    legend.text = c(
      paste("Individually clustered, mean: ", round(mean(global_nmi), 2), sep = ""),
      paste("Grouped and clustered, mean: ", round(mean(everyone_nmi), 2), sep = "")
    ),
    args.legend = list(x = "top", cex = 0.75),
    ylim = c(0, 1)
  )

text(
  mp,
  height,
  labels = format(100 * round(height, 2)),
  pos = 3,
  cex = 0.6
)

mean(everyone_nmi)
mean(global_nmi)

####

# load flowset and dataframe
SS <-
  SamSPECTRAL(flowset$`001.fcs`@exprs,
              normal.sigma = 200,
              separation.factor = 0.5)
nmi <- external_validation(SS, people[[1]]$Targets, method = "nmi")


####
poly = function(a) {
  c = 7
  for (i in 1:6) {
    for (j in 1:6) {
      a[, c] = a[, j] * a[, i]
      c = c + 1
    }
  }
  return(a)
}

global_nmi <- c()
for (index in 1:12) {
  local_nmi <- c()
  for (count in 1:20) {
    person.raw <- people[[index]][, 1:6]
    person.pf <- poly(person.raw)
    k <- nlevels(factor(people[[index]]$Targets))
    km <- kmeans(person.pf, k)
    nmi <-
      external_validation(people[[index]]$Targets, km$cluster, method = "nmi")
    local_nmi <- c(local_nmi, nmi)
  }
  global_nmi <- c(global_nmi, mean(local_nmi))
}



# For one patient:
patient <- people[[10]]
patient.raw <-
  patient[c("FSC.H", "SSC.H", "FL1.H", "FL2.H", "FL3.H", "FL4.H")]

#c = 7
#for(i in 1:6){
#	for(j in 1:6){
#		patient.raw[,c] <- patient.raw[,j] * patient.raw[,i]
#		c = c+1
#	}
#}

patient.raw <- as.data.frame(scale(patient.raw))


k <- nlevels(factor(patient$Targets))
km <- kmeans(patient.raw, k)

patient.new <- as.data.frame(patient.raw[, 1:6])
patient.new$Predict <- km$cluster
patient.new$Actual <- as.integer(patient$Targets)

print(paste("Patient 10"))

conMatrix <- table(patient.new$Predict, patient.new$Actual)
print(conMatrix)

nmi <-
  external_validation(patient.new$Actual, patient.new$Predict, method = "nmi")
print(nmi)

patient.sample <- patient.new[sample(nrow(patient.new), 300), ]

scatterplot3d(
  patient.sample$FSC.H,
  patient.sample$SSC.H,
  patient.sample$FL1.H,
  color = patient.sample$Predict,
  pch = patient.sample$Actual,
  main = "Patient 10",
  xlab = "FSC.H",
  ylab = "SSC.H",
  zlab = "FL1.H"
)


# This takes Kmeans of the polynomial feature augmented matrix
par(mfrow = c(3, 4))
nmilist <- c()
for (count in 1:12) {
  patient <- people[[count]]
  patient.clean <- subset(patient, Targets > 0)
  patient.raw <-
    patient.clean[c("FSC.H", "SSC.H", "FL1.H", "FL2.H", "FL3.H", "FL4.H")]
  
  c = 7
  for (i in 1:6) {
    for (j in 1:6) {
      patient.raw[, c] <- patient.raw[, j] * patient.raw[, i]
      c = c + 1
    }
  }
  
  patient.raw <- as.data.frame(scale(patient.raw))
  
  
  k <- nlevels(factor(patient.clean$Targets))
  km <- kmeans(patient.raw, k)
  
  patient.new <- as.data.frame(patient.raw[, 1:6])
  patient.new$Predict <- km$cluster
  patient.new$Actual <- as.integer(patient.clean$Targets)
  
  print(paste("Patient", count, sep = " "))
  
  conMatrix <- table(patient.new$Predict, patient.new$Actual)
  print(conMatrix)
  
  #fmeasure <- f.measure(patient.new$Predict, patient.new$Actual)
  #print(fmeasure)
  
  nmi <-
    external_validation(patient.new$Actual, patient.new$Predict, method = "nmi")
  nmilist <- c(nmilist, nmi)
  print(nmi)
  
  patient.sample <- patient.new[sample(nrow(patient.new), 300), ]
  
  scatterplot3d(
    patient.sample$FSC.H,
    patient.sample$SSC.H,
    patient.sample$FL1.H,
    color = patient.sample$Predict,
    pch = patient.sample$Actual,
    main = paste("Patient", count, sep = " "),
    xlab = "FSC.H",
    ylab = "SSC.H",
    zlab = "FL1.H"
  )
}
mean(nmilist)

# STOP HERE

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


# Playing around with everyone
everyone = people[[1]]
for (i in 2:12) {
  everyone = rbind(everyone, people[[i]])
}

everyone = everyone[everyone$Targets != 0, 1:6]

c = 7
for (i in 1:6) {
  for (j in 1:6) {
    everyone[, c] = everyone[, j] * everyone[, i]
    c = c + 1
  }
}

everyone.scaled <- as.data.frame(scale(everyone))

km <-
  kmeans(everyone.scaled, nlevels(factor(everyone$Targets)), iter.max = 30)

km <- kmeans(everyone.scaled, 5, iter.max = 40)

everyone$Targets <-
  rbind(
    people[[1]][Targets != 0, 7],
    people[[2]][Targets != 0, 7],
    people[[3]][Targets != 0, 7],
    people[[4]][Targets != 0, 7],
    people[[5]][Targets != 0, 7],
    people[[6]][Targets != 0, 7],
    people[[7]][Targets != 0, 7],
    people[[8]][Targets != 0, 7],
    people[[9]][Targets != 0, 7],
    people[[10]][Targets != 0, 7],
    people[[11]][Targets != 0, 7],
    people[[12]][Targets != 0, 7]
  )

nmi <- external_validation

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


######

nlevels <- c()
for (i in 1:12) {
  obj <- factor(people[[i]]$Targets)
  nlevels <- c(nlevels, nlevels(obj))
}

preprocess <- c()
par(mfrow = c(3, 4))
# For each patient:
for (i in 1:12) {
  # Create a subset for the patient
  patient <- people[[i]]
  # Subset for use in kmeans
  patient.raw <-
    patient[c("FSC.H", "SSC.H", "FL1.H", "FL2.H", "FL3.H", "FL4.H")]
  # Run kmeans using nlevels vector from earlier
  km <- kmeans(patient.raw, nlevels[i] - 1)
  # Add kmeans predictions to scaled dataframe
  patient.raw$Predict <- km$cluster
  # Return actual values to scaled dataframe
  patient.raw$Actual <- patient$Targets
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
    pch = patient.raw$Actual,
    xlab = "FSC.H",
    ylab = "SSC.H",
    zlab = "FL1.H",
    main = paste("Patient", i, sep = " ")
  )
  preprocess <- c(preprocess, nmi)
  # plot(patient.raw$FSC.H, patient.raw$SSC.H, col=patient.raw$Predict, pch=patient.raw$Actual)
}
mean(preprocess)


## FROM MAIN
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

# Save patient data in separate files
folder = '/Users/david/Documents/GitHub/Stat-Clustering-mini-project/Patient Data/'
for(i in 1:12){
  save(people[[i]], file = paste(folder, "Patient", i, ".Rdata", sep=""))
}
