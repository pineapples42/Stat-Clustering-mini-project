library(flowCore)
library(ClusterR)
library(scatterplot3d)
# Info for all 12 people will be stored in the 'people' variable in dataframe format
# EXAMPLES:
#     instead of summary(person5), use summary(people[[5]]) (double square brackets are neccesary)
#     people[[5]]$FSC.H
#     kmeans(people[[5]], 3)
#     plot(people[[1]]$FSC.H, people[[1]]$SSC.H)
# Add your own folder path in folder:
folder = '/Users/david/GitHub/Stat-Clustering-mini-project/FlowCore Data/' # David
#folder = '/home/persimmon/Documents/project/' #Max
people = list()
file = '1'
ext = '.fcs'
for (i in 1:9) {
  people = append(people, list(as.data.frame(read.FCS(
    as.character(paste(c(
      folder, '00', file, ext
    ), collapse = ''))
  )@exprs)))
  file = gsub(as.character(i), as.character(i + 1), file)
}
people = append(people, list(as.data.frame(read.FCS(
  as.character(paste(c(folder, '010.fcs'), collapse = ''))
)@exprs)))
people = append(people, list(as.data.frame(read.FCS(
  as.character(paste(c(folder, '011.fcs'), collapse = ''))
)@exprs)))
people = append(people, list(as.data.frame(read.FCS(
  as.character(paste(c(folder, '012.fcs'), collapse = ''))
)@exprs)))

#Adding manual gated results to dataframe
folder = '/Users/david/GitHub/Stat-Clustering-mini-project/Manual Gates/' # David
#folder = '/home/persimmon/Documents/project/' #Max
for (i in 1:9) {
  targets = as.matrix(read.csv(paste(
    c(folder, '00', as.character(i), '.csv'), collapse = ''
  )))
  people[[i]]$Targets = targets
}
targets = as.matrix(read.csv(paste(c(folder, '010.csv'), collapse = '')))
people[[10]]$Targets = targets
targets = as.matrix(read.csv(paste(c(folder, '011.csv'), collapse = '')))
people[[11]]$Targets = targets
targets = as.matrix(read.csv(paste(c(folder, '012.csv'), collapse = '')))
people[[12]]$Targets = targets

# Elbow Plots (Determining K if we didn't have the manual gates)
nlevels <- c()
for (i in 1:12) {
  obj <- factor(people[[i]]$Targets)
  nlevels <- c(nlevels, nlevels(obj))
}

scores = list()
clusters = list()
for (person in people) {
  kmscore <- c()
  for (k in 1:20) {
    km <- kmeans(person[,1:6], k, iter.max = 20)
    kmscore <- c(kmscore, km$betweenss / km$totss)
  }
  scores <- c(list(kmscore), scores)
}
par(mfrow = c(3, 4))
for (i in 1:12) {
  plot <- plot(
    scores[[i]],
    type = "p",
    ylab = "Between SS / Total SS",
    xlab = "Number of Clusters",
    main = paste("Patient", i, sep = ' ')
  )
  plot <-
    points(nlevels[i], scores[[i]][nlevels[i]], col = "red", pch = 3)
}

# Polynomial features v. No poly feat
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
for (i in 1:12) {
  pfeatures <- c()
  for (variable in 1:20) {
    k <- nlevels(factor(people[[i]]$Targets))
    people.poly <- poly(people[[i]])
    km <- kmeans(people.poly, k)
    nmi <-
      external_validation(people[[i]]$Targets, km$cluster, method = 'nmi')
    pfeatures <- c(pfeatures, nmi)
  }
  global.pfeatures <- c(global.pfeatures, mean(pfeatures))
}

global.nopfeatures <- c()
for (i in 1:12) {
  nopfeatures <- c()
  for (variable in 1:20) {
    k <- nlevels(factor(people[[i]]$Targets))
    people.poly <- people[[i]][, 1:6]
    km <- kmeans(people.poly, k)
    nmi <-
      external_validation(people[[i]]$Targets, km$cluster, method = 'nmi')
    nopfeatures <- c(nopfeatures, nmi)
  }
  global.nopfeatures <- c(global.nopfeatures, mean(nopfeatures))
}

height.pf <- rbind(global.nopfeatures, global.pfeatures)

par(mfrow = c(1,1))
pf <-
  barplot(
    height.pf,
    beside = TRUE,
    col = c(4, 2),
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
  labels = format(100 * round(height.pf, 2)),
  pos = 3,
  cex = 0.6
)

# Scaled vs. Unscaled
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
  global.not.scaled.nmi <-
    c(global.not.scaled.nmi, mean(not_scaled_nmi))
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

par(mfrow = c(1,1))
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
      paste("Unscaled, mean: ", round(mean(
        global.not.scaled.nmi
      ), 2), sep = ""),
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

# Grouped v. Individually Clustered
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
par(mfrow = c(1,1))
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

# Scatteplot of One Patient, Unscaled without PF
# Note that the NMI varies significantly across runs
patient <- people[[10]]
patient.raw <-
  patient[c("FSC.H", "SSC.H", "FL1.H", "FL2.H", "FL3.H", "FL4.H")]

k <- nlevels(factor(patient$Targets))
km <- kmeans(patient.raw, k - 1)

patient.new <- as.data.frame(patient.raw[, 1:6])
patient.new$Predict <- km$cluster
patient.new$Actual <- as.integer(patient$Targets)

conMatrix <- table(patient.new$Predict, patient.new$Actual)
print(conMatrix)

nmi <-
  external_validation(patient.new$Actual, patient.new$Predict, method = "nmi")
print(nmi)

patient.sample <- patient.new[sample(nrow(patient.new), 1000), ]

scatterplot3d(
  patient.sample$FSC.H,
  patient.sample$SSC.H,
  patient.sample$FL1.H,
  color = patient.sample$Predict,
  pch = patient.sample$Actual,
  main = "Patient 10: Unscaled, No Polynomial Features",
  sub = paste("NMI =", nmi, sep = " "),
  xlab = "FSC.H",
  ylab = "SSC.H",
  zlab = "FL1.H"
)

# Scatterplot of one patient, Scaled without PF
# Note that the NMI varies significantly across runs
patient <- people[[10]]
patient.raw <-
  patient[c("FSC.H", "SSC.H", "FL1.H", "FL2.H", "FL3.H", "FL4.H")]

patient.raw <- as.data.frame(scale(patient.raw))


k <- nlevels(factor(patient$Targets))
km <- kmeans(patient.raw, k - 1)

patient.new <- as.data.frame(patient.raw[, 1:6])
patient.new$Predict <- km$cluster
patient.new$Actual <- as.integer(patient$Targets)

conMatrix <- table(patient.new$Predict, patient.new$Actual)
print(conMatrix)

nmi <-
  external_validation(patient.new$Actual, patient.new$Predict, method = "nmi")
print(nmi)

patient.sample <- patient.new[sample(nrow(patient.new), 1000), ]

scatterplot3d(
  patient.sample$FSC.H,
  patient.sample$SSC.H,
  patient.sample$FL1.H,
  color = patient.sample$Predict,
  pch = patient.sample$Actual,
  main = "Patient 10: Scaled, No Polynomial Features",
  sub = paste("NMI =", nmi, sep = " "),
  xlab = "FSC.H",
  ylab = "SSC.H",
  zlab = "FL1.H"
)

# Scatterplot one patient, unscaled with polynomial features
# Note that the NMI caries significantly across runs
patient <- people[[10]]
patient.raw <-
  patient[c("FSC.H", "SSC.H", "FL1.H", "FL2.H", "FL3.H", "FL4.H")]

c = 7
for (i in 1:6) {
  for (j in 1:6) {
    patient.raw[, c] <- patient.raw[, j] * patient.raw[, i]
    c = c + 1
  }
}

k <- nlevels(factor(patient$Targets))
km <- kmeans(patient.raw, k - 1)

patient.new <- as.data.frame(patient.raw[, 1:6])
patient.new$Predict <- km$cluster
patient.new$Actual <- as.integer(patient$Targets)

conMatrix <- table(patient.new$Predict, patient.new$Actual)
print(conMatrix)

nmi <-
  external_validation(patient.new$Actual, patient.new$Predict, method = "nmi")
print(nmi)

patient.sample <- patient.new[sample(nrow(patient.new), 1000), ]

scatterplot3d(
  patient.sample$FSC.H,
  patient.sample$SSC.H,
  patient.sample$FL1.H,
  color = patient.sample$Predict,
  pch = patient.sample$Actual,
  main = "Patient 10: Unscaled, With Polynomial Features",
  sub = paste("NMI =", nmi, sep = " "),
  xlab = "FSC.H",
  ylab = "SSC.H",
  zlab = "FL1.H"
)

# Scaterrplot of one patient, scaled with polynomial features
# Note that the NMI varies significantly across runs
patient <- people[[10]]
patient.raw <-
  patient[c("FSC.H", "SSC.H", "FL1.H", "FL2.H", "FL3.H", "FL4.H")]

nmi <-
  external_validation(patient.new$Actual, patient.new$Predict, method = "nmi")
print(nmi)

patient.sample <- patient.new[sample(nrow(patient.new), 1000), ]

scatterplot3d(
  patient.sample$FSC.H,
  patient.sample$SSC.H,
  patient.sample$FL1.H,
  color = patient.sample$Predict,
  pch = patient.sample$Actual,
  main = "Patient 10: Unscaled, With Polynomial Features",
  sub = paste("NMI =", nmi, sep = " "),
  xlab = "FSC.H",
  ylab = "SSC.H",
  zlab = "FL1.H"
)

# Scaterrplot of one patient, scaled with polynomial features
# Note that the NMI varies significantly across runs
patient <- people[[10]]
patient.raw <-
  patient[c("FSC.H", "SSC.H", "FL1.H", "FL2.H", "FL3.H", "FL4.H")]

c = 7
for (i in 1:6) {
  for (j in 1:6) {
    patient.raw[, c] <- patient.raw[, j] * patient.raw[, i]
    c = c + 1
  }
}

patient.raw <- as.data.frame(scale(patient.raw))


k <- nlevels(factor(patient$Targets))
km <- kmeans(patient.raw, k - 1)

patient.new <- as.data.frame(patient.raw[, 1:6])
patient.new$Predict <- km$cluster
patient.new$Actual <- as.integer(patient$Targets)

conMatrix <- table(patient.new$Predict, patient.new$Actual)
print(conMatrix)

nmi <-
  external_validation(patient.new$Actual, patient.new$Predict, method = "nmi")
print(nmi)

patient.sample <- patient.new[sample(nrow(patient.new), 1000), ]

scatterplot3d(
  patient.sample$FSC.H,
  patient.sample$SSC.H,
  patient.sample$FL1.H,
  color = patient.sample$Predict,
  pch = patient.sample$Actual,
  main = "Patient 10: Scaled, With Polynomial Features",
  sub = paste("NMI =", nmi, sep = " "),
  xlab = "FSC.H",
  ylab = "SSC.H",
  zlab = "FL1.H"
)

# Initial v. Latest Results

# Save the patient data in files specified in class
folder = '/Users/david/GitHub/Stat-Clustering-mini-project/Patient Data/'
for (i in 1:12) {
  patient.matrix <- people[[i]][, 1:6]
  ytrue <- people[[i]][, 7]
  save(patient.matrix,
       ytrue,
       file = paste(folder, "Patient", i, ".Rdata", sep = ""))
}
#creating everyone with patient labels just so the code below works
everyone = people[[1]]
everyone['patient'] = rep(1, length(people[[1]]$FSC.H))
for(i in 2:12){
  person = people[[i]]
  person['patient'] = rep(i, length(people[[i]]$FSC.H))
  everyone = rbind(everyone, person)
}
# Creating density plots of fsc.h for each patient
par(mfrow = c(3,4))
for(i in 1:12){
  person = people[[i]]
  plot(density(person$FSC.H), main = as.character(i))
}

# Scatter plot of 12 with kmeans colors and target shapes
par(mfrow = c(3,4))
for (i in 1:12){
  patient <- people[[i]]
  patient.raw <-
    patient[c("FSC.H", "SSC.H", "FL1.H", "FL2.H", "FL3.H", "FL4.H")]
  
  k <- nlevels(factor(patient$Targets))
  km <- kmeans(patient.raw, k)
  
  patient.new <- patient.raw
  patient.new$Predict <- km$cluster
  patient.new$Actual <- as.integer(patient$Targets)
  
  conMatrix <- table(patient.new$Predict, patient.new$Actual)
  print(conMatrix)
  
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

# Creating and plotting initial vs final results
nmi_vector_final = c()
km_final_results = kmeans(poly(everyone[,1:6]),5)
for(i in 1:12){
  patient_eval = external_validation(km_final_results$cluster[everyone$patient == i & everyone$Targets != 0], everyone$Targets[everyone$patient == i & everyone$Targets != 0], method = 'nmi')
  nmi_vector_final = c(nmi_vector_final, patient_eval)
}
par(mfrow = c(1,1))
df = as.data.frame(global.nopfeatures)
df['final'] = nmi_vector_final
barplot(t(as.matrix(df)), beside = TRUE, legend.text = c('Before', 'After'), ylim = c(0,1), main='Initial vs. Final Results', names.arg = 1:12,  args.legend = list(x = "top"), col = c('red','blue'))

# creating and plotting Samspectral results
# Takes a long time though, don't do it unless you really want to
library(SamSPECTRAL)
sam_patient_nmi = c()
for(i in 1:12){
  person = people[[i]]
  sam = SamSPECTRAL(as.matrix(person[1:6]), normal.sigma = 150, separation.factor = .4, number.of.clusters = 5)
  nmi = external_validation(sam[person$Targets != 0], person$Targets[person$Targets != 0], method = 'nmi')
  sam_patient_nmi = c(sam_patient_nmi, nmi)
}
df = as.data.frame(nmi_vector_initial)
df['final'] = sam_patient_nmi
barplot(t(as.matrix(df)), beside = TRUE, legend.text = c('Initial Results', 'SamSpectral'), ylim = c(0,1), main='Sam vs Not', names.arg = 1:12,  args.legend = list(x = "top"), col = c('red','blue'))
