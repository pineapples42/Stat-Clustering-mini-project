# Using Multilayer Perceptron Models for Multilabel Classification

load("/Users/david/GitHub/Stat-Clustering-mini-project/dataframe.rdata")
library(nnet)
library(caTools)
library(neuralnet)
library(NeuralNetTools)
library(ClusterR)
library(SamSPECTRAL)
library(flowMeans)
library(scatterplot3d)

labelnames <- c("L1", "L2", "L3", "L4", "L5")
nn.nmi <- c()

for (person in people) {
  # Retrieve the individual patient and remove outliers
  patient <- subset(person, person$Targets != 0)
  
  # Scale the data and create a set of one-hot variables from the target vector, combine the result
  patient.class <-
    cbind(as.data.frame(scale(patient[, 1:6])), class.ind(patient$Targets))
  
  # Affix non-numerical labels to the class indicator columns
  names(patient.class) <-
    c(names(patient)[1:6], labelnames[1:(length(patient.class) - 6)])
  
  # Split data into training and test sets
  split <- sample.split(patient$Targets, SplitRatio = 0.80)
  train <- subset(patient.class, split == TRUE)
  test <- subset(patient.class, split == FALSE)
  
  # Store formula for access in model
  formula <-
    as.formula(paste(
      paste(labelnames[1:(length(patient.class) - 6)], collapse = " + "),
      " ~ ",
      paste(names(patient)[1:6], collapse = " + ")
    ))
  
  # Create neural net & train with training data
  neural <- neuralnet(
    formula,
    data = train,
    hidden = c(15, 10, 5),
    act.fct = "logistic",
    linear.output = FALSE,
    lifesign = "full"
  )
  
  # Compute matrix of predictions for test data
  prediction <- compute(neural, test[1:6])
  
  # Reform class indicator matrices into factor vectors
  y <- max.col(test[, 7:length(test)])
  yhat <- max.col(prediction$net.result)
  
  # Perform clustering validation
  nn.nmi <- c(nn.nmi, external_validation(y, yhat, method = "nmi"))
  
  # Optionally, generate confusion matrix
  print(table(y, yhat))
  
  # Optionally, plot a model of the neural net
  #plotnet(neural)
}

# For comparison, K-means with polynomial features on all patients grouped, then seperated for evaluation
polyfeat = function(a) {
  c = 7
  for (i in 1:6) {
    for (j in 1:6) {
      a[, c] = a[, j] * a[, i]
      c = c + 1
    }
  }
  return(a)
}

size <- c()
for (i in 1:12) {
  temp <- people[[i]]
  temp <- subset(temp, temp$Targets != 0)
  size <- c(size, nrow(temp))
}

count <- 1
nmiranges <- list()
for (i in 1:length(size)) {
  nmiranges[[i]] <- count:(count + size[i] - 1)
  count <- count + size[i]
}

everyone <- people[[1]]
for (i in 2:12) {
  everyone <- rbind(everyone, people[[i]])
}

everyone <- subset(everyone, everyone$Targets != 0)

kmeans.nmi <- c()
km.all <- kmeans(polyfeat(everyone[, 1:6]), 5)
for (range in nmiranges) {
  nmi <-
    external_validation(km.all$cluster[range], everyone$Targets[range], method = "nmi")
  kmeans.nmi <- c(kmeans.nmi, nmi)
}


# Back to the basics: no polynomial features or anything
km.nmi <- c()
for (i in 1:12) {
  k.nmi <- c()
  for (variable in 1:20) {
    km <- kmeans(people[[i]][, 1:6], nlevels(factor(people[[i]]$Targets)))
    nmi <-
      external_validation(people[[i]]$Targets, km$cluster, method = 'nmi')
    k.nmi <- c(k.nmi, nmi)
  }
  km.nmi <- c(km.nmi, mean(k.nmi))
}

# And now, SamSPECTRAL
sam.nmi <- c()
for (person in people) {
  patient <- subset(person, person$Targets != 0)
  sam <- SamSPECTRAL(
    as.matrix(patient[1:6]),
    normal.sigma = 100,
    separation.factor = .7,
    number.of.clusters = nlevels(factor(patient$Targets))
  )
  print(table(sam, patient$Targets))
  nmi <- external_validation(sam, patient$Targets, method = 'nmi')
  sam.nmi <- c(sam.nmi, nmi)
}

# Finally, flowMeans
fm.nmi <- c()
for (person in people) {
  patient <- subset(person, person$Targets != 0)
  flow <- flowMeans(patient, varNames = names(patient)[1:6], NumC = nlevels(factor(patient$Targets)))
  print(table(flow@Label, patient$Targets))
  nmi <- external_validation(flow@Label, patient$Targets, method = 'nmi')
  fm.nmi <- c(fm.nmi, nmi)
}


# BASIC V PREPROCESSED BARCHART
barheight2  <- rbind(km.nmi, kmeans.nmi)
colours <- c(4, 2)
mp2 <-
  barplot(
    barheight2,
    beside = TRUE,
    col = colours,
    names.arg = 1:12,
    main = "Basic K-means v. K-means with PreProcessing",
    ylab = "NMI",
    xlab = "Patient",
    legend.text = c(
      paste("Basic, mean: ", round(mean(km.nmi), 2), sep = ""),
      paste("Processed, mean: ", round(mean(kmeans.nmi), 2), sep = "")
    ),
    args.legend = list(x = "bottom", cex = 0.75),
    ylim = c(0, 1)
  )

text(
  mp2,
  barheight2,
  labels = format(100 * round(barheight2, 2)),
  pos = 1,
  cex = 0.6,
  col = "white"
)


# KMEANS V SAM BARCHART
barheight3  <- rbind(kmeans.nmi, sam.nmi)
colours <- c(4, 2)
mp3 <-
  barplot(
    barheight3,
    beside = TRUE,
    col = colours,
    names.arg = 1:12,
    main = "K-means with PreProcessing v. SamSPECTRAL",
    ylab = "NMI",
    xlab = "Patient",
    legend.text = c(
      paste("K-means, mean: ", round(mean(kmeans.nmi), 2), sep = ""),
      paste("SamSPECTRAL, mean: ", round(mean(sam.nmi), 2), sep = "")
    ),
    args.legend = list(x = "bottom", cex = 0.75),
    ylim = c(0, 1)
  )

text(
  mp3,
  barheight3,
  labels = format(100 * round(barheight3, 2)),
  pos = 1,
  cex = 0.6,
  col = "white"
)

# KMEANS V FLOWMEANS BARCHART
barheight4  <- rbind(kmeans.nmi, fm.nmi)
colours <- c(4, 2)
mp4 <-
  barplot(
    barheight4,
    beside = TRUE,
    col = colours,
    names.arg = 1:12,
    main = "K-means with PreProcessing v. flowMeans",
    ylab = "NMI",
    xlab = "Patient",
    legend.text = c(
      paste("K-means, mean: ", round(mean(kmeans.nmi), 2), sep = ""),
      paste("flowMeans, mean: ", round(mean(fm.nmi), 2), sep = "")
    ),
    args.legend = list(x = "bottom", cex = 0.75),
    ylim = c(0, 1)
  )

text(
  mp4,
  barheight4,
  labels = format(100 * round(barheight4, 2)),
  pos = 1,
  cex = 0.6,
  col = "white"
)


# FLOWMEANS V MLP BARCHART
barheight  <- rbind(fm.nmi, nn.nmi)
colours <- c(4, 2)
mp <-
  barplot(
    barheight,
    beside = TRUE,
    col = colours,
    names.arg = 1:12,
    main = "flowMeans v. Multilayer Perception",
    ylab = "NMI",
    xlab = "Patient",
    legend.text = c(
      paste("flowMeans, mean: ", round(mean(fm.nmi), 2), sep = ""),
      paste("MLP, mean: ", round(mean(nn.nmi), 2), sep = "")
    ),
    args.legend = list(x = "bottom", cex = 0.75),
    ylim = c(0, 1)
  )

text(
  mp,
  barheight,
  labels = format(100 * round(barheight, 2)),
  pos = 1,
  cex = 0.6,
  col = "white"
)

# KMEANS PROCESSED BOXPLOT
boxplot(
  kmeans.nmi,
  notch = F,
  col = "orange",
  main = "PreProcessing Results"
)


# SAM BOXPLOT
boxplot(
  sam.nmi,
  notch = F,
  col = "orange",
  main = "SamSPECTRAL Results"
)

# FLOW BOXPLOT
boxplot(
  fm.nmi,
  notch = F,
  col = "orange",
  main = "flowMeans Results"
)


# MLP BOXPLOT
boxplot(
  nn.nmi,
  notch = F,
  col = "orange",
  main = "Multilayer Perception Results"
)


# COMPARISON BOXPLOTS
boxplot(
  km.nmi,
  kmeans.nmi,
  sam.nmi,
  fm.nmi,
  nn.nmi,
  names = c("Basic K-means", "Preprocessing", "SamSPECTRAL", "flowMeans", "Multilayer Perception"),
  notch = F,
  col = "orange",
  main = "Comparison of Clustering/Classification Methods"
)

# RESULTS TABLE
nmitable <- rbind(km.nmi,kmeans.nmi, sam.nmi, fm.nmi, nn.nmi)
print(round(nmitable, 2))

round(mean(km.nmi), 2)
round(mean(kmeans.nmi), 2)
round(mean(sam.nmi), 2)
round(mean(fm.nmi), 2)
round(mean(nn.nmi), 2)

# Looking at Patient 4
patient4.sample <- people[[4]][sample(nrow(people[[4]]), 1000),]

scatterplot3d(
  patient4.sample$FSC.H,
  patient4.sample$SSC.H,
  patient4.sample$FL1.H,
  color = as.integer(patient4.sample$Targets) + 1,
  main = "Patient 4: Problematic Clustering",
  xlab = "FSC.H",
  ylab = "SSC.H",
  zlab = "FL1.H"
)
