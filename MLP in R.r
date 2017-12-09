# Using Multilayer Perceptron Models for Multilabel Classification

load("/Users/david/GitHub/Stat-Clustering-mini-project/dataframe.rdata")
library(nnet)
library(caTools)
library(neuralnet)
library(NeuralNetTools)
library(ClusterR)

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
  plotnet(neural)
}
