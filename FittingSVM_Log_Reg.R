#K-fold method for 
library(boot)
library(pROC)
library(e1071)


mydata <- read.csv("/Users/ashleyyang/Desktop/Stat228/everything_cleaned.csv")
head(mydata)
mydata <- mydata[,c(4,6,7,8,9,10,11,12,13)]

set.seed(1)

K <- 5
n <- nrow(mydata)
n.fold <- floor(n / K)
n.shuffle <- sample(1:n, n, replace = FALSE)
index.fold <- list()

for(i in 1:K) {
  if(i < K) {
    index.fold[[i]] <- n.shuffle[((i - 1) * n.fold + 1):(i * n.fold)]
  } else {
    index.fold[[i]] <- n.shuffle[((K - 1) * n.fold + 1):n]
  }
}

threshold.values <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)

misclassification_rates <- numeric(length(threshold.values))

sensitivity_rates <- numeric(length(threshold.values))
specificity_rates <- numeric(length(threshold.values))

for (j in 1:length(threshold.values)) {
  
  TP <- TN <- FP <- FN <- 0
  
  for(i in 1:K) {
    train_data <- mydata[-index.fold[[i]], ]
    test_data <- mydata[index.fold[[i]], ]
    
    fit <- glm(Legalized ~ ., data = train_data, family = "binomial")
    prob <- predict(fit, newdata = test_data, type = "response")
    
    pred <- ifelse(prob >= threshold.values[j], 1, 0)
    actual <- test_data$Legalized
    
    TP <- TP + sum(pred == 1 & actual == 1)
    TN <- TN + sum(pred == 0 & actual == 0)
    FP <- FP + sum(pred == 1 & actual == 0)
    FN <- FN + sum(pred == 0 & actual == 1)
  }
  
  misclassification_rates[j] <- (FP + FN) / n
  sensitivity_rates[j] <- TP / (TP + FN)
  specificity_rates[j] <- TN / (TN + FP)
}

for (j in 1:length(threshold.values)) {
  cat("Threshold:", threshold.values[j], 
      "- Misclassification Rate:", round(misclassification_rates[j], 4),
      "- Sensitivity:", round(sensitivity_rates[j], 4),
      "- Specificity:", round(specificity_rates[j], 4), "\n")
}

best_threshold <- threshold.values[which.min(misclassification_rates)]

cat("\nBest Threshold:", best_threshold, 
    "with Misclassification Rate:", min(misclassification_rates), 
    "Sensitivity:", sensitivity_rates[which.min(misclassification_rates)],
    "Specificity:", specificity_rates[which.min(misclassification_rates)], "\n")


##Model assumptions for Logistic Regression
log.model <- glm(Legalized ~ ., data = mydata, family = "binomial")
summary(log.model)

par(mfrow=c(2,2))
plot(log.model)
set.seed(1665)
train_index <- sample(1:nrow(mydata), 0.8 * nrow(mydata))

train_data <- mydata[train_index, ]
log.model <- glm(Legalized ~ ., data = train_data, family = "binomial")

summary(log.model)

library(e1071)

#~~~~~~~~~~~~~~~~# 
# SVM (using k-fold with cost tuning)

set.seed(1)
K <- 5
n <- nrow(mydata)
n.fold <- floor(n / K)
n.shuffle <- sample(1:n, n, replace = FALSE)
index.fold <- list()

for(i in 1:K) {
  if(i < K) {
    index.fold[[i]] <- n.shuffle[((i - 1) * n.fold + 1):(i * n.fold)]
  } else {
    index.fold[[i]] <- n.shuffle[((K - 1) * n.fold + 1):n]
  }
}

cost.values <- c(0.01, 0.1, 1, 10, 100)

misclassification_rates <- numeric(length(cost.values))
sensitivity_rates <- numeric(length(cost.values))
specificity_rates <- numeric(length(cost.values))

for (j in 1:length(cost.values)) {
  
  TP <- TN <- FP <- FN <- 0
  
  for(i in 1:K) {
    train_data <- mydata[-index.fold[[i]], ]
    test_data <- mydata[index.fold[[i]], ]
    
    svm.fit <- svm(Legalized ~ ., data = train_data, kernel = "radial", cost = cost.values[j], scale = TRUE)
    
    svm.prob <- predict(svm.fit, newdata = test_data)
    
    pred <- ifelse(svm.prob >= 0.5, 1, 0)
    actual <- test_data$Legalized
    
    TP <- TP + sum(pred == 1 & actual == 1)
    TN <- TN + sum(pred == 0 & actual == 0)
    FP <- FP + sum(pred == 1 & actual == 0)
    FN <- FN + sum(pred == 0 & actual == 1)
  }
  
  misclassification_rates[j] <- (FP + FN) / n
  sensitivity[j] <- TP/ (TP+FN)
  specificity[j] <- TN / (TN + FP)
  
}

for (j in 1:length(cost.values)) {
  cat("Cost:", cost.values[j], "- Misclassification Rate:", round(misclassification_rates[j], 4), "\n")
}
for (j in 1:length(cost.values)) {
  cat("Cost:", cost.values[j], "- Sensitivity:", round(sensitivity[j], 4), "\n")
}
for (j in 1:length(cost.values)) {
  cat("Cost:", cost.values[j], "- Specificity:", round(specificity[j], 4), "\n")
}



best_cost <- cost.values[which.min(misclassification_rates)]
for (j in 1:length(cost.values)) {
  cat("Cost:", cost.values[j], "- Misclassification Rate:", misclassification_rates[j], "\n")
}
cat("\nBest Cost:", best_cost, "with Misclassification Rate:", min(misclassification_rates), "\n")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
