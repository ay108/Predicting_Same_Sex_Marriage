#K-fold method for 
library(boot)
library(pROC)
library(e1071)


mydata <-  read.csv("/Users/ashleyyang/Desktop/Stat228/everything_cleaned.csv")
head(mydata)
mydata <- mydata[,c(4,6,7,8,9,10,11,12,13)]

set.seed(11001)

K <- 5
n <- nrow(mydata)
n.fold <- floor(n / K)
n.shuffle <- sample(1:n, n, replace = FALSE)
index.fold <- list()

# Assign indices to folds
for(i in 1:K) {
  if(i < K) {
    index.fold[[i]] <- n.shuffle[((i - 1) * n.fold + 1):(i * n.fold)]
  } else {
    index.fold[[i]] <- n.shuffle[((K - 1) * n.fold + 1):n]
  }
}


TP <- TN <- FP <- FN <- 0

for(i in 1:K) {
  # Training data (excluding fold i)
  train_data <- mydata[-index.fold[[i]], ]
  test_data <- mydata[index.fold[[i]], ]
  
  # Fit logistic regression
  fit <- glm(Legalized ~ ., data = train_data, family = "binomial")
  
  # Predict probabilities
  prob <- predict(fit, newdata = test_data, type = "response")
  
  # Classify using 0.5 threshold
  pred <- ifelse(prob >= 0.5, 1, 0)
  actual <- test_data$Legalized
  
  # Update confusion matrix values
  TP <- TP + sum(pred == 1 & actual == 1)
  TN <- TN + sum(pred == 0 & actual == 0)
  FP <- FP + sum(pred == 1 & actual == 0)
  FN <- FN + sum(pred == 0 & actual == 1)
}

# Calculate metrics
misclassification_rate <- (FP + FN) / n
sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)


cat("Misclassification Rate:", misclassification_rate, "\n")
cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")



##Model assumptions for Logistic Regression
log.model <- glm(Legalized ~ ., data = mydata, family = "binomial")
summary(log.model)

mydata$prob <- fitted(log.model)
mydata$logit <- log(mydata$prob / (1 - mydata$prob))

# Plot logit vs predictor
plot(mydata$Legalized, mydata$logit)
abline(lm(logit ~ Legalized, data = mydata), col = "red")

#idk... this looks good(?)

par(mfrow=c(2,2))
plot(log.model)

#130 is a major outlier
#Across all four plots, observation #130 is flagged consistently:
# 
# ✅ 1. Residuals vs Fitted
# Shows that #130 has a very large residual — indicating poor model fit for this point.
# 
# It's clearly away from the rest, which is a red flag.
# 
# ✅ 2. Q-Q Plot
# It deviates heavily from the theoretical line — showing it's not following the expected distribution.
# 
# Suggests non-normal residuals, particularly due to this outlier.
# 
# ✅ 3. Scale-Location Plot
# Confirms #130 has higher standardized residual variance, indicating it's not just extreme — it's unstable.
# 
# Residuals seem to fan out, which also suggests heteroscedasticity (non-constant variance).
# 
# ✅ 4. Residuals vs Leverage
# This is the most important plot for deciding influence.

#130 is near the Cook’s distance lines and has moderate leverage — this means it could be influencing the model quite a bit.


#this is switzerland! legalized in 2022
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

# Assign indices to folds
for(i in 1:K) {
  if(i < K) {
    index.fold[[i]] <- n.shuffle[((i - 1) * n.fold + 1):(i * n.fold)]
  } else {
    index.fold[[i]] <- n.shuffle[((K - 1) * n.fold + 1):n]
  }
}

# Define a range of cost values to try
cost.values <- c(0.01, 0.1, 1, 10, 100)

# Create empty vector to store misclassification rates
misclassification_rates <- numeric(length(cost.values))
sensitivity_rates <- numeric(length(cost.values))
specificity_rates <- numeric(length(cost.values))


# Loop over each cost value
for (j in 1:length(cost.values)) {
  
  TP <- TN <- FP <- FN <- 0
  
  for(i in 1:K) {
    # Training data (excluding fold i)
    train_data <- mydata[-index.fold[[i]], ]
    test_data <- mydata[index.fold[[i]], ]
    
    # Fit SVM with different cost
    svm.fit <- svm(Legalized ~ ., data = train_data, kernel = "radial", cost = cost.values[j], scale = TRUE)
    
    # Predict
    svm.prob <- predict(svm.fit, newdata = test_data)
    
    # Classify
    pred <- ifelse(svm.prob >= 0.5, 1, 0)
    actual <- test_data$Legalized
    
    # Update confusion matrix values
    TP <- TP + sum(pred == 1 & actual == 1)
    TN <- TN + sum(pred == 0 & actual == 0)
    FP <- FP + sum(pred == 1 & actual == 0)
    FN <- FN + sum(pred == 0 & actual == 1)
  }
  
  # Calculate misclassification rate for this cost
  misclassification_rates[j] <- (FP + FN) / n
  #caluclate sensitivity
  sensitivity[j] <- TP/ (TP+FN)
  sensitivity[j] <- TP / (TP + FN)
  specificity[j] <- TN / (TN + FP)
  
}

# Print each cost and corresponding misclassification rate
for (j in 1:length(cost.values)) {
  cat("Cost:", cost.values[j], "- Misclassification Rate:", round(misclassification_rates[j], 4), "\n")
}
for (j in 1:length(cost.values)) {
  cat("Cost:", cost.values[j], "- Sensitivity:", round(sensitivity[j], 4), "\n")
}
for (j in 1:length(cost.values)) {
  cat("Cost:", cost.values[j], "- Specificity:", round(specificity[j], 4), "\n")
}




# Find the best cost
best_cost <- cost.values[which.min(misclassification_rates)]

# Print results
for (j in 1:length(cost.values)) {
  cat("Cost:", cost.values[j], "- Misclassification Rate:", misclassification_rates[j], "\n")
}

cat("\nBest Cost:", best_cost, "with Misclassification Rate:", min(misclassification_rates), "\n")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
