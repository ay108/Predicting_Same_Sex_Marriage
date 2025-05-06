library(boot)
library(pROC)
library(e1071)
library(tidyverse)
## Data Cleaning Part 1
## Converting colonization years dataset
# Read in colonization dataset
colonization <- read.csv("/Users/vivianhuang/Desktop/STAT 228/final project/dataverse_files/COLDAT_colonies.csv", header = TRUE)
nrow(colonization)

# Encode into binary categorical
colonization$Colonized <- ifelse((rowSums(colonization[,c(2:9)], na.rm = TRUE)) >= 1, "1", "0")

# Only include needed columns
colonization <- colonization[,c(1,43)]

# Write to new csv 
write.csv(colonization, file = "/Users/vivianhuang/Desktop/STAT 228/final project/EuropeanColonized.csv")


# New Colonization dataset
new.colonization <- read.csv("/Users/vivianhuang/Desktop/STAT 228/final project/EuropeanColonized.csv", header = TRUE)
names(new.colonization)[names(new.colonization) == "country"] <- "Country"
new.colonization <- new.colonization[,-1]

# Read in religion csv
religion <- read.csv("/Users/vivianhuang/Desktop/STAT 228/final project/PewResearchPercentage.csv", header = TRUE)
subset <- religion[,-c(1:6)]

# Explore to see which religions are most prominent
colMeans(subset)
which.max(apply(subset, 2, max))
max_counts <- apply(subset, 1, which.max)       
table(names(subset)[max_counts])                


# Final Datasets
new.religion <- read.csv("/Users/vivianhuang/Desktop/STAT 228/final project/Pew Research Religion Percentages by Country - rounded_percentage.csv")
rel.colonize <- merge(new.colonization, new.religion, by = "Country", all = T)

# Read in Gay marriage dataset
gay <- read.csv("/Users/vivianhuang/Desktop/STAT 228/final project/LegalizedGayMarriage.csv", header = TRUE)
head(gay)
names(gay)[names(gay) == "Countries"] <- "Country"

# Merge religion, gay, and colonization datasets
rel.col.gay <- merge(rel.colonize, gay, by = "Country", all = T)

# Write combined dataset to new csv
write.csv(rel.col.gay, file = "/Users/vivianhuang/Desktop/STAT 228/final project/GayReligionColMerged.csv")
newdata <- read.csv("/Users/vivianhuang/Desktop/STAT 228/final project/ogeverything.csv", header = TRUE)

# Manually encode some missing data
newdata$Legalized[newdata$Country.Name == 'Chile'] <- 0
newdata$Legalized[newdata$Country.Name == 'Cuba'] <- 0
newdata$Legalized[newdata$Country.Name == 'Slovenia'] <- 0

newdata <- newdata[-30,]
write.csv(newdata, file = "/Users/vivianhuang/Desktop/STAT 228/final project/everything.csv", row.names = FALSE)

## Testing for multicollinearity

data <- read.csv("/Users/vivianhuang/Desktop/STAT 228/final project/everything_cleaned.csv", header = TRUE)
data <- data[,-1]
pairs(data[,-c(1,2,12)])


library("usdm")

quant.data <- data[,-c(1,2,11,12)]
pred.data <- data[,-c(1,2,12)]
vifstep(pred.data[,-2], th = 10)
```

## Basic decision tree 
set.seed(1) 

# Split the data
id.train = sample(1:147,118,replace=FALSE) # Approximately 80%

gay.train = data[id.train,] 
gay.validate = data[-id.train,] 

#install.packages("tree")
library("tree")

gay.tree <- tree(Legalized~Education.Expenditure....GDP. +
                   Health.Expenditure....GDP. + GDP.Per.Capita +
                   Democracy.score + Buddhists + Christians + 
                   Muslims + ht_colonial, data = data)
plot(gay.tree)
text(gay.tree, pretty = 0)

prune <- cv.tree(gay.tree,K=10,FUN=prune.tree)
plot(prune)

library("MASS")

set.seed(1)
n <- nrow(data) 
K <- 5 
n.fold <- floor(n/K) 
n.shuffle <- sample(1:n, n, replace=FALSE) #shuffle the n indexes 
index.fold <- list() 

for(i in 1:K) 
{ 
  if(i<K) 
  { 
    index.fold[[i]] <- n.shuffle[((i-1)*n.fold+1):(i*n.fold)] 
  }else 
  { 
    index.fold[[i]] <- n.shuffle[((K-1)*n.fold+1):n]  
  } }

misclassification.scoret <- 0 
sensitivityt <- 0
specificityt <- 0
for(i in 1:K) { #fit the full model based on the data excluding the ith fold 
  gay.tree <- tree(Legalized~Education.Expenditure....GDP. +
                     Health.Expenditure....GDP. + GDP.Per.Capita +
                     Democracy.score + Buddhists + Christians + 
                     Muslims + ht_colonial, data = data[-index.fold[[i]],])
  
  newgay.tree <- prune.tree(gay.tree, best = 2)
  
  # Predict based on validation data 
  pred <- predict(newgay.tree, newdata = data[index.fold[[i]],]) 
  predyes <- ifelse(pred>0.3,'1','0')  
  conf.mat1 <- table(predyes,data[index.fold[[i]],]$Legalized)
  
  # Create confusion matrix
  misclassification.scoret <- misclassification.scoret +  (conf.mat1[1,2]+conf.mat1[2,1])/(conf.mat1[1,2]+conf.mat1[2,1]+conf.mat1[1,1]+conf.mat1[2,2])
  sensitivityt <- sensitivityt + (conf.mat1[2,2])/(conf.mat1[2,2]+conf.mat1[1,2])
  specificityt <- specificityt + (conf.mat1[1,1])/(conf.mat1[1,1]+conf.mat1[2,1])
}

misclassification.scoret/5
sensitivityt/5
specificityt/5
# 7.408% misclassification rate
# 78.095% sensitivity rate
# 96.697% Specificity rate


# Random Forest

set.seed(1)
n <- nrow(data) 
K <- 5 #5-fold CV as an example 
n.fold <- floor(n/K) #size of each fold, rounded down to the nearest integer (so the last fold might be larger in size) 
n.shuffle <- sample(1:n, n, replace=FALSE) #shuffle the n indexes 
index.fold <- list() 

for(i in 1:K) 
{ 
  if(i<K) 
  { 
    index.fold[[i]] <- n.shuffle[((i-1)*n.fold+1):(i*n.fold)] 
  }else 
  { 
    index.fold[[i]] <- n.shuffle[((K-1)*n.fold+1):n]  
  } }

misclassification.score1 <- 0 
sensitivity1 <- 0
specificity1 <- 0
for(i in 1:K) 
{ 
  library("randomForest")
  gay.forest <- randomForest(Legalized~Education.Expenditure....GDP. +
                               Health.Expenditure....GDP. + GDP.Per.Capita +
                               Democracy.score + Buddhists + Christians + 
                               Muslims + ht_colonial, data = data[-index.fold[[i]],])
  pred.forest <- predict(gay.forest, newdata = data[index.fold[[i]],], type = "response")
  pred.forestyes <- ifelse(pred.forest>0.3, '1', '0')
  conf.mat2 <- table(pred.forestyes,data[index.fold[[i]],]$Legalized)
  
  misclassification.score1 <- misclassification.score1 +  (conf.mat2[1,2]+conf.mat2[2,1])/(conf.mat2[1,2]+conf.mat2[2,1]+conf.mat2[1,1]+conf.mat2[2,2])
  sensitivity1 <- sensitivity1 + (conf.mat2[2,2])/(conf.mat2[2,2]+conf.mat2[1,2])
  specificity1 <- specificity1 + (conf.mat2[1,1])/(conf.mat2[1,1]+conf.mat2[2,1])
}

misclassification.score1/5
sensitivity1/5
specificity1/5
# 6.719% misclassification rate
# 90.952% sensitivity rate
# 94.125% Specificity rate


misclassification.score2 <- 0 
sensitivity2 <- 0
specificity2 <- 0
for(i in 1:K) 
{ 
  # Bagging
  gay.bag <- randomForest(Legalized~Education.Expenditure....GDP. +
                            Health.Expenditure....GDP. + GDP.Per.Capita +
                            Democracy.score + Buddhists + Christians + 
                            Muslims + ht_colonial, data = data[-index.fold[[i]],])
  pred.bag <- predict(gay.bag, newdata = data[index.fold[[i]],], type = "response", mtry = 8)
  pred.bagyes <- ifelse(pred.bag>0.3, '1', '0')
  
  # Create confusion matrix
  conf.mat3 <- table(pred.bagyes,data[index.fold[[i]],]$Legalized) 
  
  # Calculate misclassification, sensitivity, and specificity
  misclassification.score2 <- misclassification.score2 +  (conf.mat3[1,2]+conf.mat3[2,1])/(conf.mat3[1,2]+conf.mat3[2,1]+conf.mat3[1,1]+conf.mat3[2,2])
  sensitivity2 <- sensitivity2 + (conf.mat3[2,2])/(conf.mat3[2,2]+conf.mat3[1,2])
  specificity2 <- specificity2 + (conf.mat3[1,1])/(conf.mat3[1,1]+conf.mat3[2,1])
  
}

misclassification.score2/5
sensitivity2/5
specificity2/5
# 6.719% misclassification rate
# 90.952% sensitivity rate
# 94.125% Specificity rate


## Linear discriminant Analysis
library("MASS")

set.seed(1)
n <- nrow(data) 
K <- 5 #5-fold CV as an example 
n.fold <- floor(n/K) #size of each fold, rounded down to the nearest integer (so the last fold might be larger in size) 
n.shuffle <- sample(1:n, n, replace=FALSE) #shuffle the n indexes 
index.fold <- list() 

for(i in 1:K) 
{ 
  if(i<K) 
  { 
    index.fold[[i]] <- n.shuffle[((i-1)*n.fold+1):(i*n.fold)] 
  }else 
  { 
    index.fold[[i]] <- n.shuffle[((K-1)*n.fold+1):n]  
  } }

misclassification.score <- 0 
sensitivity <- 0
specificity <- 0
for(i in 1:K) 
{ #fit the full model based on the data excluding the ith fold 
  fit.lda <- lda(Legalized~Education.Expenditure....GDP. +
                   Health.Expenditure....GDP. + GDP.Per.Capita +
                   Democracy.score + Buddhists + Christians + 
                   Muslims + ht_colonial, data = data[-index.fold[[i]],])
  
  post.p <- predict(fit.lda, newdata = data[index.fold[[i]],])$posterior
  Y.hat <- ifelse(post.p[,2]>0.3, "1", "0")
  conf.lda <- table(Y.hat, data[index.fold[[i]],]$Legalized)
  
  misclassification.score <- misclassification.score +  (conf.lda[1,2]+conf.lda[2,1])/(conf.lda[1,2]+conf.lda[2,1]+conf.lda[1,1]+conf.lda[2,2])
  sensitivity <- sensitivity + (conf.lda[2,2])/(conf.lda[2,2]+conf.lda[1,2])
  specificity <- specificity + (conf.lda[1,1])/(conf.lda[1,1]+conf.lda[2,1])
}

misclassification.score/5 # 10.812% misclassification score
sensitivity/5 #81.429% sensitivity 
specificity/5 # 91.586% specificity 


# Testing for assumptions

# Testing for normality
variables <- c("Education.Expenditure....GDP.",
               "Health.Expenditure....GDP.", "GDP.Per.Capita",
               "Democracy.score", "Buddhists", "Christians",
               "Muslims", "ht_colonial")

par(mfrow = c(4,2))

for (i in variables){
  qqnorm(gay.train[[i]]) ; qqline(gay.train[[i]])
}
# Violates normality assumptions

# Testing variance
library("biotools")
boxM(gay.train[,-c(1,2,12)],group = gay.train$Legalized)
# Violates the equal variance assumption
```
democracy <- read.csv("~/Desktop/Stat228/democracy_index.csv")
#only want to keep year 2020

democracy_2020 <- democracy[democracy$Year=='2020',]
countries <- read.csv(("~/Desktop/Stat228/Countries.csv"))
countries_2020 <- countries[countries$Year=='2020',]
countries_2020 <- countries_2020[,c(1,2,3,6,8,9,12,15,21,24,25)]
countries_fuller <- merge(countries_2020, democracy_2020,
                          by.x = "Country.Name", by.y = "Entity",
                          all = TRUE)

countries_demo_clean <- countries_fuller[rowSums(is.na(countries_fuller)) <= 8, ]

countries_clean_demo <- countries_demo_clean[!is.na(countries_demo_clean$Democracy.score), ]
View(countries_clean_demo)

#get rid fo column year and code (dups)
countries_clean_demo <- countries_clean_demo[,-c(13,14)]
head(countries_clean_demo)
write.csv(countries_clean_demo,"~/Desktop/Stat228/countries_with_demo.csv", row.names = FALSE)


religion_country <- read.csv("~/Desktop/Stat228/GayReligionColMerged.csv")
head(religion_country)
religion_country <-religion_country[,c(2,4,5,6)]
head(religion_country)


everything_but_colonized <- merge(countries_clean_demo, religion_country,
                                  by.x = "Country.Name", by.y = "Country",
                                  all = TRUE)
View(everything_but_colonized)

everything_but_colonized_cleaned <- everything_but_colonized[rowSums(is.na(everything_but_colonized)) <= 10, ]

library(readxl)
colonized <- read_excel(file.choose())
head(colonized)
colonized_2020 <- colonized[colonized$year=='2020',]
View(colonized_2020)


colonized_2020$ht_colonial <- ifelse(colonized_2020$ht_colonial != 0, 1, 0)

View(colonized_2020)

everything <- merge(everything_but_colonized_cleaned, colonized_2020,
                    by.x = "Country.Name", by.y = "cname",
                    all = TRUE)

View(everything)
everything_cleaned <- everything[rowSums(is.na(everything)) <= 8, ]

View(everything_cleaned)

write.csv(everything_cleaned,"~/Desktop/Stat228/countries_everything_cleaned.csv", row.names = FALSE)


everything_cleaned <- everything_cleaned[,c(1,2,5,6,7,11,13,14,15,16,18)]

View(everything_cleaned)

everything <- merge(everything_cleaned, religion_country[, c("Country", "Legalized")],
                    by.x = "Country.Name", by.y = "Country", all.x = TRUE)

View(everything)

write.excel(everything,"~/Desktop/Stat228/everything.", row.names = FALSE)


mydata <- read.csv("/Users/ashleyyang/Desktop/Stat228/everything_cleaned.csv")
head(mydata)
mydata <- mydata[,c(4,6,7,8,9,10,11,12,13)]

set.seed(100)

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
plot(log.model)


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
