# Lab 06 LIME feature selection
library(pander)
library(knitr)
library(caret)

iris <- iris
iris$class <- as.numeric(iris$Species == "setosa")
outcome <- 'Species'
pos <- 1

set.seed(12345)
intrain <- runif(nrow(iris)) < 0.75
train <- iris[intrain,]
test <- iris[!intrain,]
head(train)
##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species class
## 1          5.1         3.5          1.4         0.2  setosa     1
## 5          5.0         3.6          1.4         0.2  setosa     1
## 6          5.4         3.9          1.7         0.4  setosa     1
## 7          4.6         3.4          1.4         0.3  setosa     1
## 8          5.0         3.4          1.5         0.2  setosa     1
## 9          4.4         2.9          1.4         0.2  setosa     1
cat("Training set size is", dim(train))
## Training set size is 107 6
cat("Test set size is", dim(test))
## Test set size is 43 6

# variables holding truth
responses <- colnames(iris) %in% c("Species", "class")
features <- colnames(iris)[!responses]

formula <- paste("class", paste(features, collapse=" + "), sep=" ~ ")

model_logr <- glm(formula=formula, data=train, family=binomial(link="logit"))
## Warning: glm.fit: algorithm did not converge
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
train$pred <- predict(model_logr, newdata=train, type="response")
test$pred <- predict(model_logr, newdata=test, type="response")


# ytrue should be a vector containing 1s (or TRUE) and 0s (or FALSE);
# ypred should be a vector containing the predicted probability values for the target class.
# Both ytrue and ypred should have the same length.
performanceMeasures <- function(ytrue, ypred, model.name = "model", threshold=0.5) {
  # compute the normalised deviance
  dev.norm <- -2 * logLikelihood(ytrue, ypred)/length(ypred)
  # compute the confusion matrix
  cmat <- table(actual = ytrue, predicted = ypred >= threshold)
  accuracy <- sum(diag(cmat)) / sum(cmat)
  precision <- cmat[2, 2] / sum(cmat[, 2])
  recall <- cmat[2, 2] / sum(cmat[2, ])
  f1 <- 2 * precision * recall / (precision + recall)
  data.frame(model = model.name, precision = precision, recall = recall, 
             f1 = f1, dev.norm = dev.norm)
}

# create confusion_matrix
confusion_matrix <- function(ytrue, ypred, threshold=0.5) {
  # compute the confusion matrix
  cmat <- table(actual = ytrue, predicted = ypred >= threshold)
  cmat
}

cases <- c(5,11,13,24,30)
print(test[cases, c("class","pred")])
##     class         predR
## 20      1 1.000000e+00
## 43      1 1.000000e+00
## 51      0 2.220446e-16
## 89      0 2.220446e-16
## 113     0 2.220446e-16


cm <- confusion_matrix(test$class, test$pred, 0.5)



