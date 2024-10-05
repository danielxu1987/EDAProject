iris <- iris
iris$class <- as.numeric(iris$Species == "setosa")
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

responses <- colnames(iris) %in% c("Species", "class")
features <- colnames(iris)[!responses]

formula <- paste("class", paste(features, collapse=" + "), sep=" ~ ")

formula 
## [1] "class ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width"

model_logr <- glm(formula=formula, data=train, family=binomial(link="logit"))
## Warning: glm.fit: algorithm did not converge
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
train$pred <- predict(model_logr, newdata=train, type="response")
test$pred <- predict(model_logr, newdata=test, type="response")


performanceMeasures <- function(pred, truth, name = "model") {
  ctable <- table(truth = truth, pred = (pred > 0.5))
  accuracy <- sum(diag(ctable)) / sum(ctable)
  precision <- ctable[2, 2] / sum(ctable[, 2])
  recall <- ctable[2, 2] / sum(ctable[2, ])
  f1 <- 2 * precision * recall / (precision + recall)
  data.frame(model = name, precision = precision,
             recall = recall,
             f1 = f1, accuracy = accuracy)
}

pretty_perf_table <- function(model,training,test) {
  library(pander)
  # setting up Pander Options
  panderOptions("plain.ascii", TRUE)
  panderOptions("keep.trailing.zeros", TRUE)
  panderOptions("table.style", "simple")
  perf_justify <- "lrrrr"
  # comparing performance on training vs. test
  pred_train <- predict(model, newdata=training)
  truth_train <- training[, "class"]
  pred_test <- predict(model, newdata=test)
  truth_test <- test[, "class"]
  trainperf_tree <- performanceMeasures(
    pred_train, truth_train, "logistic, training")
  testperf_tree <- performanceMeasures(
    pred_test, truth_test, "logistic, test")
  perftable <- rbind(trainperf_tree, testperf_tree)
  pandoc.table(perftable, justify = perf_justify)
}

pretty_perf_table(model_logr, train, test)
## 
## 
## model                  precision   recall   f1   accuracy
## -------------------- ----------- -------- ---- ----------
## logistic, training             1        1    1          1
## logistic, test                 1        1    1          1

library(knitr)

cases <- c(5,11,13,24,30)
test[cases, c("class","pred")]
##     class         predR
## 20      1 1.000000e+00
## 43      1 1.000000e+00
## 51      0 2.220446e-16
## 89      0 2.220446e-16
## 113     0 2.220446e-16

confusion_matrix(test$class, test$pred > 0.5)
##      prediction
## truth FALSE TRUE
##     0    31    0
##     1     0   12

# if needed, install the library xgboost
# install.packages("xgboost")
library(xgboost)

# Input:
# - variable_matrix: matrix of input data
# - labelvec: numeric vector of class labels (1 is positive class)
#
# Returns:
# - xgboost modelR
fit_iris_example = function(variable_matrix, labelvec) {
  cv <- xgb.cv(variable_matrix, label = labelvec,
               params=list(
                 objective="binary:logistic"
               ),
               nfold=5,
               nrounds=100,
               print_every_n=10,
               metrics="logloss")
  
  evalframe <- as.data.frame(cv$evaluation_log)
  NROUNDS <- which.min(evalframe$test_logloss_mean)
  
  model <- xgboost(data=variable_matrix, label=labelvec,
                   params=list(
                     objective="binary:logistic"
                   ),
                   nrounds=NROUNDS,
                   verbose=FALSE)
  
  model
}

# Fit the iris dataset using xgboost:
input <- as.matrix(train[features])
model <- fit_iris_example(input, train$class)
## [1]  train-logloss:0.455851+0.000089 test-logloss:0.455942+0.000886 
## [11] train-logloss:0.033041+0.000072 test-logloss:0.033115+0.000717 
## [21] train-logloss:0.023723+0.000067 test-logloss:0.023887+0.001041 
## [31] train-logloss:0.023715+0.000065 test-logloss:0.023889+0.001136 
## [41] train-logloss:0.023715+0.000065 test-logloss:0.023890+0.001147 
## [51] train-logloss:0.023715+0.000065 test-logloss:0.023890+0.001148 
## [61] train-logloss:0.023715+0.000065 test-logloss:0.023890+0.001148 
## [71] train-logloss:0.023715+0.000065 test-logloss:0.023890+0.001148 
## [81] train-logloss:0.023715+0.000065 test-logloss:0.023890+0.001148 
## [91] train-logloss:0.023715+0.000065 test-logloss:0.023890+0.001148 
## [100]    train-logloss:0.023715+0.000065 test-logloss:0.023890+0.001148

library(lime)
explainer <- lime(train[features], model = model, 
                  bin_continuous = TRUE, n_bins = 10)


cases <- c(3,11,21,30)
(example <- test[cases,features])
##     Sepal.Length Sepal.Width Petal.Length Petal.Width
## 4            4.6         3.1          1.5         0.2
## 43           4.4         3.2          1.3         0.2
## 77           6.8         2.8          4.8         1.4
## 113          6.8         3.0          5.5         2.1
explanation <- lime::explain(example, explainer, n_labels = 1, n_features = 4)
plot_features(explanation)