# code for classfication only

library(knitr)
library(dplyr)
library(rpart)
library(ROCR)

# load dataset
df_original <- read.csv("./Countries and death causes.csv",header = T, sep=",")

numericVars <- names(df_original[, -c(1:3)])
df <- df_original[, -c(2)]

# convert entity and year to factors
df$Entity <- as.factor(df$Entity)
df$Year <- as.factor(df$Year)

dietFeatureVars <- c('Diet.low.in.whole.grains', 'Diet.low.in.fruits',
                     'Diet.low.in.Vegetables', 'Diet.low.in.nuts.and.seeds')

countries <- unique(df$Entity)
years <- 1990:2019

#df_v2 <- df_num
df$total.mortality <- rowSums(df[numericVars])

for (col in numericVars) {
  df[paste0(col, ".rate")] <- df[col] / df['total.mortality']
}

df <- df |>
  mutate(
    fruit.over.median = (Diet.low.in.fruits.rate > median(Diet.low.in.fruits.rate)), # indicate if death count of low fruit consumption is over its median
    vege.over.median = (Diet.low.in.Vegetables.rate > median(Diet.low.in.Vegetables.rate)), # indicate if death count of low vegetable consumption is over its median
    grain.over.median = (Diet.low.in.whole.grains.rate > median(Diet.low.in.whole.grains.rate)),
    nutseed.over.median = (Diet.low.in.nuts.and.seeds.rate > median(Diet.low.in.nuts.and.seeds.rate)),
    sodium.over.median = (Diet.high.in.sodium.rate > median(Diet.high.in.sodium.rate)) # flag if death count of high sodium consumption is over its median
  )

outcome <- c('sodium.over.median')
pos <- TRUE # positive value indicator


d <- df

set.seed(729375)
d$rgroup <- runif(dim(d)[1])
dTrainAll <- subset(d, rgroup<=0.9)
dTest <- subset(d, rgroup>0.9)

# names of columns that are categorical type and numerical type
vars <- setdiff(colnames(dTrainAll), c(outcome, 'rgroup'))
catVars <- vars[sapply(df[,vars], class) %in% c('character', 'logical', 'factor')]
numericVars <- vars[sapply(df[,vars], class) %in% c('numeric','integer')]


# split dTrainAll into a training set and a validation (or calibration) set
useForCal <- rbinom(n=dim(dTrainAll)[1], size=1, prob=0.1)>0
dCal <- subset(dTrainAll, useForCal)
dTrain <- subset(dTrainAll, !useForCal)

mkPredC <- function(outCol, varCol, appCol) {
  pPos <- sum(outCol==pos)/length(outCol)
  naTab <- table(as.factor(outCol[is.na(varCol)]))
  pPosWna <- (naTab/sum(naTab))[pos]
  vTab <- table(as.factor(outCol), varCol)
  pPosWv <- (vTab[pos,]+1.0e-3*pPos)/(colSums(vTab)+1.0e-3)
  pred <- pPosWv[appCol]
  pred[is.na(appCol)] <- pPosWna
  pred[is.na(pred)] <- pPos
  pred
}

# now go through all the categorical variables in the `catVars` vector
# and perform the predictions. The outputs are stored back into the
# data frame.
for (v in catVars) {
  pi <- paste('pred.', v, sep='')
  dTrain[,pi] <- mkPredC(dTrain[,outcome], dTrain[,v], dTrain[,v])
  dCal[,pi] <- mkPredC(dTrain[,outcome], dTrain[,v], dCal[,v])
  dTest[,pi] <- mkPredC(dTrain[,outcome], dTrain[,v], dTest[,v])
}

calcAUC <- function(ypred, ytrue) {
  perf <- performance(prediction(ypred, ytrue), 'auc')
  as.numeric(perf@y.values)
}

# Define a function to compute log likelihood so that we can reuse it.
logLikelihood <- function(ypred, ytrue) {
  sum(ifelse(ytrue, log(ypred), log(1-ypred)), na.rm=T)
}

# Compute the likelihood of the Null model on the calibration
# set (for the KDD dataset from previous lecture)
logNull <- logLikelihood(sum(dCal[,outcome]==pos)/nrow(dCal), dCal[,outcome]==pos)

cat("The log likelihood of the Null model is:", logNull)

## The log likelihood of the Null model is: -1178.017, ours is -424.2028

# selCatVars is a vector that keeps the names of the top performing categorical variables.
selCatVars <- c()
minDrop <- 0  # may need to adjust this number

for (v in catVars) {
  pi <- paste('pred.', v, sep='')
  devDrop <- 2*(logLikelihood(dCal[,pi], dCal[,outcome]==pos) - logNull)
  if (devDrop >= minDrop) {
    cat(sprintf("%6s, deviance reduction: %g\n", v, devDrop))
    selCatVars <- c(selCatVars, pi)
  }
}

# fruit.over.median, deviance reduction: 0.586775

# for numerical variables, we convert them into categorical one and
# call the `mkPredC` function above.
mkPredN <- function(outCol, varCol, appCol) {
  cuts <- unique(as.numeric(
    quantile(varCol, probs=seq(0, 1, 0.1), na.rm=T)))
  varC <- cut(varCol, cuts)
  appC <- cut(appCol, cuts)
  mkPredC(outCol, varC, appC)
}

# get all percentage / rate variables 
rateVars <- grep("\\.rate$", numericVars, value = TRUE)

# now go through all the diet related numerical variables in the `numericVars` vector
# and perform the predictions. Again, the outputs are stored back into
# the data frame.
for (v in rateVars) {
  pi <- paste('pred.', v, sep='')
  dTrain[,pi] <- mkPredN(dTrain[,outcome], dTrain[,v], dTrain[,v])
  dTest[,pi] <- mkPredN(dTrain[,outcome], dTrain[,v], dTest[,v])
  dCal[,pi] <- mkPredN(dTrain[,outcome], dTrain[,v], dCal[,v])
}


# fruit.over.median, deviance reduction: 0.501294

# selNumVars is a vector that keeps the names of the top performing numerical variables.
selNumVars <- c()
minDrop <- 0  # may need to adjust this number
for (v in rateVars) {
  pi <- paste('pred.', v, sep='')
  devDrop <- 2*(logLikelihood(dCal[,pi], dCal[,outcome]==pos) - logNull)
  if (devDrop >= minDrop) {
    cat(sprintf("%6s, deviance reduction: %g\n", v, devDrop))
    selNumVars <- c(selNumVars, pi)
  }
}

selVars <- c(selCatVars, selNumVars)

cat("Performance of the top performing single variables on the test set:")
## Performance of the top performing single variables on the test set:
for (v in selVars) {
  # retrieve the original variable name (character location 5 onward)
  orig_v <- substring(v, 5)
  cat(sprintf("Variable %6s: AUC = %g\n", orig_v, calcAUC(dTest[,v], dTest[,outcome]==pos)))
}

for(v in catVars) {
  pi <- paste('pred.', v, sep='')
  aucTrain <- calcAUC(dTrain[,pi], dTrain[,outcome])
  if (aucTrain >= 0.5) {
    aucCal <- calcAUC(dCal[,pi], dCal[,outcome])
    print(sprintf(
      "%s: trainAUC: %4.3f; calibrationAUC: %4.3f",
      pi, aucTrain, aucCal))
  }
}