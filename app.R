# This is the shiny app to assist data exploration for EDA project in unit CITS4009
# Made by Sifeng Xu, 24525844

# Load  libraries
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(gridExtra)
library(knitr)
library(reshape2)
library(ROCit)


# load dataset
df_original <- read.csv("./Countries and death causes.csv", header = T, sep=",")

colnames(df_original)[1] <- "Country"

num_vars <- sort(names(df_original[, -c(1:3)]))
countries <- unique(df_original$Country)
years <- 1990:2019
# remove regions groups without a valid Code
df <- df_original[df_original$Code != '', ]

dataUK <- df[df['Country']=='United Kingdom',] #df[df['Country']=='China',]
dataTAN <- df[df['Country']=='Tanzania',]
dataNLD <- df[df['Country']=='Netherlands',]
dataCAM <- df[df['Country']=='Cambodia',]


# merge total population 
popl <- read.table('./API_SP.POP.TOTL_DS2_en_csv_v2_31753/API_SP.POP.TOTL_DS2_en_csv_v2_31753.csv',header=T, sep=',')
colnames(popl) <- gsub("^X", "", colnames(popl))

popl <- popl[, c('Country.Code', years)]
popl_df <- melt(popl, id.vars = "Country.Code", variable.name = "Year", value.name = "total.population")
popl_df$Year <- as.numeric(as.character(popl_df$Year))
df_merged <- merge(df, popl_df, by.x = c("Code", "Year"), by.y = c("Country.Code", "Year"), all.x = TRUE)

df_merged <- df_merged[, c(1:3, ncol(df_merged)-1, ncol(df_merged), 4:(ncol(df_merged)-2))]

nacount <- count_nas(df_merged)
nas_df_merged <- df_merged[is.na(df_merged$total.population),]
# nas_df_merged[sample(nrow(nas_df_merged), 6), 1:5]

# remove NA's in df_merged
df_merged <- setdiff(df_merged, nas_df_merged)


df_merged$Country <- as.factor(df_merged$Country)
df_merged$Year <- as.factor(df_merged$Year)

df_merged$total.mortality <- rowSums(df_merged[num_vars])

for (col in num_vars) {
  df_merged[paste0(col, ".rate")] <- df_merged[col] / df_merged['total.mortality']
}

rateVars <- grep("\\.rate$", names(df_merged), value = TRUE)

figRateAll <- ggplot(stack(df_merged[rateVars])) + geom_boxplot(mapping = aes(x = ind, y = values)) + labs(x ='', y='Contribution of causes to total mortality') + coord_flip() 
# count the death rate of all causes with respect to the total population
df_merged$Death.rate <- (df_merged$total.mortality / df_merged$total.population)

df_merged$Code <- NULL

outcome <- c('Death.rate.high') # Smoking.kills.more, sodium.over.median
pos <- TRUE # positive value 

df_merged <- df_merged |>
  mutate(
    Death.rate.high = (Death.rate > median(Death.rate)),
    Unsafe.sex.over.median = (Unsafe.sex.rate > median(Unsafe.sex.rate)),
    Smoking.over.median = (Smoking.rate > median(Smoking.rate)),
    Blood.pressure.over.median = (High.systolic.blood.pressure.rate < median(High.systolic.blood.pressure.rate))
  )

d <- df_merged

set.seed(729375)
d$rgroup <- runif(dim(d)[1])
dTrainAll <- subset(d, rgroup<=0.9)
dTest <- subset(d, rgroup>0.9)

# split dTrainAll into a training set and a validation (or calibration) set
useForCal <- rbinom(n=dim(dTrainAll)[1], size=1, prob=0.1)>0
dCal <- subset(dTrainAll, useForCal)
dTrain <- subset(dTrainAll, !useForCal)

# names of columns that are categorical type and numerical type
vars <- setdiff(colnames(dTrainAll), c(outcome, 'rgroup'))
catVars <- vars[sapply(d[,vars], class) %in% c('character', 'logical',  'factor')]

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
# and perform the predictions. 
for (v in catVars) {
  pi <- paste('pred.', v, sep='')
  dTrain[,pi] <- mkPredC(dTrain[,outcome], dTrain[,v], dTrain[,v])
  dCal[,pi] <- mkPredC(dTrain[,outcome], dTrain[,v], dCal[,v])
  dTest[,pi] <- mkPredC(dTrain[,outcome], dTrain[,v], dTest[,v])
}

# Define a function to compute log likelihood so that we can reuse it.
logLikelihood <- function(ypred, ytrue) {
  sum(ifelse(ytrue, log(ypred), log(1-ypred)), na.rm=T)
}

# Compute the likelihood of the Null model on the calibration
pred.Null <- sum(dTrain[,outcome]==pos)/nrow(dTrain)
logNull <- logLikelihood(sum(dCal[,outcome]==pos)/nrow(dCal), dCal[,outcome]==pos)
cat("The log likelihood of the Null model is:", logNull)

selCatVars <- c()
minDrop <- -3

for (v in catVars) {
  pi <- paste('pred.', v, sep='')
  logPred <- logLikelihood(dCal[,pi], dCal[,outcome]==pos)
  cat(sprintf("%6s, log likelihood: %g\n", v, logPred))
  devDrop <- 2*(logPred - logNull)
  if (devDrop >= minDrop) {
    cat(sprintf("%6s, deviance reduction: %g\n", v, devDrop))
    selCatVars <- c(selCatVars, pi)
  }
}

devDrop <- 2*(logLikelihood(dCal[,outcome], dCal[,outcome]==pos) - logNull)
cat(sprintf("To compare, deviance reduction of the variable that we want to predict %6s is: %g\n", outcome, devDrop))



# colour_id 1-7 are: black,red,green,blue,cyan,purple,gold
plot_roc <- function(predcol, outcol, colour_id=2, overlaid=F) {
  ROCit_obj <- rocit(score=predcol, class=outcol==pos)
  par(new=overlaid)
  plot(ROCit_obj, col = c(colour_id, 1),
       legend = TRUE, YIndex = FALSE, values = FALSE)
}


# plot_roc(dCal[[selCatVars[1]]], dCal[,outcome]) #First variable is red
# plot_roc(dCal[[selCatVars[2]]], dCal[,outcome], colour_id=3, overlaid=T) # Second variable is green
# plot_roc(dCal[[selCatVars[3]]], dCal[,outcome], colour_id=4, overlaid=T)

numericVars <- vars[sapply(d[,vars], class) %in% c('numeric','integer')]

# get all percentage / rate variables 
rateVars <- grep("\\.rate$", numericVars, value = TRUE)

mkPredN <- function(outCol, varCol, appCol) {
  cuts <- unique(as.numeric(
    quantile(varCol, probs=seq(0, 1, 0.1), na.rm=T)))
  varC <- cut(varCol, cuts)
  appC <- cut(appCol, cuts)
  mkPredC(outCol, varC, appC)
}

# now go through all numerical variables in the `numericVars` vector and perform the predictions.
for (v in numericVars) {
  pi <- paste('pred.', v, sep='')
  dTrain[,pi] <- mkPredN(dTrain[,outcome], dTrain[,v], dTrain[,v])
  dTest[,pi] <- mkPredN(dTrain[,outcome], dTrain[,v], dTest[,v])
  dCal[,pi] <- mkPredN(dTrain[,outcome], dTrain[,v], dCal[,v])
}

# selNumVars is a vector that keeps the names of the top performing numerical variables.
selNumVars <- c()
minDrop <- 0

for (v in numericVars) {
  pi <- paste('pred.', v, sep='')
  logPred <- logLikelihood(dCal[,pi], dCal[,outcome]==pos)
  
  devDrop <- 2*(logPred - logNull)
  if (devDrop >= minDrop) {
    cat(sprintf("%6s, deviance reduction: %g\n", v, devDrop))
    selNumVars <- c(selNumVars, pi)
  }
}

library(ROCR)
calcAUC <- function(ypred, ytrue) {
  perf <- performance(prediction(ypred, ytrue), 'auc')
  as.numeric(perf@y.values)
}

# selVars <- c(selCatVars, selNumVars)
selVars <- c()

## Performance of the top performing single variables on the test set:
for(v in c(selCatVars, selNumVars)) {
  pi <- v
  aucTrain <- calcAUC(dTrain[,pi], dTrain[,outcome])
  if (aucTrain > 0.52) {
    selVars <- c(selVars, pi)
    aucCal <- calcAUC(dCal[,pi], dCal[,outcome])
    print(sprintf(
      "%s: trainAUC: %4.3f; calibrationAUC: %4.3f",
      pi, aucTrain, aucCal))
  } 
}

# Run 100-fold cross validation
original_vars <- character() #sub("^pred\\.", "", selVars)
# cat("Performance of the top performing single variables on the test set:")

for (v in selVars) {
  # retrieve the original variable name (character location 6 onward)
  orig_v <- substring(v, 6)
  original_vars <- c(original_vars, orig_v)
  cat(sprintf("In dTest Variable %6s: AUC = %g\n", orig_v, calcAUC(dTest[,v], dTest[,outcome]==pos)))
}

for (var in original_vars) {
  aucs <- rep(0,100)
  for (rep in 1:length(aucs)) {
    useForCalRep <- rbinom(n=nrow(dTrainAll), size=1, prob=0.1) > 0
    predRep <- mkPredC(dTrainAll[!useForCalRep, outcome],
                       dTrainAll[!useForCalRep, var],
                       dTrainAll[useForCalRep, var])
    aucs[rep] <- calcAUC(predRep, dTrainAll[useForCalRep, outcome])
  }
}

# # colour_id 1-7 are: black,red,green,blue,cyan,purple,gold
# plot_roc(dCal$pred.total.population, dCal[,outcome], colour_id=3) # population is green
# plot_roc(dCal$pred.Unsafe.sanitation, dCal[,outcome], colour_id=4, overlaid=T) #  sanitation is blue

# fig1 <- ggplot(dCal) + geom_density(aes(x=pred.total.population, color=as.factor(Death.rate.high)))
# fig2 <- ggplot(dCal) + geom_density(aes(x=pred.Unsafe.sanitation, color=as.factor(Death.rate.high)))
# grid.arrange(fig1, fig2, ncol=1)


# -------------------- Clustering---------
set.seed(123)
uCons <- unique(df_merged$Country)

clusterSet <- df_merged[df_merged$Year == 2000, ] 

clusterSet <- clusterSet[sample(length(uCons), size = 25, replace = F), ]

vars.to.use <- c('Diet.low.in.whole.grains.rate', 'Diet.low.in.fruits.rate', 'Diet.low.in.Vegetables.rate', 'Diet.low.in.nuts.and.seeds.rate', 'Diet.high.in.sodium.rate')

scaled_df <- scale(clusterSet[, vars.to.use]) # this is a matrix 

# The scaled:center attribute contains the mean values of all the columns.
print(attr(scaled_df, "scaled:center"))
# The scaled:scale attribute contains the variances of all the columns.
print(attr(scaled_df, "scaled:scale"))

d <- dist(scaled_df, method="euclidean")
pfit <- hclust(d, method="ward.D2")

# To examine `pfit`, type: summary(pfit) and pfit$height
plot(pfit, labels=clusterSet$Country, main="Dendrogram for Diet Related Causes")
rect.hclust(pfit, k=5) # k=5 means we want rectangles to be put around 5 clusters
xx <- c(1, 7, 12, 19, 22)
yy <- -4.5
clusterID <- c(2,3,1,5,4)
text(xx, yy, clusterID, col="red")


# Calculate the principal components of scaled_df
princ <- prcomp(scaled_df) 
nComp <- 2 # focus on the first two principal components

# project scaled_df onto the first 2 principal components to form a new
# 2-column data frame.
project2D <- as.data.frame(predict(princ, newdata=scaled_df)[,1:nComp])

# combine with `groups` and clusterSet$Country to form a 4-column data frame
hclust.project2D <- cbind(project2D, cluster=as.factor(groups), country=clusterSet$Country)

# finding convex hull
find_convex_hull <- function(proj2Ddf, groups) {
  do.call(rbind,
          lapply(unique(groups),
                 FUN = function(c) {
                   f <- subset(proj2Ddf, cluster==c);
                   f[chull(f),]
                 }
          )
  )
}
hclust.hull <- find_convex_hull(hclust.project2D, groups)

fig_hclust <- ggplot(hclust.project2D, aes(x=PC1, y=PC2)) +
  geom_point(aes(shape=cluster, color=cluster)) +
  geom_text(aes(label=country, color=cluster), hjust=0, vjust=1, size=3) +
  geom_polygon(data=hclust.hull, aes(group=cluster, fill=as.factor(cluster)),
               alpha=0.4, linetype=0) + theme(text=element_text(size=20))

sqr_euDist <- function(x, y) {
  sum((x - y)^2)
}

# Function to calculate WSS of a cluster, represented as a n-by-d matrix
# (where n and d are the numbers of rows and columns of the matrix)
# which contains only points of the cluster.
wss <- function(clustermat) {
  c0 <- colMeans(clustermat)
  sum(apply( clustermat, 1, FUN=function(row) {sqr_euDist(row, c0)} ))
}

# Function to calculate the total WSS. Argument `scaled_df`: data frame
# with normalised numerical columns. Argument `labels`: vector containing
# the cluster ID (starting at 1) for each row of the data frame.
wss_total <- function(scaled_df, labels) {
  wss.sum <- 0
  k <- length(unique(labels))
  for (i in 1:k)
    wss.sum <- wss.sum + wss(subset(scaled_df, labels == i))
  wss.sum
}

# Function to calculate total sum of squared (TSS) distance of data
# points about the (global) mean. This is the same as WSS when the
# number of clusters (k) is 1.
tss <- function(scaled_df) {
  wss(scaled_df)
}
# Function to return the CH indices computed using hierarchical
# clustering (function `hclust`) or k-means clustering (`kmeans`)
# for a vector of k values ranging from 1 to kmax.
CH_index <- function(scaled_df, kmax, method="kmeans") {
  if (!(method %in% c("kmeans", "hclust")))
    stop("method must be one of c('kmeans', 'hclust')")
  npts <- nrow(scaled_df)
  wss.value <- numeric(kmax) # create a vector of numeric type
  # wss.value[1] stores the WSS value for k=1 (when all the
  # data points form 1 large cluster).
  wss.value[1] <- wss(scaled_df)
  if (method == "kmeans") {
    # kmeans
    for (k in 2:kmax) {
      clustering <- kmeans(scaled_df, k, nstart=10, iter.max=100)
      wss.value[k] <- clustering$tot.withinss
    }
  } else {
    # hclust
    d <- dist(scaled_df, method="euclidean")
    pfit <- hclust(d, method="ward.D2")
    for (k in 2:kmax) {
      labels <- cutree(pfit, k=k)
      wss.value[k] <- wss_total(scaled_df, labels)
    }
  }
  bss.value <- tss(scaled_df) - wss.value # this is a vector
  B <- bss.value / (0:(kmax-1)) # also a vector
  W <- wss.value / (npts - 1:kmax) # also a vector
  data.frame(k = 1:kmax, CH_index = B/W, WSS = wss.value)
}

# calculate the CH criterion
crit.df <- CH_index(scaled_df, 10, method="hclust")

figCH <- ggplot(crit.df, aes(x=k, y=CH_index)) +
  geom_point() + geom_line(colour="red") +
  scale_x_continuous(breaks=1:10, labels=1:10) +
  labs(y="CH index") + theme(text=element_text(size=20))
figWSS <- ggplot(crit.df, aes(x=k, y=WSS), color="blue") +
  geom_point() + geom_line(colour="blue") +
  scale_x_continuous(breaks=1:10, labels=1:10) +
  theme(text=element_text(size=20))

# grid.arrange(figCH, figWSS, nrow=1)

kbest.p <- 5
# run kmeans with 5 clusters, 100 random starts, and 100
# maximum iterations per run.
kmClusters <- kmeans(scaled_df, kbest.p, nstart=100, iter.max=100)

kmClustering.ch <- kmeansruns(scaled_df, krange=1:10, criterion="ch")

kmClustering.asw <- kmeansruns(scaled_df, krange=1:10, criterion="asw")

hclusting <- CH_index(scaled_df, 10, method="hclust")
cat("CH index from hclust for k=1 to 10:/n", hclusting$CH_index)

kmCritframe <- data.frame(k=1:10, ch=kmClustering.ch$crit,
                          asw=kmClustering.asw$crit)

figCHKmean <- ggplot(kmCritframe, aes(x=k, y=ch)) +
  geom_point() + geom_line(colour="red") +
  scale_x_continuous(breaks=1:10, labels=1:10) +
  labs(y="CH index") + theme(text=element_text(size=20))

figASWKmean <- ggplot(kmCritframe, aes(x=k, y=asw)) +
  geom_point() + geom_line(colour="blue") +
  scale_x_continuous(breaks=1:10, labels=1:10) +
  labs(y="ASW") + theme(text=element_text(size=20))

# grid.arrange(figCHKmean, figASWKmean, nrow=1)

fig <- c()
kvalues <- c(2,5,6,8)
for (k in kvalues) {
  groups <- kmeans(scaled_df, k, nstart=100, iter.max=100)$cluster
  kmclust.project2D <- cbind(project2D, cluster=as.factor(groups),
                             country=df$Country)
  kmclust.hull <- find_convex_hull(kmclust.project2D, groups)
  assign(paste0("fig", k),
         ggplot(kmclust.project2D, aes(x=PC1, y=PC2)) +
           geom_point(aes(shape=cluster, color=cluster)) +
           geom_polygon(data=kmclust.hull, aes(group=cluster, fill=cluster),
                        alpha=0.4, linetype=0) +
           labs(title = sprintf("k = %d", k)) +
           theme(legend.position="none", text=element_text(size=20))
  )
}

# grid.arrange(fig2, fig5, fig6, fig8, nrow=2)


## Clustering

dTrain$class <- as.numeric(dTrain[outcome] == TRUE)
dTest$class <- as.numeric(dTest[outcome] == TRUE)
dCal$class <- as.numeric(dCal[outcome] == TRUE)
pos <- 1

set.seed(12345)

# variables holding truth
#responses <- colnames(dTrain) %in% c(outcome, "class")
features <- numericVars[numericVars != 'Death.rate'] # colnames(dTrain)[!responses] # gsub("^pred\\.", "", selVars) 

formula <- paste("class", paste(features, collapse=" + "), sep=" ~ ")
# print(formula)

model_logr <- glm(formula=formula, data=dTrain, family=binomial(link="logit"))

dTrain$pred <- predict(model_logr, newdata=dTrain, type="response")
dTest$pred <- predict(model_logr, newdata=dTest, type="response")
dCal$pred <- predict(model_logr, newdata=dCal, type="response")

# ytrue should be a vector containing 1s (or TRUE) and 0s (or FALSE);
# ypred should be a vector containing the predicted probability values for the target class.
# Both ytrue and ypred should have the same length.
performanceMeasures <- function(ytrue, ypred, model.name = "model", threshold=0.5) {
  # compute the normalised deviance
  dev.norm <- -2 * logLikelihood(ytrue, ypred)/length(ypred)
  cmat <- table(actual = ytrue, predicted = ypred >= threshold)
  accuracy <- sum(diag(cmat)) / sum(cmat)
  precision <- cmat[2, 2] / sum(cmat[, 2])
  recall <- cmat[2, 2] / sum(cmat[2, ])
  f1 <- 2 * precision * recall / (precision + recall)
  data.frame(model = model.name, precision = precision, recall = recall, 
             f1 = f1, dev.norm = dev.norm)
}

fit_mortality_example = function(variable_matrix, labelvec) {
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

# Fit the mortality dataset using xgboost:
input <- as.matrix(dTrain[features])
model <- fit_mortality_example(input, dTrain$class)

explainer <- lime(dTrain[features], model = model, 
                  bin_continuous = TRUE, n_bins = 10)

example <- dTest[cases,features]

explanation <- lime::explain(example, explainer, n_labels = 1, n_features = 4)


# ------------- GUI -------------

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("EDA Project single-multi variables"),
  
  # Sidebar with a slider input
  tabsetPanel(
    id = 'tabs',
    
    tabPanel("EDA",
             sidebarLayout(
               sidebarPanel(
                 selectInput("plot_type", "Select plot type:",
                             choices = c('hist_single',
                                         'box_single',
                                         'box_compare',
                                         'trend_compare',
                                         'bar_compare',
                                         "pairwise",
                                         "scatter",
                                         "geom_bin2d",
                                         "colorplot",
                                         "jitter",
                                         "hist_density",
                                         "stackedbar",
                                         "sidebysidebar",
                                         "filledbar",
                                         "overlayingbar"
                             )),
                 conditionalPanel(
                   condition = "input.plot_type == 'hist_single'",
                   selectInput(inputId = "year", label = "Choose year", choices = c('All', years)),
                   selectInput(inputId = "var", label = "Choose death cause", choices = num_vars)
                 ),
                 conditionalPanel(
                   condition = "input.plot_type == 'box_single'",
                   selectInput(inputId = "con", label = "Choose country", choices = c('All', countries), selected = 'All'),
                   selectInput(inputId = "var", label = "Choose death cause", choices = num_vars)
                 ),
                 conditionalPanel(
                   condition = "input.plot_type == 'box_compare'",
                   selectInput(inputId = "var", 
                               label = "Choose death cause", choices = num_vars)#c('Unsafe.water.source', 'Unsafe.sanitation', 'No.access.to.handwashing.facility')),
                 ),
                 conditionalPanel(
                   condition = "input.plot_type == 'trend_compare'",
                   selectInput(inputId = "var", label = "Choose death cause", choices = num_vars),
                 ),
                 conditionalPanel(
                   condition = "input.plot_type == 'bar_compare'",
                   selectInput(inputId = "year", label = "Choose year", choices = unique(df$Year)),
                   selectInput(inputId = "var", label = "Choose death cause", choices = num_vars, selected = 'Drug.use'),
                 ),
                 conditionalPanel(
                   condition = "input.plot_type == 'pairwise'",
                   selectInput(inputId = "con", label = "Choose country", choices = countries),
                   
                   pickerInput(
                     inputId = "selected_attributes",
                     label = "Select attributes:",
                     choices = num_vars,
                     multiple = TRUE,
                     options = list('actions-box' = TRUE)
                   )
                 ),
                 conditionalPanel(
                   condition = "input.plot_type == 'scatter'",
                   #selectInput(inputId = "con", label = "Choose country", choices = countries),
                   selectInput("x_attr", "Select x-axis attribute:", num_vars),
                   selectInput("y_attr", "Select y-axis attribute:", num_vars)
                 ),
                 conditionalPanel(
                   condition = "input.plot_type == 'geom_bin2d'",
                   #selectInput(inputId = "con", label = "Choose country", choices = countries),
                   selectInput("x_attr", "Select x-axis attribute:", num_vars),
                   selectInput("y_attr", "Select y-axis attribute:", num_vars),
                   sliderInput('alp', 'Transparency', min = 0.1, max = 1.0, value = 0.35)
                 )
               ),
               mainPanel(
                 plotOutput("plot", height = '600px', width = '900px')
               )
             )
    ),
    tabPanel("ML",
             sidebarLayout(
               sidebarPanel(
                 selectInput("mlOption", "Select an option for ML:",
                             choices = c('Contribution death causes',
                                         'ROC plot categorical vars',
                                         'ROC plot selNumVars',
                                         'Double Density plot',
                                         'Hierarchical Cluster',
                                         'Hclust assessment',
                                         'Kmeans clust assessment',
                                         'PCA clusters',
                                         'LIME explainer plot')),
                 conditionalPanel(
                   condition = "input.mlOption == 'Hierarchical Cluster'",
                   sliderInput('kvalue', 'K value', min = 2, max = 10, value = 2, step = 1)
                   # selectInput(inputId = "kvalue", label = "Choose K", choices = 2:10)
                 ),
               ),
               mainPanel(
                 plotOutput("mlplot", height = '600px', width = '900px')
               )
             )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # in the server:
  output$plot <- renderPlot({
    if(input$plot_type == "hist_single"){
      data <- data.frame()
      
      if(input$year == 'All')
        data <- df_original
      else
        data <- df_original[df_original['Year']==input$year,] 
      
      ggplot(data) +
        geom_histogram(aes_string(x = input$var), bins = 40, fill = "gray") +
        labs(x = input$var, y = "Frequency", subtitle = 'Frequency per cause for all countries')
    }
    else if (input$plot_type == "box_single"){
      data <- data.frame()
      
      if(input$con == 'All')
        data <- df_original
      else
        data <- df_original[df_original['Country']==input$con,] 
      
      boxplot(data[input$var], xlab=input$var, ylab = 'Count')
    }
    else if (input$plot_type == 'box_compare'){
      sel_var <- input$var
      
      p1 <- ggplot(dataUK) + geom_boxplot(aes_string(y=sel_var)) +
        labs(x = input$var, y= 'Count', title = 'UK')
      p2 <- ggplot(dataTAN) + geom_boxplot(aes_string(y=sel_var)) +
        labs(x = input$var, y= 'Count', title = 'Tanzania')
      p3 <- ggplot(dataNLD) + geom_boxplot(aes_string(y=sel_var)) +
        labs(x = input$var, y= 'Count', title = 'Netherlands')
      p4 <- ggplot(dataCAM) + geom_boxplot(aes_string(y=sel_var)) +
        labs(x = input$var, y= 'Count', title = 'Cambodia')
      
      grid.arrange(p1, p2, p3, p4, nrow=2)
    }
    else if(input$plot_type == 'trend_compare'){
      sel_var <- input$var
      
      p1 <- ggplot(dataUK, aes_string(x ='Year', y=sel_var)) + 
        geom_line(color='gray', size=2) + 
        labs(title = 'UK')
      p2 <- ggplot(dataTAN, aes_string(x ='Year', y=sel_var)) + 
        geom_line(color='gray', size=2) + 
        labs(title = 'Tanzania')
      p3 <- ggplot(dataNLD, aes_string(x ='Year', y=sel_var)) + 
        geom_line(color='gray', size=2) + 
        labs(title = 'Netherlands')
      p4 <- ggplot(dataCAM, aes_string(x ='Year', y=sel_var)) + 
        geom_line(color='gray', size=2) + 
        labs(title = 'Cambodia')
      
      grid.arrange(p1, p2, p3, p4, nrow=2)
    }
    else if(input$plot_type == 'bar_compare'){
      df_original_year <- df_original[df_original$Year==input$year,]
      sam_year <- df_original_year[sample(nrow(df_original_year), 20), ]
      # sel_var <- input$var
      rr_trans <- transform(sam_year, Country = reorder(Country, Drug.use))
      ggplot(rr_trans, aes(x = Country, y = Drug.use)) +
        geom_bar(stat = "identity", fill = 'red') + 
        coord_flip()
    }
    else if (input$plot_type == "pairwise") {
      data <- df_original[df_original['Country']==input$con,]
      selected_attrs <- input$selected_attributes
      if (length(selected_attrs) < 2) {
        return(NULL)
      }
      pairs(data[, selected_attrs], main = input$con)
    } 
    else if(input$plot_type == "scatter") {
      x_attr <- input$x_attr
      y_attr <- input$y_attr
      ggplot(df_original, aes_string(x = x_attr, y = y_attr)) +
        geom_jitter(width=0.6, height=3, alpha=0.5) + 
        geom_smooth() +
        xlim(1, 3000) +
        ylim(1, 7000)
    } 
    else if(input$plot_type == "geom_bin2d"){
      #data <- df_original[df_original['Country']==input$con,]
      x_attr <- input$x_attr
      y_attr <- input$y_attr
      alp <- input$alp
      ggplot(df_original, aes_string(x=x_attr, y=y_attr) ) +
        geom_bin2d(alpha=alp) +
        xlim(1, 3000) +
        ylim(1, 7000)
    }
  })
  
  output$mlplot <- renderPlot(
    if (input$mlOption == "Contribution death causes") {
      figRateAll
    } 
    else if (input$mlOption == "ROC plot categorical vars") {
      plot_roc(dCal[[selCatVars[1]]], dCal[,outcome]) #First variable is red
      plot_roc(dCal[[selCatVars[2]]], dCal[,outcome], colour_id=3, overlaid=T) # Second variable is green
      plot_roc(dCal[[selCatVars[3]]], dCal[,outcome], colour_id=4, overlaid=T)
      text(x = 0.4, y = 0.8, 
           label = paste('Unsafe.sex.over.median (red)', ' Smoking.over.median (green)',
                         "and Blood.pressure.over.median (blue) are overlapping", 
                         sep='\n')
      )
    }
    else if (input$mlOption == "ROC plot selNumVars") {
      plot_roc(dCal$pred.total.population, dCal[,outcome], colour_id=3) # population is green
      plot_roc(dCal$pred.Unsafe.sanitation, dCal[,outcome], colour_id=4, overlaid=T) #  sanitation is blue
      text(x = 0.4, y = 0.6, 'Total population', col = 'green', cex= 1.5)
      text(x = 0.4, y = 0.3, 'Unsafe sanitation', col = 'blue', cex = 1.5)
    }
    else if (input$mlOption == "Double Density plot"){
      fig1 <- ggplot(dCal) + geom_density(aes(x=pred.total.population, color=as.factor(Death.rate.high)))
      fig2 <- ggplot(dCal) + geom_density(aes(x=pred.Unsafe.sanitation, color=as.factor(Death.rate.high)))
      grid.arrange(fig1, fig2, ncol=1)
    }
    else if (input$mlOption == "Hierarchical Cluster"){
      # To examine `pfit`, type: summary(pfit) and pfit$height
      plot(pfit, labels=clusterSet$Country, main="Dendrogram for Diet Related Causes")
      kvalue <- as.numeric(input$kvalue)
      rect.hclust(pfit, k=kvalue) # k=5 means we want rectangles to be put around 5 clusters
      xx <- c(1, 7, 12, 19, 22)
      yy <- -4.5
      clusterID <- c(2,3,1,5,4)
      text(xx, yy, clusterID, col="red")
    }
    else if (input$mlOption == "Hclust assessment"){
      grid.arrange(figCH, figWSS, nrow=1)
    }
    else if (input$mlOption == "Kmeans clust assessment"){
      grid.arrange(figCHKmean, figASWKmean, nrow=1)
    }
    else if (input$mlOption == "PCA clusters"){
      grid.arrange(fig2, fig5, fig6, fig8, nrow=2)
    }
    else if (input$mlOption == "LIME explainer plot"){
      plot_features(explanation)
    }
  )
}



# Run the application
shinyApp(ui = ui, server = server)

