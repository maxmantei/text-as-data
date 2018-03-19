# Ulrich Fritsche
# based on code by Gregor Wiedemann and Andreas Niekler and Pablo Barbera (LSE)
# Jan 22, 2018
# --------------------------------------
# Housekeeping & Path
# --------------------------------------


# Be aware that all necessary packages are installed
#

rm(list=ls())
# check working directory. It should be the destination folder of the extracted 
# zip file. If necessary, use `setwd("your-tutorial-folder-path")` to change it.

setwd("C:/Users/FritscheU/Dropbox/RCourse_Text_Mining/Code")
getwd() # check that the folder is correct

# important option for text analysis
options(stringsAsFactors = F)

library(tm)

textdata <- read.csv("data/sotu_paragraphs.csv", sep = ";", encoding = "UTF-8")
english_stopwords <- readLines("resources/stopwords_en.txt", encoding = "UTF-8")

# Create corpus object
names(textdata)[names(textdata) == "id"] <- "doc_id"
textdata <- textdata[c(2,6,1,3,4,5)]

corpus <- Corpus(DataframeSource(textdata))

# Preprocessing chain
processedCorpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus <- tm_map(processedCorpus, removeNumbers)
processedCorpus <- tm_map(processedCorpus, content_transformer(tolower))
processedCorpus <- tm_map(processedCorpus, stripWhitespace)

# Read previously annotated training data
trainingData <- read.csv2("data/paragraph_training_data_format.csv", stringsAsFactors = T)
# Training data format 
colnames(trainingData)

# Example paragraph annotated as "Foreign Affairs"
set.seed(13)
domestic_example <- sample(trainingData$ID[trainingData$LABEL == "DOMESTIC"], 1)
as.character(corpus[[domestic_example]])

foreign_example <- sample(trainingData$ID[trainingData$LABEL == "FOREIGN"], 1)
as.character(corpus[[foreign_example]])

#How is the ratio between domestic and foreign content in the training data?

classCounts <- table(trainingData[, "LABEL"])
print(classCounts)

numberOfDocuments <- nrow(trainingData)

# Base line: create feature set out of unigrams
DTM <- DocumentTermMatrix(processedCorpus)
# How many features do we have?
dim(DTM)

# Probably the DTM is too big for the classifier. Let us reduce it
minimumFrequency <- 5
DTM <- DocumentTermMatrix(processedCorpus, control = list(bounds = list(global = c(minimumFrequency, Inf))))
dim(DTM)

# we build a linear SVM classification model with the LiblineaR package.
require(LiblineaR)
require(SparseM)
source("utils.R")

# First, we load the packages. Since Liblinear requires a special Sparse Matrix format, we also load the “SparseM” package and a conversion function which allows to convert slam-matrices (as used in the tm package) into SparseM-matrices.
# Then, we split the annotated data into a training set (80%) and a test set (20%) using a boolean selector. The expression assigned to selector_idx creates a boolean vector of length 300 containing a FALSE value in every fifths position. This selector is used to select to training set. Its inverted vector (!) is used to select the test set.

annotatedDTM <- DTM[trainingData[, "ID"], ]
annotatedDTM <- convertSlamToSparseM(annotatedDTM)
annotatedLabels <- trainingData[, "LABEL"]

# split into training and test set
selector_idx <- rep(c(rep(TRUE, 4), FALSE), length.out = numberOfDocuments) # rep() replicates, length.out definiert Länge, generiert eine Dummy-Variable mit 111101111011110...
trainingDTM <- annotatedDTM[selector_idx, ]
trainingLabels <- annotatedLabels[selector_idx]
testDTM <- annotatedDTM[!selector_idx, ]
testLabels <- annotatedLabels[!selector_idx]

# create SVM model
model <- LiblineaR(trainingDTM, trainingLabels, type=2)
summary(model)

# The model created by the LiblineaR function can now be utilized to predict the labels of the test set. Then we compare the result of the automatic classification to our known labels to determine the accuracy of the process.

classification <- predict(model, testDTM) 
predictedLabels <- classification$predictions
contingencyTable <- table(predictedLabels, testLabels)
print(contingencyTable) # contingency table 

accuracy <- sum(diag(contingencyTable)) / length(testLabels)
print(accuracy) # Accuracy = fraction of correctly classified elements

# Function for Precision, Recall, Specificity, F value, Accuracy
source("F.measure.R") ## here c-optimisation is included

# Create pseudo classification
pseudoLabelsDOM <- factor(rep("DOMESTIC", length(testLabels)), levels(testLabels))
pseudoLabelsFOR <- factor(rep("FOREIGN", length(testLabels)), levels(testLabels))

# Evaluation of former SVM classification with F-measures
F.measure(predictedLabels, testLabels, positiveClassName = "DOMESTIC")
F.measure(predictedLabels, testLabels, positiveClassName = "FOREIGN")

# Evaluation of pseudo classification with F-measures
F.measure(pseudoLabelsDOM, testLabels, positiveClassName = "DOMESTIC")
F.measure(pseudoLabelsDOM, testLabels, positiveClassName = "FOREIGN")

# If classes in training/test sets are imbalanced, accuracy might be a misleading measurement. Other measure should be considered additionally.
# To utilize accuracy and F-measure in a meaningful way, the less frequent class should be defined as POSITIVE class (FOREIGN in our case).

# introduce another function

get_k_fold_logical_indexes <- function(j, k, n) {
  if (j > k) stop("Cannot select fold larger than nFolds")
  fold_lidx <- rep(FALSE, k)
  fold_lidx[j] <- TRUE
  fold_lidx <- rep(fold_lidx, length.out = n)
  return(fold_lidx)
}

# Example usage
get_k_fold_logical_indexes(1, k = 10, n = 12)

# create a loop for k-fold validation
k <- 10
evalMeasures <- NULL
for (j in 1:k) {
  # create j-th boolean selection vector
  currentFold <- get_k_fold_logical_indexes(j, k, nrow(trainingDTM))
  
  # select training data split
  foldDTM <- annotatedDTM[!currentFold, ]
  foldLabels <- annotatedLabels[!currentFold]
  
  # create model
  model <- LiblineaR(foldDTM, foldLabels)
  
  # select test data split
  testSet <- annotatedDTM[currentFold, ]
  testLabels <- annotatedLabels[currentFold]
  
  # predict test labels
  predictedLabels <- predict(model, testSet)$predictions
  
  # evaluate predicted against test labels
  kthEvaluation <- F.measure(predictedLabels, testLabels, positiveClassName = "FOREIGN")
  
  # combine evaluation measures for k runs
  evalMeasures <- rbind(evalMeasures, kthEvaluation)
}
# Final evaluation values of k runs:
print(evalMeasures)

print(colMeans(evalMeasures))

# C parameter

cParameterValues <- c(0.003, 0.01, 0.03, 0.1, 0.3, 1, 3 , 10, 30, 100)
fValues <- NULL

for (cParameter in cParameterValues) {
  print(paste0("C = ", cParameter))
  evalMeasures <- k_fold_cross_validation(annotatedDTM, annotatedLabels, cost = cParameter)
  fValues <- c(fValues, evalMeasures["F"])
}

# plot result

plot(fValues, type="o", col="green", xaxt="n")
axis(1,at=1:length(cParameterValues), labels = cParameterValues)

# best value

bestC <- cParameterValues[which.max(fValues)]
print(paste0("Best C value: ", bestC, ", F1 = ", max(fValues)))

###########################################################################################
# Another (smart) approach: Pablo Barbera (LSE)
###########################################################################################

# setwd("C:/Users/FritscheU/Dropbox/RCourse_Text_Mining/Code/data/")

library(quanteda)
#tweets <- read.csv("./EP-elections-tweets.csv", stringsAsFactors=F)
#save(tweets, file = "data/tweets.RData")

# Due to some compatibility issues, we saved the data in original R format
load("data/tweets.RData")

# dummy for polite
tweets$impolite <- ifelse(tweets$polite=="polite", 0, 1)

# cleaning
tweets$text <- gsub('@[0-9_A-Za-z]+', '@', tweets$text)

# tweets$text <- gsub('@[0-9_A-Za-z]+','', tweets$text)

# we make use of quanteda's dfm object format
twcorpus <- corpus(tweets$text) # create corpus
twdfm <- dfm(twcorpus, remove = c(stopwords("english"), "t.co", "https", "rt", "amp", "http", "t.c", "can")) # create dfm object
twdfm <- dfm_trim(twdfm, min_docfreq = 2) # trim by minimum document frequency
topfeatures(twdfm, 30) # have a look at "topfeatures"
#textplot_wordcloud(twdfm, max.words = 200)

set.seed(123)
# define training data
training <- sample(1:nrow(tweets), floor(.80 * nrow(tweets)))
test <- (1:nrow(tweets))[1:nrow(tweets) %in% training == FALSE]

library(glmnet)
library(doParallel) # parallel computing in Windows
registerDoParallel(3) # I use 3 out of my 4 cores
# 
#require(doMC) # linux
#registerDoMC(cores=3)
ridge <- cv.glmnet(twdfm[training,], tweets$impolite[training], 
                   family="binomial", alpha=0, nfolds=5, parallel=T,
                   type.measure="deviance")
plot(ridge)

# Alternatively you can apply lasso (or other approaches) - check out glmnet package

# lasso <- cv.glmnet(twdfm[training,], tweets$impolite[training], 
#                    family="binomial", alpha=1, nfolds=5, parallel=T, intercept=T,
#                    type.measure="class")
# 
# plot(lasso)

# performance

## function to compute accuracy
accuracy <- function(ypred, y){
  tab <- table(ypred, y)
  return(sum(diag(tab))/sum(tab))
}
# function to compute precision
precision <- function(ypred, y){
  tab <- table(ypred, y)
  return((tab[2,2])/(tab[2,1]+tab[2,2]))
}
# function to compute recall
recall <- function(ypred, y){
  tab <- table(ypred, y)
  return(tab[2,2]/(tab[1,2]+tab[2,2]))
}
# computing predicted values
preds <- predict(ridge, twdfm[test,], type="response") > mean(tweets$impolite[test])
# confusion matrix
table(preds, tweets$impolite[test])
# performance metrics
accuracy(preds, tweets$impolite[test])
precision(preds, tweets$impolite[test])
recall(preds, tweets$impolite[test])

# from the different values of lambda, let's pick the best one
best.lambda <- which(ridge$lambda==ridge$lambda.min)
beta <- ridge$glmnet.fit$beta[,best.lambda]
head(beta)

# identifying predictive features
df <- data.frame(coef = as.numeric(beta),
                 word = names(beta), stringsAsFactors=F)

df <- df[order(df$coef),]
head(df[,c("coef", "word")], n=30)
paste(df$word[1:30], collapse=", ")

df <- df[order(df$coef, decreasing=TRUE),]
head(df[,c("coef", "word")], n=30)
paste(df$word[1:30], collapse=", ")

# Exercises:
# Compare Lasso and Ridge
# Construct F1