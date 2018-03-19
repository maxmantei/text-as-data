# Ulrich Fritsche
# based on code by Gregor Wiedemann and Andreas Niekler
# Jan 16, 2018
# --------------------------------------
# Housekeeping & Path
# --------------------------------------


# Be aware that all necessary packages are installed

rm(list=ls())
# check working directory. It should be the destination folder of the extracted 
# zip file. If necessary, use `setwd("your-tutorial-folder-path")` to change it.

setwd("C:/Users/FritscheU/Dropbox/RCourse_Text_Mining/Code")
getwd() # check that the folder is correct

# important option for text analysis
options(stringsAsFactors = F)

library(tm)

textdata <- read.csv("data/sotu.csv", sep = ";", encoding = "UTF-8")
english_stopwords <- readLines("resources/stopwords_en.txt", encoding = "UTF-8")

# We rename "id" into "doc_id"
names(textdata)[names(textdata) == "id"] <- "doc_id"
textdata <- textdata[c(1,5,2,3,4)]

# Create corpus object

corpus <- Corpus(DataframeSource(textdata))
corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = TRUE)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeWords, english_stopwords)

# Stemming

require("SnowballC")
corpus <- tm_map(corpus, stemDocument, language = "en")
corpus <- tm_map(corpus, stripWhitespace)

# View first document

substr(as.character(corpus[[1]]), 0, 250)

# Create a DTM

DTM <- DocumentTermMatrix(corpus)

require(slam)

# Compute IDF: log(N / n_i)

number_of_docs <- nrow(DTM)
term_in_docs <- col_sums(DTM > 0)
idf <- log2(number_of_docs / term_in_docs)

# Compute TF

first_obama_speech <- which(textdata$president == "Barack Obama")[1]
tf <- as.vector(DTM[first_obama_speech, ])

# Compute TF-IDF

tf_idf <- tf * idf
names(tf_idf) <- colnames(DTM)

# there is a simple way to use tfidf weights to the DTM
# weightDTM <- weightTfIdf(DTM, normalize = T)


targetDTM <- DocumentTermMatrix(corpus)

termCountsTarget <- as.vector(targetDTM[first_obama_speech, ])
names(termCountsTarget) <- colnames(targetDTM)

# Just keep counts greater than zero

termCountsTarget <- termCountsTarget[termCountsTarget > 0]

lines <- readLines("resources/eng_wikipedia_2010_30K-sentences.txt", encoding = "UTF-8")
comparisonCorpus <- Corpus(VectorSource(lines))
comparisonCorpus <- tm_map(comparisonCorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
comparisonCorpus <- tm_map(comparisonCorpus, removeNumbers)
comparisonCorpus <- tm_map(comparisonCorpus, content_transformer(tolower))
comparisonCorpus <- tm_map(comparisonCorpus, removeWords, english_stopwords)
comparisonCorpus <- tm_map(comparisonCorpus, stemDocument, language = "en")
comparisonCorpus <- tm_map(comparisonCorpus, stripWhitespace)

# functions for tm_map can be created by the user!

comparisonDTM <- DocumentTermMatrix(comparisonCorpus)
termCountsComparison <- col_sums(comparisonDTM)

# Loglikelihood for a single term

term <- "care"

# Determine variables

a <- termCountsTarget[term]
b <- termCountsComparison[term]
c <- sum(termCountsTarget)
d <- sum(termCountsComparison)

# Compute log likelihood test

Expected1 = c * (a+b) / (c+d)
Expected2 = d * (a+b) / (c+d)
t1 <- a * log((a/Expected1))
t2 <- b * log((b/Expected2))
logLikelihood <- 2 * (t1 + t2)

print(logLikelihood)

# use set operation to get terms only occuring in target document

uniqueTerms <- setdiff(names(termCountsTarget), names(termCountsComparison))

# Have a look into a random selection of terms unique in the target corpus

sample(uniqueTerms, 20)

# Create vector of zeros to append to comparison counts

zeroCounts <- rep(0, length(uniqueTerms))
names(zeroCounts) <- uniqueTerms
termCountsComparison <- c(termCountsComparison, zeroCounts)

# Get list of terms to compare from intersection of target and comparison vocabulary

termsToCompare <- intersect(names(termCountsTarget), names(termCountsComparison))

# Calculate statistics (same as above, but now with vectors!)

a <- termCountsTarget[termsToCompare]
b <- termCountsComparison[termsToCompare]
c <- sum(termCountsTarget)
d <- sum(termCountsComparison)
Expected1 = c * (a+b) / (c+d)
Expected2 = d * (a+b) / (c+d)
t1 <- a * log((a/Expected1) + (a == 0))
t2 <- b * log((b/Expected2) + (b == 0))
logLikelihood <- 2 * (t1 + t2)

# Compare relative frequencies to indicate over/underuse

relA <- a / c
relB <- b / d

# underused terms are multiplied by -1

logLikelihood[relA < relB] <- logLikelihood[relA < relB] * -1

# top terms (overuse in targetCorpus compared to comparisonCorpus)

sort(logLikelihood, decreasing=TRUE)[1:25]

# bottom terms (underuse in targetCorpus compared to comparisonCorpus)

sort(logLikelihood, decreasing=FALSE)[1:25]

llTop100 <- sort(logLikelihood, decreasing=TRUE)[1:100]
frqTop100 <- termCountsTarget[names(llTop100)]
frqLLcomparison <- data.frame(llTop100, frqTop100)
View(frqLLcomparison)

# Number of significantly overused terms (p < 0.01)

sum(logLikelihood > 6.63)

require(wordcloud)
top50 <- sort(logLikelihood, decreasing = TRUE)[1:50]
wordcloud(names(top50), top50, max.words = 50, scale = c(3, .9), colors = brewer.pal(8, "Dark2"), random.order = F)

# produce wordclouds as pdf files!

source("calculateLogLikelihood.R")

presidents <- unique(textdata$president)
for (president in presidents) {
  
  cat("Extracting terms for president", president, "\n")
  
  selector_logical_idx <- textdata$president == president
  
  presidentDTM <- targetDTM[selector_logical_idx, ]
  termCountsTarget <- col_sums(presidentDTM)
  
  otherDTM <- targetDTM[!selector_logical_idx, ] # inversion!!! False gets true
  termCountsComparison <- col_sums(otherDTM)
  
  loglik_terms <- calculateLogLikelihood(termCountsTarget, termCountsComparison)
  
  top50 <- sort(loglik_terms, decreasing = TRUE)[1:50]
  
  fileName <- paste0("wordclouds/", president, ".pdf")
  pdf(fileName, width = 9, height = 7)
  wordcloud(names(top50), top50, max.words = 50, scale = c(3, .9), colors = brewer.pal(8, "Dark2"), random.order = F)
  dev.off()
  
}

