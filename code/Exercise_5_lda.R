# Ulrich Fritsche
# based on code by Gregor Wiedemann and Andreas Niekler
# Jan 16, 2018
# --------------------------------------
# Housekeeping & Path
# --------------------------------------
#
# Be aware that all necessary packages are installed!

rm(list=ls()) # cleaning memory

# check working directory. It should be the destination folder of the extracted 
# zip file. If necessary, use `setwd("your-tutorial-folder-path")` to change it.

setwd("C:/Users/FritscheU/Dropbox/RCourse_Text_Mining/Code")
getwd() # check that the folder is correct

# important option for text analysis
options(stringsAsFactors = F)

# load packages
library("tm")
library("topicmodels")

textdata <- read.csv("data/sotu.csv", sep = ";", encoding = "UTF-8")
english_stopwords <- readLines("resources/stopwords_en.txt", encoding = "UTF-8")

# We rename "id" into "doc_id"

names(textdata)[names(textdata) == "id"] <- "doc_id"
textdata <- textdata[c(1,5,2,3,4)] # re-ordering

# Create corpus object

corpus <- Corpus(DataframeSource(textdata))

# Preprocessing chain

processedCorpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus <- tm_map(processedCorpus, removeNumbers)
processedCorpus <- tm_map(processedCorpus, content_transformer(tolower))
processedCorpus <- tm_map(processedCorpus, removeWords, english_stopwords)
processedCorpus <- tm_map(processedCorpus, stemDocument, language = "en")
processedCorpus <- tm_map(processedCorpus, stripWhitespace)

# compute document term matrix with terms >= minimumFrequency

minimumFrequency <- 5
DTM <- DocumentTermMatrix(processedCorpus, control = list(bounds = list(global = c(minimumFrequency, Inf))))

# have a look at the number of documents and terms in the matrix

dim(DTM)


# number of topics
# There is a large literature on evaluating and "fine-tuning" the number of K.
# In case you want to try -- please uncomment the following lines (default):

# library("ldatuning")
# 
# res_lda_tuning <- FindTopicsNumber(
#   DTM,
#   topics = seq(from = 10, to = 50, by = 5),
#   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#   method = "Gibbs",
#   control = list(iter = 100, verbose = 25, seed = 9161),
#   mc.cores = 2L,
#   verbose = TRUE
# )
# 
# FindTopicsNumber_plot(res_lda_tuning)

# K <- 30
K <- 20

# compute the LDA model, inference via 1000 iterations of Gibbs sampling
#topicModel <- LDA(DTM, K, method="Gibbs", control=list(iter = 1000, verbose = 25, seed = 9161))
#save(topicModel, file = "data/topicModel_sotu.RData")
load("data/topicModel_sotu.RData")

# have a look a some of the results (posterior distributions)
tmResult <- posterior(topicModel)

# format of the resulting object

attributes(tmResult)
nTerms(DTM)              # lengthOfVocab

# topics are probability distribtions over the entire vocabulary

beta <- tmResult$terms   # get beta from results
dim(beta)                # K distributions over nTerms(DTM) terms
rowSums(beta)            # rows in beta sum to 1
nDocs(DTM)               # size of collection

# for every document we have a probaility distribution of its contained topics

theta <- tmResult$topics 
dim(theta)     
rowSums(theta)[1:10]     # rows in theta sum to 1

# show 10 most likely terms within the term probabilities beta of the inferred topics

terms(topicModel, 10)

# create top5 terms per topic

top5termsPerTopic <- terms(topicModel, 5)

# Use top 5 terms as as topic names

topicNames <- apply(top5termsPerTopic, 2, paste, collapse=" ")

# visualize topics as word cloud

library("wordcloud")

#topicToViz <- 11 # change for your own topic of interest
topicToViz <- grep('mexico', topicNames)[1] # Or select a topic by a term contained in its name

# select to 40 most probable terms from the topic by sorting the term-topic-probability vector in decreasing order
top40terms <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
words <- names(top40terms)

# extract the probabilites of each of the 40 terms
probabilities <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]

# visualize the terms as wordcloud
mycolors <- brewer.pal(8, "Accent")

# send the wordcloud to pdf (different to tutorial)
fileName <- paste0("wordclouds/", topicToViz, ".pdf")
pdf(fileName, width = 9, height = 7)
wordcloud(words, probabilities, random.order = FALSE, color = mycolors)
dev.off()

# contents of three sample documents:
exampleIds <- c(1, 100, 200)
# lapply(corpus[exampleIds], as.character)

# visualize the topic distributions within the documents
# load libraries for visualization
library("reshape2")
library("ggplot2")

N <- length(exampleIds)

# get topic proportions form example documents
topicProportionExamples <- theta[exampleIds,]
colnames(topicProportionExamples) <- topicNames
vizDataFrame <- melt(cbind(data.frame(topicProportionExamples), document = factor(1:N)), variable.name = "topic", id.vars = "document")  

ggplot(data = vizDataFrame, aes(topic, value, fill = document), ylab = "proportion") + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  coord_flip() +
  facet_wrap(~ document, ncol = N)

# Extensions
# Now let us change the alpha prior to a lower value to see how this affects the topic distributions in the model.

# see alpha from previous model
attr(topicModel, "alpha") 

topicModel2 <- LDA(DTM, K, method="Gibbs", control=list(iter = 500, verbose = 25, alpha = 0.2))
tmResult <- posterior(topicModel2)
theta <- tmResult$topics
beta <- tmResult$terms
topicNames <- apply(terms(topicModel2, 5), 2, paste, collapse = " ")  #reset topicnames

# Topic ranking
# What are the defining topics within a collection? There are different approaches to find out which can be used to bring the topics into a certain order.

# Approach 1: We sort topics according to their probability within the entire collection:
# What are the most probable topics in the entire collection?
topicProportions <- colSums(theta) / nDocs(DTM)  # mean probablities over all documents
names(topicProportions) <- topicNames     # assign the topic names we created before
sort(topicProportions, decreasing = TRUE) # show summed proportions in decreased order

# Approach 2: We count how often a topic appears as a primary topic within a document. This method is also called Rank-1.
countsOfPrimaryTopics <- rep(0, K)
names(countsOfPrimaryTopics) <- topicNames
for (i in 1:nDocs(DTM)) {
  topicsPerDoc <- theta[i, ] # select topic distribution for document i
  # get first element position from ordered list
  primaryTopic <- order(topicsPerDoc, decreasing = TRUE)[1] 
  countsOfPrimaryTopics[primaryTopic] <- countsOfPrimaryTopics[primaryTopic] + 1
}
sort(countsOfPrimaryTopics, decreasing = TRUE)

# In the following, we will select documents based on their topic content and display the resulting document quantity over time.
topicToFilter <- 6  # you can set this manually ...
# ... or have it selected by a term in the topic name (e.g. 'iraq')
#topicToFilter <- grep('iraq', topicNames)[1] 

topicThreshold <- 0.2
selectedDocumentIndexes <- which(theta[, topicToFilter] >= topicThreshold)
filteredCorpus <- corpus[selectedDocumentIndexes]

# show length of filtered corpus
filteredCorpus

# Topic proportions over time
# append decade information for aggregation
textdata$decade <- paste0(substr(textdata$date, 0, 3), "0")
# save the data for further analysis

# get mean topic proportions per decade
topic_proportion_per_decade <- aggregate(theta, by = list(decade = textdata$decade), mean)

# set topic names to aggregated columns
colnames(topic_proportion_per_decade)[2:(K+1)] <- topicNames

# reshape data frame
vizDataFrame <- melt(topic_proportion_per_decade, id.vars = "decade")

# plot topic proportions per deacade as bar plot
ggplot(vizDataFrame, aes(x=decade, y=value, fill=variable)) + geom_bar(stat = "identity")

#####################################################
# Exercises or homework
# Estimate the model with 10 and 50 topics
# Use stemming in the pre-processing and re-estimate


######################################################
# New section on STM based on quanteda and stm

library("tidyverse")
library("quanteda")
library("stm")

#####################################################

dataset <- read_csv("data/MyData.csv")
myCorpus <- corpus(dataset$text)
docvars(myCorpus, field = "decade") <- as.integer(dataset$decade)
docvars(myCorpus, field = "president") <- as.character(dataset$president)

# DTM is dfm in quanteda 
dfm <- dfm(myCorpus,
           remove = c(stopwords("english"),english_stopwords),
           ngrams= 1L,
           stem = F,
           remove_numbers = TRUE, 
           remove_punct = TRUE,
           remove_symbols = TRUE)

# avoids complete preprocessing pipe... be aware that German language support is still in its infancy

stmdfm <- convert(dfm, to = "stm", docvars = docvars(myCorpus))
out <- prepDocuments(stmdfm$documents, stmdfm$vocab, stmdfm$meta, lower.thresh = 5)

# there is a kSearch function here -- that takes a lot of time!

# k <- seq(from = 10, to = 50, by = 5)
# 
# kresult <- searchK(out$documents, out$vocab, k, prevalence =~ s(decade), data = out$meta, init.type = "Spectral")
# # 
# # pdf("./kresult.pdf", width = 9, height = 7)
# # plot(kresult)
# # dev.off()
# 
# plot(kresult)

k <- 20

# stmFit <- stm(out$documents, out$vocab, K = k, prevalence =~ s(decade), max.em.its = 150, data = out$meta, init.type = "Spectral", seed = 300)
# # 
# save(stmFit, file = "data/stmFit_sotu.RData")

load("data/stmFit_sotu.RData")

# summary plot
plot(stmFit, 
     type = "summary", 
     xlim = c(0,.25), 
     n = 5, 
     labeltype = "prob",
     main = "Topics", 
     text.cex = 0.8)

# save topic information
topicNames <- labelTopics(stmFit, n = 5)
topic <- data.frame(
  TopicNumber = 1:k,
  TopicProportions = colMeans(stmFit$theta))

# plot(topic$TopicProportions,ylab="Topic Proportions",xlab="Topics")

# use 'estimateEffect' function
prep <- estimateEffect(1:k ~ s(decade), stmFit, meta = out$meta, uncertainty = "Global")

# compare decades
# define decades first
i <- 1890
j <- 1990

# define x axis label
xlab_str <- paste0("More Likely ", j,"                   Not Significant                    More Likely ",i)

Result <-plot(
  prep, covariate =
    "decade",
  model = stmFit,
  method = "difference",
  cov.value1 = i,
  cov.value2 = j,
  verbose.labels = F,
  ylab = "Expected Difference in Topic Probability by Decade (with 95% CI)",
  xlab = xlab_str,
  main = "Effect of Decade on Topic Prevalence",
  xlim = c(-.7, .7)
)

cloud(stmFit, topic = 4, scale = c(2,.25))
cloud(stmFit, topic = 14, scale = c(2,.25))

# summary(prep, topics=4)

# another plot
par(mfrow = c(1, 1), mar = c(4, 4, 2, 2))
i <- c(4, 14)
plot(
  prep,
  "decade",
  method = "continuous",
  topics = i,
  main = "Topics 4 and 14 by Decade",
  printlegend = T,
  ylab = "Exp. Topic Prob",
  xlab = "decade",
  ylim = c(-0.1, 0.70)
)

# topic correlations
mod.out.corr <- topicCorr(stmFit)
plot(mod.out.corr)

##################################################