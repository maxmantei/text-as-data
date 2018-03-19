# Ulrich Fritsche
# based on code by Gregor Wiedemann and Andreas Niekler, GESIS, 2017
# Jan 16, 2018

# --------------------------------------
# Housekeeping & Path
# --------------------------------------

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

# Due to some changes in tm package, we have to bring the data into a specific order. First column = "doc_id", second column = "text", other columns are metadata

names(textdata)[names(textdata) == "id"] <- "doc_id"
textdata <- textdata[c(1,5,2,3,4)]

# Create corpus object

corpus <- Corpus(DataframeSource(textdata))

# Function 1: Sentence as a base for analysis

require(openNLP)

# Function to convert a document in a vector of sentences

convert_text_to_sentences <- function(text, lang = "en", SentModel = "resources/en-sent.bin") {
  
  # Calculate sentenve boundaries as annotation with Apache OpenNLP Maxent-sentence-detector. This is external to R. 
  sentence_token_annotator <- Maxent_Sent_Token_Annotator(language = lang, model = SentModel)
  
  # Convert text to NLP string
  text <- NLP::as.String(text) # "NLP::" calls an NLP specific function!
  
  # Annotate the sentence boundaries
  sentenceBoundaries <- NLP::annotate(text, sentence_token_annotator) # same
  
  # Select sentences as rows of a new matrix
  sentences <- text[sentenceBoundaries]
  
  # return the sentences
  return(sentences)
}

# Function (2) to convert a corpus of documents into a corpus of single sentences from documents
reshape_corpus <- function(currentCorpus, ...) {
  
  # Extraction of all sentences from the corpus as a list
  text <- lapply(currentCorpus, as.character)
  
  # convert the text
  pb <- txtProgressBar(min=0, max=length(text))
  i <- 0
  docs <- lapply(text, FUN=function(x){
    i <<- i + 1
    setTxtProgressBar(pb, i)
    convert_text_to_sentences(x)
  }, ...)
  close(pb)
  
  docs <- as.vector(unlist(docs))
  
  # Create a new corpus of the segmented sentences
  newCorpus <- Corpus(VectorSource(docs))
  
  return(newCorpus)
}

# Look into your memory space: both functions have to be there (at the very end) as object type "function"

# just look that everything is ok

length(corpus)
substr(as.character(corpus[[1]]), 0, 200)

# apply functions
# reshape corpus into sentences

sentenceCorpus <- reshape_corpus(corpus)

# reshaped corpus length and its first 'document'
length(sentenceCorpus)
as.character(sentenceCorpus[[1]])

# Preprocessing chain
sentenceCorpus <- tm_map(sentenceCorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
sentenceCorpus <- tm_map(sentenceCorpus, removeNumbers)
sentenceCorpus <- tm_map(sentenceCorpus, content_transformer(tolower))
sentenceCorpus <- tm_map(sentenceCorpus, removeWords, english_stopwords) # here we use the external list
sentenceCorpus <- tm_map(sentenceCorpus, stripWhitespace) # eliminate white spaces

# 
# Additionally, we are interested in the joint occurrence of words in a sentence. For this, we do not need the exact count of how often the terms occur, but only the information whether they occur together or not. This can be encoded in a binary document-term-matrix. The parameter weighting in the control options calls the weightBin function. This writes a 1 into the DTM if the term is contained in a sentence and 0 if not.

minimumFrequency <- 10
binDTM <- DocumentTermMatrix(sentenceCorpus, control=list(bounds = list(global=c(minimumFrequency, Inf)), weighting = weightBin))

# Convert to sparseMatrix matrix
# What does sparse mean here?

require(Matrix)
binDTM <- sparseMatrix(i = binDTM$i, j = binDTM$j, x = binDTM$v, dims = c(binDTM$nrow, binDTM$ncol), dimnames = dimnames(binDTM))

# Matrix multiplication for cooccurrence counts
coocCounts <- t(binDTM) %*% binDTM # adjacency matrix

# Let’s look at a snippet of the result. The matrix has nTerms rows and columns and is symmetric. Each cell contains the number of joint occurrences. In the diagonal, the frequencies of single occurrences of each term are encoded.

as.matrix(coocCounts[202:205, 202:205])

# In order to not only count joint occurrence we have to determine their significance. Different significance-measures can be used. We need also various counts to calculate the significance of the joint occurrence of a term i (coocTerm) with any other term j: * k - Number of all context units in the corpus * ki - Number of occurrences of coocTerm * kj - Number of occurrences of comparison term j * kij - Number of joint occurrences of coocTerm and j
# These quantities can be calculated for any term coocTerm as follows:

coocTerm <- "spain"
k <- nrow(binDTM)
ki <- sum(binDTM[, coocTerm])
kj <- colSums(binDTM)
names(kj) <- colnames(binDTM)
kij <- coocCounts[coocTerm, ]

########## MI: log(k*kij / (ki * kj) ########
mutualInformationSig <- log(k * kij / (ki * kj))
mutualInformationSig <- mutualInformationSig[order(mutualInformationSig, decreasing = TRUE)]

########## DICE: 2 X&Y / X + Y ##############
dicesig <- 2 * kij / (ki + kj)
dicesig <- dicesig[order(dicesig, decreasing=TRUE)]

########## Log Likelihood ###################
logsig <- 2 * ((k * log(k)) - (ki * log(ki)) - (kj * log(kj)) + (kij * log(kij)) 
               + (k - ki - kj + kij) * log(k - ki - kj + kij) 
               + (ki - kij) * log(ki - kij) + (kj - kij) * log(kj - kij) 
               - (k - ki) * log(k - ki) - (k - kj) * log(k - kj))
logsig <- logsig[order(logsig, decreasing=T)]

# Put all significance statistics in one Data-Frame
resultOverView <- data.frame(
  names(sort(kij, decreasing=T)[1:10]), sort(kij, decreasing=T)[1:10],
  names(mutualInformationSig[1:10]), mutualInformationSig[1:10], 
  names(dicesig[1:10]), dicesig[1:10], 
  names(logsig[1:10]), logsig[1:10],
  row.names = NULL)
colnames(resultOverView) <- c("Freq-terms", "Freq", "MI-terms", "MI", "Dice-Terms", "Dice", "LL-Terms", "LL")
print(resultOverView)


# Read in the source code for the co-occurrence calculation
source("calculateCoocStatistics.R")
# Definition of a parameter for the representation of the co-occurrences of a concept
numberOfCoocs <- 15
# Determination of the term of which co-competitors are to be measured.
coocTerm <- "freedom"
#coocTerm <- "california"
#coocTerm <- "trust"
coocs <- calculateCoocStatistics(coocTerm, binDTM, measure="LOGLIK")

# Display the numberOfCoocs main terms
print(coocs[1:numberOfCoocs])

# Produce iGraph graphs

resultGraph <- data.frame(from = character(), to = character(), sig = numeric(0))

# The structure of the temporary graph object is equal to that of the resultGraph
tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))

# Fill the data.frame to produce the correct number of lines
tmpGraph[1:numberOfCoocs, 3] <- coocs[1:numberOfCoocs]
# Entry of the search word into the first column in all lines
tmpGraph[, 1] <- coocTerm
# Entry of the co-occurrences into the second column of the respective line
tmpGraph[, 2] <- names(coocs)[1:numberOfCoocs]
# Set the significances
tmpGraph[, 3] <- coocs[1:numberOfCoocs]

# Attach the triples to resultGraph
resultGraph <- rbind(resultGraph, tmpGraph)

# Iteration over the most significant numberOfCoocs co-occurrences of the search term
for (i in 1:numberOfCoocs){
  
  # Calling up the co-occurrence calculation for term i from the search words co-occurrences
  newCoocTerm <- names(coocs)[i]
  coocs2 <- calculateCoocStatistics(newCoocTerm, binDTM, measure="LOGLIK")
  
  #print the co-occurrences
  coocs2[1:10]
  
  # Structure of the temporary graph object
  tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
  tmpGraph[1:numberOfCoocs, 3] <- coocs2[1:numberOfCoocs]
  tmpGraph[, 1] <- newCoocTerm
  tmpGraph[, 2] <- names(coocs2)[1:numberOfCoocs]
  tmpGraph[, 3] <- coocs2[1:numberOfCoocs]
  
  #Append the result to the result graph
  resultGraph <- rbind(resultGraph, tmpGraph[2:length(tmpGraph[, 1]), ])
}

#resultGraph <- data.frame(from = character(), to = character(), sig = numeric(0))

# Sample of some examples from resultGraph
resultGraph[sample(nrow(resultGraph), 6), ]

require(igraph)

# Set the graph and type. In this case, "F" means "Force Directed"
graphNetwork <- graph.data.frame(resultGraph, directed = F)

# Identification of all nodes with less than 2 edges
graphVs <- V(graphNetwork)[degree(graphNetwork) < 2]
# These edges are removed from the graph
graphNetwork <- delete.vertices(graphNetwork, graphVs) 

# Assign colors to edges and nodes (searchterm blue, rest orange)
V(graphNetwork)$color <- ifelse(V(graphNetwork)$name == coocTerm, 'cornflowerblue', 'orange') 

# Edges with a significance of at least 50% of the maximum sig- nificance in the graph are drawn in orange
halfMaxSig <- max(E(graphNetwork)$sig) * 0.5
E(graphNetwork)$color <- ifelse(E(graphNetwork)$sig > halfMaxSig, "coral", "azure3")

# Disable edges with radius
E(graphNetwork)$curved <- 0 
# Size the nodes by their degree of networking
V(graphNetwork)$size <- log(degree(graphNetwork)) * 5

# All nodes must be assigned a standard minimum-size
V(graphNetwork)$size[V(graphNetwork)$size < 5] <- 3 

# edge thickness
E(graphNetwork)$width <- 2

# Define the frame and spacing for the plot
par(mai=c(0,0,1,0)) 

# Final plot
plot(graphNetwork,              
     layout = layout.fruchterman.reingold,  # Force Directed Layout 
     main = paste(coocTerm, ' Graph'),
     vertex.label.family = "sans",
     vertex.label.cex = 0.8,
     vertex.shape = "circle",
     vertex.label.dist = 0.5,           # Labels of the nodes moved slightly
     vertex.frame.color = 'darkolivegreen',
     vertex.label.color = 'black',      # Color of node names
     vertex.label.font = 2,         # Font of node names
     vertex.label = V(graphNetwork)$name,       # node names
     vertex.label.cex = 1 # font size of node names 
)