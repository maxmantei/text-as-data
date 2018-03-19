# Ulrich Fritsche
# based on code by Gregor Wiedemann & Andreas Niekler
# Jan 18, 2018
# --------------------------------------
# Housekeeping
# --------------------------------------

rm(list=ls())

# important option for text analysis
options(stringsAsFactors = F)

# check working directory. It should be the destination folder of the extracted 
# zip file. If necessary, use `setwd("your-tutorial-folder-path")` to change it.

setwd("C:/Users/FritscheU/Dropbox/RCourse_Text_Mining/Code")

getwd() # check that the folder is correct

# ---------------------------------------
# Read in: separator is semikolon
textdata <- read.csv("data/sotu.csv", header = TRUE, sep = ";", encoding = "UTF-8")
# we add some more metadata columns to the data frame
textdata$year <- substr(textdata$date, 0, 4) #construct a column with year information
textdata$decade <- paste0(substr(textdata$date, 0, 3), "0") # use the first 3 numbers of year and add zero

# dimensions of the data frame
dim(textdata)

# column names of text and metadata
colnames(textdata)

# How many speeches per president?
table(textdata[, "president"])

# package load
library("tm")

names(textdata)[names(textdata) == "id"] <- "doc_id" #rename
textdata <- textdata[c(1,5,2,3,4,6,7)] #reorder

corpus <- Corpus(DataframeSource(textdata)) # corpus formation takes texts and meta data
# have a look on the new corpus object
corpus

# accessing a single document object
corpus[[1]]
# getting its text content
cat(as.character(corpus[[1]]))

# Create a DTM (may take a while)
DTM <- DocumentTermMatrix(corpus)
# Show some information
DTM
# Dimensionality of the DTM
dim(DTM)

library("slam")

# sum columns for word counts
freqs <- col_sums(DTM)
# get vocabulary vector
words <- colnames(DTM)
# combine words and their frequencies in a data frame
wordlist <- data.frame(words, freqs)
# re-order the wordlist by decreasing frequency
wordIndexes <- order(wordlist[, "freqs"], decreasing = TRUE) # gives an index 
wordlist <- wordlist[wordIndexes, ] 
# show the most frequent words
head(wordlist, 25)  

# plot

plot(wordlist$freqs , type = "l", lwd=2, main = "Rank frequency plot", xlab="Rank", ylab ="Frequency")
plot(wordlist$freqs , type = "l", log="xy", lwd=2, main = "Rank frequency plot", xlab="log-Rank", ylab ="log-Frequency")

# To illustrate the range of ranks best to be used for analysis, we augment information in the rank frequency plot. First, we mark so-called stop words. These are words of a language that normally do not contribute to semantic information about a text. In addition, all words in the word list are identified which occur less than 10 times.
# # The %in% operator can be used to compare which elements of the first vector are contained in the second vector. At this point, we compare the words in the word list with a loaded stopword list (retrieved by the function stopwords of the tm package) . The result of the %in% operator is a boolean vector which contains TRUE or FALSE values.
# # A boolean value (or a vector of boolean values) can be inverted with the ! operator (TRUE gets FALSE and vice versa). The which command returns the indices of entries in a boolean vector which contain the value TRUE.
# # We also compute indices of words, which occur less than 10 times. With a union set operation, we combine both index lists. With a setdiff operation, we reduce a vector of all indices (the sequence 1:nrow(wordlist)) by removing the stopword indices and the low freuent word indices.
# # With the command “lines” the range of the remining indices can be drawn into the plot.

plot(wordlist$freqs, type = "l", log="xy",lwd=2, main = "Rank frequency plot", xlab="Rank", ylab = "Frequency")
englishStopwords <- stopwords("en")
stopwords_idx <- which(wordlist$words %in% englishStopwords)
low_frequent_idx <- which(wordlist$freqs < 10)
insignificant_idx <- union(stopwords_idx, low_frequent_idx)
meaningful_range_idx <- setdiff(1:nrow(wordlist), insignificant_idx)
lines(meaningful_range_idx, wordlist$freqs[meaningful_range_idx], col = "green", lwd=2, type="p", pch=20)


# # additional exercises
# wordlist2 <- wordlist[meaningful_range_idx, ] 
# head(wordlist2,25)
# # head(wordlist$words[meaningful_range_idx],200)
# 
# 
# # wordlist$freqs == 1 gives a vector of False / True values , T is counted as 1, summing over it gives the number we need
# sum(wordlist$freqs == 1)/ncol(DTM)

# preprocessing steps on corpus

corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = TRUE)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeWords, stopwords("en")) # time intensive
corpus <- tm_map(corpus, stripWhitespace)

# check out

as.character(corpus[[1]])

# create again a DTM

DTM <- DocumentTermMatrix(corpus)
dim(DTM)

# time series
terms_to_observe <- c("nation", "war", "god", "terror", "security")

# The reduced DTM contains counts for each of our 5 terms and in each of the 231 documents (rows of the reduced DTM). Since our corpus covers a time span of more than 200 years, we could aggregate frequencies in single documents per decade to get a meaningful representation of term frequencies over time.
# Information of each decade per document we added in the beginning to the textdata variable. There are ten speeches per decade mostly (you can check with table(textdata$decade). We use textdata$decade as a grouping parameter for the aggregate function. This function sub-selects rows from the input data (DTM_reduced) for all different decade values given in the by-parameter. Each sub-selection is processed column-wise using the function provided in the third parameter (sum).

DTM_reduced <- as.matrix(DTM[, terms_to_observe])

# Frequencies per decade

counts_per_year <- aggregate(DTM_reduced, by = list(decade = textdata$decade), sum)

# give x and y values beautiful names
decades <- counts_per_year$decade
frequencies <- counts_per_year[, terms_to_observe]

# plot multiple frequencies
matplot(decades, frequencies, type = "l")

# add legend to the plot
l <- length(terms_to_observe)
legend('topleft', legend = terms_to_observe, col=1:l, text.col = 1:l, lty = 1:l)  

# grouping of semantic categories

positive_terms_all <- readLines("data/senti_words_positive.txt")
negative_terms_all <- readLines("data/senti_words_negative.txt")

library("slam")

# we first need to restrict the list to those words actually occurring in our speeches. These terms then can be aggregated per speech by a simple row_sums command.

positive_terms_in_suto <- intersect(colnames(DTM), positive_terms_all)
counts_positive <- row_sums(DTM[, positive_terms_in_suto])

negative_terms_in_suto <- intersect(colnames(DTM), negative_terms_all)
counts_negative <- row_sums(DTM[, negative_terms_in_suto])

counts_all_terms <- row_sums(DTM)

relative_sentiment_frequencies <- data.frame(
  positive = counts_positive / counts_all_terms,
  negative = counts_negative / counts_all_terms
)

sentiments_per_president <- aggregate(relative_sentiment_frequencies, by = list(president = textdata$president), mean)

head(sentiments_per_president)

# use ggplot

library("reshape2") # we need this for melting, sentiment_per_president is "reshaped"
df <- melt(sentiments_per_president, id.vars = "president")

library("ggplot2") # advanced graphs
ggplot(data = df, aes(x = president, y = value, fill = variable)) + 
  geom_bar(stat="identity", position=position_dodge()) + coord_flip()

# order by positive sentiments
ggplot(data = df, aes(x = reorder(president, df$value, head, 1), y = value, fill = variable)) + geom_bar(stat="identity", position=position_dodge()) + coord_flip()

# order by negative sentiments
ggplot(data = df, aes(x = reorder(president, df$value, tail, 1), y = value, fill = variable)) + geom_bar(stat="identity", position=position_dodge()) + coord_flip()

# heatmap graph, needs a matrix object as input

terms_to_observe <- c("war", "peace", "civil", "terror", "islam", 
                      "threat", "security", "conflict", "together", 
                      "friend", "enemy", "afghanistan", "muslim", 
                      "germany", "world", "duty", "frontier", "north",
                      "south", "black", "racism", "slavery")
DTM_reduced <- as.matrix(DTM[, terms_to_observe])
rownames(DTM_reduced) <- ifelse(as.integer(textdata$year) %% 2 == 0, textdata$year, "")

heatmap(t(DTM_reduced), Colv=NA, col = rev(terrain.colors(256)), keep.dendro= FALSE, margins = c(5, 5))

# heatmap(t(DTM_reduced), Colv=NA, Rowv= NA, col = rev(terrain.colors(256)), keep.dendro= FALSE, margins = c(5, 5))
