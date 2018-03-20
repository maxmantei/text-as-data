# Ulrich Fritsche
# based on code by Gregor Wiedemann & Andreas Niekler
# Jan 18, 2018
#
# changed slightly by Maximillian Mantei @ 2018/03/20
#
# --------------------------------------
# Housekeeping (simplified by using R project)
# --------------------------------------

# set options
options(stringsAsFactors = F)

# load packages
library(readr) # uses UTF-8 by default
library(tidyverse) # order is important; let tm mask the "annotate" function (ggplot)
library(tm) # has to be loaded after tidyverse!
library(slam)

# ------------------------------------------------------------------------------

# Read in: separator is semikolon
textdata <- read_csv2("code/data/sotu.csv")
# we add some more metadata columns to the data frame
textdata <- textdata %>% 
  mutate(year = substr(date, 0, 4),  #construct a column with year information
         decade = paste0(substr(year, 0, 3), "0")) # use the first 3 numbers of year and add zero

# dimensions of the data frame
dim(textdata)

# column names of text and metadata
names(textdata)

# How many speeches per president?
textdata %>% count(president)

textdata <- textdata %>% rename("doc_id" = "id") %>% # rename
  select(c(1,5,2,3,4,6,7)) # reorder

corpus <- as.data.frame(textdata) %>% 
  DataframeSource() %>% 
  Corpus() # corpus formation takes texts and meta data

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

# sum columns for word counts (slam pkg)
freqs <- col_sums(DTM)
# get vocabulary vector
words <- colnames(DTM)
# combine words and their frequencies in a tibble
wordlist <- tibble(words, freqs)
# re-order the wordlist by decreasing frequency
wordlist <- wordlist %>% arrange(desc(freqs)) %>%
  mutate(rank = 1:n())

# show the most frequent words
head(wordlist, 25)  

# plot

ggplot(wordlist, aes(x = rank, y = freqs)) + 
  geom_line() +
  labs(title = "Rank frequency plot", 
       x = "Rank", 
       y ="Frequency") +
  theme_minimal()

ggplot(wordlist, aes(x = 1:nrow(wordlist), y = freqs)) + 
  geom_line() +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Rank frequency plot", 
       x = "Rank", 
       y = "Frequency") +
  theme_minimal()

# To illustrate the range of ranks best to be used for analysis, we augment information in the rank frequency plot. First, we mark so-called stop words. These are words of a language that normally do not contribute to semantic information about a text. In addition, all words in the word list are identified which occur less than 10 times.
# # The %in% operator can be used to compare which elements of the first vector are contained in the second vector. At this point, we compare the words in the word list with a loaded stopword list (retrieved by the function stopwords of the tm package) . The result of the %in% operator is a boolean vector which contains TRUE or FALSE values.
# # A boolean value (or a vector of boolean values) can be inverted with the ! operator (TRUE gets FALSE and vice versa). The which command returns the indices of entries in a boolean vector which contain the value TRUE.
# # We also compute indices of words, which occur less than 10 times. With a union set operation, we combine both index lists. With a setdiff operation, we reduce a vector of all indices (the sequence 1:nrow(wordlist)) by removing the stopword indices and the low freuent word indices.
# # With the command “lines” the range of the remining indices can be drawn into the plot.

meaningful <- filter(wordlist, !(words %in% stopwords("en") | freqs < 10))

ggplot(wordlist, aes(x = rank, y = freqs)) + 
  geom_line() +
  geom_point(data = meaningful, 
             color = "darkgreen") +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Rank frequency plot", 
       x = "Rank", 
       y = "Frequency") +
  theme_minimal()

# # additional exercises
# wordlist2 <- wordlist[meaningful_range_idx, ] 
# head(wordlist2,25)
# # head(wordlist$words[meaningful_range_idx],200)
# 
# 
# # wordlist$freqs == 1 gives a vector of False / True values , T is counted as 1, summing over it gives the number we need
# sum(wordlist$freqs == 1)/ncol(DTM)

# preprocessing steps on corpus
corpus <- corpus %>% 
  tm_map(removePunctuation, preserve_intra_word_dashes = TRUE) %>%
  tm_map(removeNumbers) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("en")) %>% # time intensive
  tm_map(stripWhitespace)

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
counts_per_decade <- aggregate(DTM_reduced, by = list(decade = textdata$decade), sum)

counts_per_decade %>% gather(word, freq, -decade) %>%
  ggplot(aes(x = decade, y = freq, group = word)) +
  labs(title = "Word frequencies over time", y = "frequency") +
  geom_line(aes(color = word)) + # maybe add ", linetype = word" in aes (but looks bad)
  theme_minimal()

# grouping of semantic categories
positive_terms_all <- readLines("code/data/senti_words_positive.txt")
negative_terms_all <- readLines("code/data/senti_words_negative.txt")

# We first need to restrict the list to those words actually occurring in our speeches. 
# These terms then can be aggregated per speech by a simple row_sums command.
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
# prepare data for ggplot
df <- gather(sentiments_per_president, sentiment, value, -president)

ggplot(data = df, aes(x = president, y = value, fill = sentiment)) + 
  geom_bar(stat="identity", position = position_dodge()) + 
  coord_flip() +
  theme_minimal()

# order by positive sentiments
df %>%
  arrange(desc(value)) %>%
  mutate(president = forcats::fct_inorder(president)) %>%
  ggplot(aes(x = forcats::fct_rev(president), y = value, fill = sentiment)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  coord_flip() +
  theme_minimal()

# order by negative sentiments
df %>%
  arrange(value) %>%
  mutate(president = forcats::fct_inorder(president)) %>%
  ggplot(aes(x = president, y = value, fill = sentiment)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  coord_flip() +
  theme_minimal()

# heatmap graph, needs a matrix object as input
terms_to_observe <- c("war", "peace", "civil", "terror", "islam", 
                      "threat", "security", "conflict", "together", 
                      "friend", "enemy", "afghanistan", "muslim", 
                      "germany", "world", "duty", "frontier", "north",
                      "south", "black", "racism", "slavery")
DTM_reduced <- as.matrix(DTM[, terms_to_observe])
rownames(DTM_reduced) <- ifelse(as.integer(textdata$year) %% 2 == 0, textdata$year, "")

heatmap(t(DTM_reduced), Colv=NA, col = rev(terrain.colors(256)), keep.dendro= FALSE, margins = c(5, 5))
# without hierarchical clusters:
# heatmap(t(DTM_reduced), Colv=NA, Rowv= NA, col = rev(terrain.colors(256)), keep.dendro= FALSE, margins = c(5, 5))
