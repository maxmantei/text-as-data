# Author: Ulrich Fritsche
# based on code by Gregor Wiedemann and Andreas Niekler
# Jan 16, 2018
#
# changed slightly by Maximillian Mantei @ 2018/19/03
#
# --------------------------------------
# Housekeeping & path (easier with R projects)
# --------------------------------------

# set options
options(stringsAsFactors = F)

# load packages
library(rvest)
library(readr)
library(readtext)
library(dplyr)

#-------------------------------------------------------------------------------

# Exercise 1: Grab data from a newspaper
url <- "https://www.theguardian.com/world/2017/jun/26/angela-merkel-and-donald-trump-head-for-clash-at-g20-summit"
html_document <- read_html(url)

# Select the headline, first define path
title_xpath <- "//h1[contains(@class, 'content__headline')]"
title_text <- html_document %>% # pipe operator! Remember magrittr from R course!
  html_node(xpath = title_xpath) %>% # use XPATH to extract in a node
  html_text(trim = T) # extract text

# check that it worked out!
cat(title_text)

# now we do the same with other parts!
intro_xpath <- "//div[contains(@class, 'content__standfirst')]//p"
intro_text <- html_document %>%
  html_node(xpath = intro_xpath) %>%
  html_text(trim = T)

# check out
cat(intro_text)

# Now for the text body
body_xpath <- "//div[contains(@class, 'content__article-body')]//p" # all paragraphs
body_text <- html_document %>%
  html_nodes(xpath = body_xpath) %>% # plural!
  html_text(trim = T) %>%
  paste0(collapse = "\n") # collapse the paragraphs
cat(body_text)

# For metadata we possibly need the time dimension
date_xpath <- "//time"
date_object <- html_document %>%
  html_node(xpath = date_xpath) %>%
  html_attr(name = "datetime") %>% #attribute
  as.Date()

# show the date object in a appropriate format
cat(format(date_object, "%Y-%m-%d"))

#-------------------------------------------------------------------------------

# Exercise 2: Follow a series of documents & download and parse a tag overview page
url <- "https://www.theguardian.com/world/angela-merkel"
html_document <- read_html(url)
links <- html_document %>%
  html_nodes(xpath = "//div[contains(@class, 'fc-item__container')]/a") %>%
  html_attr(name = "href")
head(links, 3)

# put together page numbers
page_numbers <- 1:3
base_url <- "https://www.theguardian.com/world/angela-merkel?page="
paging_urls <- paste0(base_url, page_numbers)

# View first 3 urls
head(paging_urls, 3)
all_links <- NULL
for (url in paging_urls) {
  # download and parse single ta overview page
  html_document <- read_html(url)
  
  # extract links to articles
  links <- html_document %>%
    html_nodes(xpath = "//div[contains(@class, 'fc-item__container')]/a") %>%
    html_attr(name = "href")
  
  # append links to vector of all links
  all_links <- c(all_links, links)
}

# view all the links
all_links %>% paste0("\n") %>% cat

# building a function for scraping guardian articles using the piping technique
scrape_guardian_article <- function(url) {
  html_document <- read_html(url)
  
  title_xpath <- "//h1[contains(@class, 'content__headline')]"
  title_text <- html_document %>%
    html_node(xpath = title_xpath) %>%
    html_text(trim = T)
  
  intro_xpath <- "//div[contains(@class, 'content__standfirst')]//p"
  intro_text <- html_document %>%
    html_node(xpath = intro_xpath) %>%
    html_text(trim = T)
  
  body_xpath <- "//div[contains(@class, 'content__article-body')]//p"
  body_text <- html_document %>%
    html_nodes(xpath = body_xpath) %>%
    html_text(trim = T) %>%
    paste0(collapse = "\n")
  
  date_xpath <- "//time"
  date_text <- html_document %>%
    html_node(xpath = date_xpath) %>%
    html_attr(name = "datetime") %>%
    as.Date()
  
  article <- data.frame(
    url = url,
    date = date_text,
    title = title_text,
    body = paste0(intro_text, "\n", body_text)
  )
  return(article)
}

# scraping articles
all_articles <- data.frame()
for (i in 1:length(all_links)) {
  cat("Downloading", i, "of", length(all_links), "URL:", all_links[i], "\n")
  article <- scrape_guardian_article(all_links[i])
  # Append current article data.frame to the data.frame of all articles
  all_articles <- rbind(all_articles, article)
}

# View first articles
head(all_articles, 3)

# Write articles to disk
write_csv(all_articles, path = "code/data/guardian_merkel.csv")

#-------------------------------------------------------------------------------

# Exercise 3: 
# Extract text from various file formats
# PDF from folder
data_files <- list.files(path = "code/data/documents", full.names = T, recursive = T) # full name preserved 

# View first file paths
head(data_files, 3)
extracted_texts <- readtext(data_files, docvarsfrom = "filepaths", dvsep = "/")

# View first rows of the extracted texts
head(extracted_texts)
extracted_texts <- select(extracted_texts,  -c(docvar1:docvar3))

# View beginning of tenth extracted text
substr(trimws(extracted_texts$text[10]) , 0, 100)

# write to csv
write_csv(extracted_texts, path = "code/data/text_extracts.csv")
