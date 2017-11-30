#webScraping using 'rvest' and visualizing 'tidytext' df
getwd()
rm(list = ls())

library(rvest)
packageDescription("rvest") #Hadley Wickham spearheading the package
url <- read_html('https://en.wikipedia.org/wiki/Hadley_Wickham')
nodes <- html_nodes(url, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "mw-parser-output", " " ))] | //li')
text <- html_text(nodes)

text

#corpus
library(tm)
corpus <- Corpus(VectorSource(text))
#dtm
dtm <- DocumentTermMatrix(corpus, control = list(tolower = T,
                                                 removeNumbers = T,
                                                 removePunctuation = T,
                                                 stopwords = T,
                                                 stripWhitespace = T,
                                                 stemDocument = T))

inspect(dtm)
dim(dtm)
dtm$dimnames #spits out all terms of the documents
head(Terms(dtm), n=20) #spits out the first 20 terms

#tidytext
library(tidytext)
df <- tidy(dtm)
View(df)

unique(df$document)
length(unique(df$document))
max(df$count)

#Visualize using drlib
library(drlib)
library(dplyr)
library(ggplot2)

df %>%
  group_by(document) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(document = factor(as.numeric(document), levels = 1:17)) %>%
  ggplot(aes(drlib::reorder_within(term, count, document), count)) +
  geom_bar(stat = "identity") +
  xlab("Top 5 Common Words") +
  drlib::scale_x_reordered() +
  coord_flip()+
  facet_wrap(~ document, scales = "free")
