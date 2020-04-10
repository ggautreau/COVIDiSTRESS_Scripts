#!/usr/bin/env Rscript

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

# inspired from http://www.sthda.com/french/wiki/text-mining-et-nuage-de-mots-avec-le-logiciel-r-5-etapes-simples-a-savoir

data = read.csv("../covid_06042020_numerical_values.csv", header=T, stringsAsFactors=F)

#choose your language
language="english"
language2letters="EN"

data = data[3:nrow(data),]

docs <- Corpus(VectorSource(as.character(unique(data[data$UserLanguage==language2letters,"Final_open"]))))
#remove special characters and numbers
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords(language))#remove most of the not significant words
to_exclude <- c()#other not significant words
docs <- tm_map(docs, removeWords, to_exclude)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

set.seed(42)

png(paste0("covid_wordcloud_",language2letters,".png"))
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=400, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
dev.off()