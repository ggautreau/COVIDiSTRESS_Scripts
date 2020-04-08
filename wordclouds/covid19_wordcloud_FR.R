#!/usr/bin/env Rscript

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

# inspired from http://www.sthda.com/french/wiki/text-mining-et-nuage-de-mots-avec-le-logiciel-r-5-etapes-simples-a-savoir

data = read.csv("covid_06042020_numerical_values.csv", header=T, stringsAsFactors=F)
data = data[3:nrow(data),]

docs <- Corpus(VectorSource(as.character(unique(data_true[data_true$UserLanguage=="FR","Final_open"]))))
#remove special characters and numbers
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("french"))#remove most of the not significant french word
to_exclude <- c("plus", "jai", "fait", "faire", "bien", "très", "tout", "être", 
"chez", "moins", "cest", "ça", "depuis", "donc", "comme", "avoir", 
"car", "avant", "vis", "quand", "aussi", "dun", "dêtre", "alors", 
"pendant", "prendre", "encore", "tous", "nest", "non", "j’ai", 
"fois", "peux", "davoir", "peut", "fais", "nai", "où", "ans", 
"assez", "mieux", "quil", "etc", "face", "trop", "dune", "déjà", 
"entre", "cause", "dont", "fortement", "certains", "aucune", 
"rester", "devoir", "rien", "cas", "chose", "jamais", "après", 
"loin", "notamment", "autre", "tant", "besoin", "permet", "plusieurs", 
"c’est", "deux", "part", "grande")#other not significant french word
docs <- tm_map(docs, removeWords, to_exclude)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

set.seed(length("geologue"))

png("covid_wordcloud_FR")
wordcloud(words = d$word, freq = d$freq, min.freq = 1
max.words=400, random.order=FALSE, rot.per=0.35,
colors=brewer.pal(8, "Dark2"))
dev.off()