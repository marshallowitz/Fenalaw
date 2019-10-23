library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")


text <- d_cjsg$ementa
docs <- Corpus(VectorSource(text))
inspect(docs)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Conversão texto para caixa baixa
docs <- tm_map(docs, content_transformer(tolower))
# Remoção de números
docs <- tm_map(docs, removeNumbers)
# Remoção de stopwords
docs <- tm_map(docs, removeWords, stopwords("portuguese"))
# Remoção de stopwords próprias
# Como vetor
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remoção de pontuações
docs <- tm_map(docs, removePunctuation)
# Eliminação de espaços em branco extra
docs <- tm_map(docs, stripWhitespace)
# Text stemming
docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))