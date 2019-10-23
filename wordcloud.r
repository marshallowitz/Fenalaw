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

# Convers�o texto para caixa baixa
docs <- tm_map(docs, content_transformer(tolower))
# Remo��o de n�meros
docs <- tm_map(docs, removeNumbers)
# Remo��o de stopwords
docs <- tm_map(docs, removeWords, stopwords("portuguese"))
# Remo��o de stopwords pr�prias
# Como vetor
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remo��o de pontua��es
docs <- tm_map(docs, removePunctuation)
# Elimina��o de espa�os em branco extra
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