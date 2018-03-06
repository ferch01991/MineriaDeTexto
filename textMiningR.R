library(NLP)
library(tm)
library(SnowballC)
library(RWeka)
library(hunspell)
library(pdftools)

setwd("//home//faherrera2//Dropbox//Tesis//corpusPlanes//Abr2015 - Ago2015//ADMINISTRACIÓN DE EMPRESAS")
options(mc.cores=1)
preprocesamientoDatos = function(corpusPlanes){
  # remover enlaces web 
  removeURL <- function(x) gsub("http[[:alnum:][:punct:]]*", "", x) 
  corpusPlanes = tm_map(corpusPlanes, content_transformer(removeURL))
  
  #espacio a los signos de puntuacion palabra(palabra)
  espacio = content_transformer(function(x, pattern) gsub(pattern, " ", x))
  corpusPlanes = tm_map(corpusPlanes, espacio, "[[:punct:]]+")
  
  #Removemos los signos de puntuaci??n
  corpusPlanes = tm_map(corpusPlanes, removePunctuation)
  #Removemos los numeros
  corpusPlanes = tm_map(corpusPlanes, removeNumbers)
  #convertimos las letras a minusculas
  corpusPlanes = tm_map(corpusPlanes, content_transformer(tolower))
  #removemos los stopwords
  corpusPlanes = tm_map(corpusPlanes, removeWords, stopwords("spanish"))
  corpusPlanes = tm_map(corpusPlanes, removeWords, stopwords("english"))
  corpusPlanes = tm_map(corpusPlanes, removeWords, stopwords("french"))
  corpusPlanes = tm_map(corpusPlanes, removeWords, stopwords("german"))
  corpusPlanes = tm_map(corpusPlanes, removeWords, stopwords("italian"))
  corpusPlanes = tm_map(corpusPlanes, removeWords, stopwords("stopwords"))
  #remove de caracteres
  #corpusPlanes = tm_map(corpusPlanes, removeWords, c("i", "ii", "NA", "iii"))
  
  #stemming reduce una palabra a su raiz
  corpusPlanes = tm_map(corpusPlanes, stemDocument, language="spanish")
  #corpusPlanes = tm_map(corpusPlanes, stemDocument, language="english")
  #corpusPlanes = tm_map(corpusPlanes, stemDocument, language="french")
  #corpusPlanes = tm_map(corpusPlanes, stemDocument, language="german")
  #corpusPlanes = tm_map(corpusPlanes, stemDocument, language="italian")
  # remove de espacios dobles
  corpusPlanes = tm_map(corpusPlanes, stripWhitespace)
}

corpusP1T1 = VCorpus(DirSource("//home//faherrera2//Dropbox//Tesis//corpusPlanes//Abr2015 - Ago2015/ADMINISTRACIÓN DE EMPRESAS", "txt", encoding = "UTF-8"))


corpusPlanes = preprocesamientoDatos(corpusP1T1)
writeCorpus(corpusPlanes, path = "//home//faherrera2//Documents//planesProcesados")

corpusMatriz = corpusPlanes
tdmUni = TermDocumentMatrix(corpusMatriz)



BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

tdm <- TermDocumentMatrix(corpusMatriz, control = list(tokenize = BigramTokenizer))

inspect(tdm)
tdm$dimnames$Terms[20:60]
matriz = as.matrix(tdm)

hunspell(corpusP1T1[[1]], format = "text")

stopWord = hunspell_parse(readLines(list.files()[1], encoding = "UTF-8"))
for(document in list.files()){
  
}

palabras = tm_map(c("árboles", "caminando", "busqueda"), stemDocument)
