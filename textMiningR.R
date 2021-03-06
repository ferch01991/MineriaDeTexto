library(NLP)
library(tm)
library(SnowballC)
library(RWeka)
library(hunspell)
library(lexRankr)
library(lsa)

# Para cambiar la codificacion de los archivos de texto
# file -bi test.txt ---- recode iso-8859-1..UTF-8 *.txt
# Quitar las tildes de los archivos de texto 
# sed -i 'y/áéíóú/aeiou/' *.txt 

setwd("//home//faherrera2//Documents//corpusPlanes//Abr2015 - Ago2015//ADMINISTRACIÓN EN BANCA Y FINANZAS")
options(mc.cores=1)
preprocesamientoDatos = function(corpusPlanes){
  # remover enlaces web 
  removeURL <- function(x) gsub("http[[:alnum:][:punct:]]*", "", x) 
  corpusPlanes = tm_map(corpusPlanes, content_transformer(removeURL))
  
  # Remover caracteres especiales
  removeCarateresEsp = function(x) gsub("[[:cntrl:]]", "", x)
  corpusPlanes = tm_map(corpusPlanes, content_transformer(removeCarateresEsp))
  
  #espacio a los signos de puntuacion palabra(palabra)
  espacio = content_transformer(function(x, pattern) gsub(pattern, " ", x))
  corpusPlanes = tm_map(corpusPlanes, espacio, "[[:punct:]]+")
  
  #Removemos los signos de puntuación
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
  #corpusPlanes = tm_map(corpusPlanes, stemDocument, language=c("spanish","english","french","german","italian"))
  
  #corpusPlanes = tm_map(corpusPlanes, stemDocument, language="english")
  #corpusPlanes = tm_map(corpusPlanes, stemDocument, language="french")
  #corpusPlanes = tm_map(corpusPlanes, stemDocument, language="german")
  #corpusPlanes = tm_map(corpusPlanes, stemDocument, language="italian")
  
  #remove de espacios dobles
  corpusPlanes = tm_map(corpusPlanes, stripWhitespace)
}
# corpus de datos
corpusP1T1 = VCorpus(DirSource("//home//faherrera2//Documents//corpusPlanes//Abr2015 - Ago2015/ADMINISTRACIÓN EN BANCA Y FINANZAS", "txt", encoding = "UTF-8"))
corpusPlanes = preprocesamientoDatos(corpusP1T1)


# https://github.com/bguvenc/LexRank/blob/master/LexRank.R
# http://slideplayer.com/slide/8123770/
# https://adamspannbauer.github.io/2017/12/17/summarizing-web-articles-with-r/

corpusPlanes = tm_map(corpusPlanes, stemDocument, language="spanish")
corpusLimpio <- corpusPlanes

matrizDTM <- DocumentTermMatrix(corpusLimpio, control = list(tokenize = NGramTokenizer))

matrizTF <- as.matrix(matrizDTM)
frecuencia <- matrizTF
frecuencia <- colSums(frecuencia)
frecuencia <- sort(subset(frecuencia, frecuencia >=8), decreasing = TRUE)

matrizDTM <- DocumentTermMatrix(corpusLimpio, control = list(dictionary = diccionario, tokenize = NGramTokenizer))
matrizTF <- as.matrix(matrizDTM)

matrizTF-IDF <- DocumentTermMatrix(corpusLimpio, control = list(weighting = weightTfIdf, normalize = TRUE))
matrizTF-IDF = removeSparseTerms(matrizTF-IDF, 0.95)



# Matriz de similaridad entre documetos
similaridadDoc <- cosine(t(matriz))


# Frecuencia de palabras
frecuencia <- matriz


#writeCorpus(corpusPlanes, path = "//home//faherrera2//Documents//planesProcesados")

tdmUni_matriz = as.matrix(tdmUni)

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm <- TermDocumentMatrix(corpusMatriz, control = list(tokenize = BigramTokenizer))




BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

inspect(stemDocument(corpusPlanes[[1]], language="spanish"))

inspect(corpusPlanes[[1]])

palabras = c("acádemico", "accion", "actual", "capitulo", "capítulo", "analiz", "globalización", "básico")
stem_words = stemDocument(palabras, language = "spanish")
stem_words = stemDocument(rownames(tdmUni_matriz), language = "spanish")
unique(stem_words)


#---------------------------------------------------
stem_words = stemDocument(c("complicación", "complicadamente", "complicado", "materiales", "academicamente"), language = "spanish")
stem_words
stemCompletion(stem_words, c("complicar", "material"))

esp = dictionary("es_ES")
words <- c("retroalimentación", "piscina", "veníamos", "escojió", "cap")
wordsB = hunspell_find(words, dict = esp)
wordsC = hunspell_suggest(unlist(wordsB), dict = esp)
words = c("retro-alimentación", "piscina", "veníamos", "escogió", "cap")
wordsD = hunspell_stem(words, dict = esp)

#Lista de palabras mal escritas
bad_words = list()
for(i in 1:length(corpusPlanes)){
  bad_words = unlist(c(bad_words, c(unique(unlist(hunspell(corpusPlanes[[i]]$content, format = "text", dict = esp))))))
}
suggest_words = hunspell_suggest(bad_words, dict = esp)
correct_words = c("actualmente", "análisis", "análisis", "", "aplicación", "approach", "aprobación", 
                  "autoevalaución","autoevaluación" ,"retroalimentación", "gerenciales", "eva", "sinópico", "práctico", 
                  "globalización", "capítulos", "problemas")


