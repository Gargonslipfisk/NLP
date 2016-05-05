#Ruta del directorio de trabajo
setwd("~/Erreria/Topic Modelling")
#Importa corpus como caracteres
Hoteles_raw = readLines("Hotels.csv", encoding = "ANSI")
#Convierte corpus a matriz
Hoteles = as.matrix(Hoteles_raw)

library(textcat)

c <- textcat(Hoteles)
# z <- cbind(Hoteles,c)
# names(z)<- c("verbatim","lenguaje")
# x<-as.data.frame(t(z))
# x$lenguajes
# z["c"]
row_to_keep <- ifelse(c=="english", TRUE, FALSE)
Hoteles_2 <- Hoteles[row_to_keep,]
# View(Hoteles)
Hoteles <- as.matrix(Hoteles_2)

# #Limpieza de NA
# a <- character()
# for (i in 1:nrow(Hoteles))
#   if (!is.na(textcat(Hoteles[i]))) a2=(c(Hoteles[i],a))
# a3 <- as.matrix(a2)
# 
# #Limpieza de los que no sean EN
# a <- character()
# for (i in 1:nrow(a3))
#   if (textcat(a3[i]) == "english") a4 = (c(a3[i],a))
# a5 <- as.data.frame(a4)

#https://rpubs.com/joseposada/topicModeling

# install.packages("corpus.JSS.papers",repos = "http://datacube.wu.ac.at/", type = "source")
# data("JSS_papers", package = "corpus.JSS.papers") #matriz del tipo lista 636x15
# JSS_papers <- JSS_papers[JSS_papers[,"date"] < "2010-08-05",] #361x15
# JSS_papers <- JSS_papers[sapply(JSS_papers[, "description"],Encoding) == "unknown",] #348x15

library("tm")
# library("XML")

# #closure function
# remove_HTML_markup =function(s) tryCatch({
#   doc = htmlTreeParse(paste("<!DOCTYPE html>", s),
#                       asText = TRUE, trim = FALSE)
#   xmlValue(xmlRoot(doc))}, 
#   error = function(s) s)

# Hotels_raw <- readLines("Hotels", encoding = "ANSI") #Encoding UTF-8 da error del tipo Utf8tolowr
# Hotels <- as.matrix(Hotels_raw)

Corpus <- Corpus(VectorSource(Hoteles)) #Vcorpus-Corpus-0na Metadata:  corpus specific: 0, document level (indexed): 0 Content:  documents: 26672

# Sys.setlocale("LC_COLLATE", "C")

DTM <- DocumentTermMatrix(Corpus,control = list(stemming = T, stopwords = T, minWordLength = 3,removeNumbers = T, removePunctuation = T))
# dim(DTM)

rownames(DTM) <- Hoteles[,1]

###############################################################################################################################################

library("slam")
# summary(col_sums(DTM)) #Convierte sparse matrix a simple triplet matrix

TFIDF <- tapply(DTM$v/row_sums(DTM)[DTM$i], DTM$j, mean) * log2(nDocs(DTM)/col_sums(DTM > 0)) 
#term frequency–inverse document frequency ¿weighting scheme?
# summary(TFIDF)

DTM <- DTM[,TFIDF >= 0.1] #Filtro con TFIDF mayor o igual a 0.1 ¿?
DTM <- DTM[row_sums(DTM) > 0,] #Filtro ¿?
# summary(col_sums(DTM))

# dim(DTM)

###############################################################################################################################################

library("topicmodels")
#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
#Number of topics
k <- 5

  #Run LDA using Gibbs sampling
  ldaOut <- LDA(DTM,k = k, method = "Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
  #write out results

  #docs to topics
  ldaOut.topics <- as.matrix(topics(ldaOut))
  write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))
  
  
  #top 6 terms in each topic
  ldaOut.terms <- as.matrix(terms(ldaOut,6))
  write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))
  
  
  #probabilities associated with each topic assignment
  topicProbabilities <- as.data.frame(ldaOut@gamma)
  write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))
  
  
  #Find relative importance of top 2 topics
  topic1ToTopic2 <- lapply(1:nrow(DTM),function(x)
    sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])
  
  
  #Find relative importance of second and third most important topics
  topic2ToTopic3 <- lapply(1:nrow(DTM),function(x)
    sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])
  
  
  #write to file
  write.csv(topic1ToTopic2,file=paste("LDAGibbs",k,"Topic1ToTopic2.csv"))
  write.csv(topic2ToTopic3,file=paste("LDAGibbs",k,"Topic2ToTopic3.csv"))


TopicModel <- list(VEM = LDA(DTM, k = k, control = list(seed = seed)),
              VEM_fixed = LDA(DTM, k = k,control = list(estimate.alpha = FALSE, seed = seed)),
              Gibbs = LDA(DTM, k = k, method = "Gibbs",control = list(seed = seed, burnin = 1000,thin = 100, iter = 1000)),CTM = CTM(DTM, k = k, control = list(seed = seed, var = list(tol = 10^-4), em = list(tol = 10^-3))))
#Warning messages:
#1: In class(value) <- "integer" :
#  NAs introduced by coercion to integer range
# sapply(TopicModel[1:2], slot, "alpha")

Topic <- topics(TopicModel[["VEM"]], 1)
Terms <- terms(TopicModel[["VEM"]], 5)

# (topics_v24 = topics(TopicModel[["VEM"]])[grep("/v24/", JSS_papers[, "identifier"])])
# most_frequent_v24 = which.max(tabulate(topics_v24))
# terms(TopicModel[["VEM"]], 10)[, most_frequent_v24]

Terms[,1:5]