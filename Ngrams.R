library(tm)
library(RWeka)
setwd("~/Erreria/NGrams")

###FUNCIÓN TDM

tdm.generate <- function(string, ng, ng2){
  
  # tutorial on rweka - http://tm.r-forge.r-project.org/faq.html
  
  corpus <- Corpus(VectorSource(string)) # create corpus for TM processing
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeNumbers) 
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  # corpus <- tm_map(corpus, removeWords, stopwords("english")) 
  options(mc.cores=1) # http://stackoverflow.com/questions/17703553/bigrams-instead-of-single-words-in-termdocument-matrix-using-r-and-rweka/20251039#20251039
  BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = ng, max = ng2)) # create n-grams
  tdm <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer)) # create tdm from n-grams
  tdm
}

###OBTENCIÓN DE DATOS

library(stringi)
library(XML)

for (j in 1:10){
  cat(j, '\n')
  wiki <- "https://vi.wikipedia.org/wiki/"
  titles <- c(rep("Special:Random",250))
  articles <- character(length(titles))
    for (i in 1:length(titles)) {
      cat(i, '\n')
      articles[i] <- xpathSApply(htmlTreeParse(stri_flatten(readLines(stri_paste(wiki, titles[i]), encoding = "UTF-8"), col = " "), useInternalNodes=T), "//div[@id = 'mw-content-text']", xmlValue, trim = TRUE)
    }
  nam <- paste("tdm", j, sep = "")
  assign(nam, value = tdm.generate(articles, 2, 9))
}

tdmc = do.call("c", lapply(objects(pa="tdm[[:digit:]]"),get))

###EXPLORACIÓN

tdm.matrix <- as.matrix(tdmc)
topwords <- rowSums(tdm.matrix)
topwords <- as.numeric(topwords)
hist(topwords, breaks = 10)

findFreqTerms(tdmc, lowfreq = 50)

findFreqTerms(tdmc, lowfreq = 100)

###https://rstudio-pubs-static.s3.amazonaws.com/69071_b683b304a55c4c4c8730183022e2eee9.html
# Error in vector(typeof(x$v), nr * nc) : vector size cannot be NA
# In addition: Warning message:
# In nr * nc : NAs produced by integer overflow

dim(tdmc)
tdmc_rs <- removeSparseTerms(tdmc, 0.98)
dim(tdmc_rs)

tdm.matrix <- as.matrix(tdmc_rs)
topwords <- rowSums(tdm.matrix)
z <- sort(topwords, decreasing = T)
zz <- as.data.frame(z)
write.csv(zz,"tdmc5.csv")

###COMBINANDO

# tdm1 = read.csv("tdmc.csv", h = T, encoding = "UTF-8", row.names = 1)
# tdm2 = read.csv("tdmc2.csv", h = T, encoding = "UTF-8", row.names = 1)
# tdmc <- merge(tdm1, tdm2, by="row.names", all=T)  # merge by row names (by=0 or by="row.names")

tdm1 = read.csv("tdmc.csv", h = T, encoding = "UTF-8")
tdm2 = read.csv("tdmc2.csv", h = T, encoding = "UTF-8")
tdm3 = read.csv("tdmc3.csv", h = T, encoding = "UTF-8")
tdm4 = read.csv("tdmc4.csv", h = T, encoding = "UTF-8")
tdm5 = read.csv("tdmc5.csv", h = T, encoding = "UTF-8")
tdmc <- merge(tdm1, tdm2, by="X", all=T)  # merge by row names (by=0 or by="row.names")
  names(tdmc) <- c("X", "tdm1", "tdm2")
tdmc2 <- merge(tdmc, tdm3, by="X", all=T)
  names(tdmc2)[4] <- "tdm3"
tdmc3 <- merge(tdmc2, tdm4, by="X", all=T)
  names(tdmc3)[5] <- "tdm4"
tdmc4 <- merge(tdmc3, tdm5, by="X", all=T)
  names(tdmc4)[6] <- "tdm5"
tdmc4[is.na(tdmc4)] <- 0                 # replace NA values

tab <- data.frame(tdmc4$tdm1,tdmc4$tdm2,tdmc4$tdm3,tdmc4$tdm4,tdmc4$tdm5)
tab <- cbind(tdmc4, Total = rowSums(tab))

tdmcs <- tab[order(-tab$Total),] 

# sum(tdmcs[4])
sum(tdmcs$Total)

tdmcsf <- cbind(tdmcs, Freq = tdmcs$Total/sum(tdmcs$Total))
write.csv(tdmcsf,"tdmcsft.csv")

###

# # DT[ ,sum := rowSums(.SD), .SDcols = grep("i", names(DT))]
# tdmc[, sum := Reduce('+', .SD), .SDcols=listCol][]
# tdmc[, sum := Reduce('+', .SD), .SDcols=grep("z", names(DT))][]
# tdmc[, sum := rowSums(.SD), .SDcols=grep("z", names(DT))]
# 
# 
# tdmc[4] <- rowSums(tdmc[2],tdmc[3])
# tdmc[4] <- rowSums(tab)
# 

###
###PROBANDO

library(XML)
library(stringi)
wiki <- "https://es.wikipedia.org/wiki/"
titles <- c(rep("Special:Random",25))
articles <- character(length(titles))
for (i in 1:length(titles)) {
  cat(i, '\n')
  articles[i] <- xpathSApply(htmlTreeParse(stri_flatten(readLines(stri_paste(wiki, titles[i]), encoding = "UTF-8"), col = " "), useInternalNodes=T), "//div[@id = 'mw-content-text']", xmlValue, trim = TRUE)
}

library(NLP)
library(tm)
library(SnowballC)

ProjectCorpus <- Corpus(VectorSource(articles))
ProjectCorpus1 <- tm_map(ProjectCorpus, tolower)
ProjectCorpus2 <- tm_map(ProjectCorpus1, PlainTextDocument)
ProjectCorpus3 <- tm_map(ProjectCorpus2, removePunctuation)
ProjectCorpus4 <- tm_map(ProjectCorpus3, removeWords, stopwords("english"))
ProjectCorpus5 <- tm_map(ProjectCorpus4, stemDocument)
ProjectCorpus6 <- tm_map(ProjectCorpus5, removeNumbers)
ProjectCorpus7 <- tm_map(ProjectCorpus6, stripWhitespace)

ProjectDTM<- DocumentTermMatrix(ProjectCorpus7)
ProjectTDM <- TermDocumentMatrix(ProjectCorpus7)
ProjectSparse <- removeSparseTerms(ProjectDTM, 0.98)
ProjectFrequency <- colSums(as.matrix(ProjectSparse))
ProjectOrder <- order(ProjectFrequency)

ProjectFrequency[tail(ProjectOrder)]
findAssocs(ProjectDTM, "monitor", corlimit=0.2)

library(Rgraphviz)
library(graph)
library(grid)
plot(ProjectDTM,
     terms=findFreqTerms(ProjectDTM, lowfreq=100),
     corThreshold=0.20)

###
library(graph)
library(grid)
plot(tdm1,
     terms=findFreqTerms(tdm1, lowfreq=120),
     corThreshold=0.11)


# docs <- Corpus(VectorSource(articles))
# docs2 <- tm_map(docs, function(x) stri_replace_all_regex(x, "<.+?>", " "))
# docs3 <- tm_map(docs2, function(x) stri_replace_all_fixed(x, "\t", " "))
# docs4 <- tm_map(docs3, PlainTextDocument)
# docs5 <- tm_map(docs4, stripWhitespace)
# docs6 <- tm_map(docs5, removePunctuation)
# docs7 <- tm_map(docs6, tolower)

# gramTokenizer <-
#   function(x)
#     unlist(lapply(ngrams(words(x), 9), paste, collapse = " "), use.names = FALSE)
# 
# tdm <- TermDocumentMatrix(docs6, control = list(tokenize = gramTokenizer))
# xxx = inspect(removeSparseTerms(tdm[, 1:1], 0.9))
