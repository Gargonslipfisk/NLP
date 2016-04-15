if (!require("pacman")) install.packages("pacman")
pacman::p_load(tm, tm.plugin.webmining, XML, stringr, xts, quantmod)

query <- c("BME:SAN","ETR:VOW3","NASDAQ:GOOGL")
ticker <- c("SAN.MC", "VOW3.DE", "GOOGL")

### SCRAPING
# Función para parsear el DOM del código HTML 
parser <- function(cr){
  tree <- parse(cr, type = "XML", asText = FALSE)
  xpathSApply(tree, path = "//item")
}

# Genera la url, descarga y genera corpus 
url <- NULL
for (i in 1:length(query)){
  cat(i, '\n')
  url <- c(paste("http://www.google.com/finance/company_news?hl=en&q=",URLencode(query[i]),"&ie=utf-8&start=0&num=2000&output=rss",sep=""), url)
  name <- paste("corpus", sub(":", "", query[i]), sep = "") 
  assign(name, value = WebCorpus(WebSource(url, class = "WebXMLSource", parser = parser, reader = readGoogle, 
                                postFUN = getLinkContent, retrieveFeedURL = F)))
}

### SENTIMENT
# Cargo diccionario de sentiment
afinn <- read.delim("AFINN/AFINN-111.txt",header = F, quote = "", stringsAsFactors = F)
names(afinn) <- c("word", "score")

head(afinn, 50)

# Extraigo titulares y fechas. Normalizo titulares para pasarle sentiment
for (j in 1:length(ls(pa="corpus"))){
  cat(j, '\n')
  name <- sub("corpus", "df", ls(pa="corpus")[j])
  assign(name, value = cbind.data.frame(do.call(c, lapply(get(ls(pa="corpus")[j]), meta, "datetimestamp")), do.call(c, lapply(get(ls(pa="corpus")[j]), meta, "heading"))))
  for (i in 1) {
    x <- get(name)
    names(x) <- c("date", "heading")
    assign(name, x)
  }
  assign(name, value = get(name)[!duplicated(get(name)$heading),])
  for (i in 1) {
    x <- get(name)
    x$heading.clean <- tolower(gsub("\\(.+\\)|[[:punct:]]", "", x$heading))
    x$heading.clean <- tolower(gsub("[[:punct:]]", "", x$heading.clean))
    # Genera TDM para contabilizar sentiment
    scoretdm <- t(apply(t(x$heading.clean), 2,function(x) str_count(x, afinn$word)))
    # Sentiment asociado a cada titular
    x$afinn.rating <- as.vector(scoretdm %*% afinn$score)
    assign(name, x)
    name <- sub("df", "xts", ls(pa="df")[j])
    assign(name, value = cbind.data.frame(x$date, x$afinn.rating))
  }
}

dim(scoretdm)

### SERIE TEMPORAL DEL SENTIMENT
# Sentimiento
for (k in 1:length(ls(pa="xts"))){
  cat(k, '\n')
  name <- ls(pa="xts")[k]
  for (i in 1) {
    x <- get(name)
    x <- xts(x[, -1], order.by = x[, 1])
    assign(name, x)
    vol <- xts(rep(1,NROW(x)), order.by = index(x))
    ep <- endpoints(x, on = "days", k = 1)
    ldata <- lapply(1:NCOL(x), function(f) period.apply(x[,f], ep, mean))
    ldata_all <- do.call("cbind", ldata)
    all_agg <- xts(ldata_all, order.by = index(x)[ep])
    vol_agg <- period.apply(vol, ep, sum)
    scorevolall_agg <- cbind(all_agg, vol_agg)
    index(scorevolall_agg) <- as.Date(index(scorevolall_agg))
    colnames(scorevolall_agg) <- c('X.Close', 'X.Volume')
    name2 <- paste("plot", sub("xts", "", name), sep="")
    assign(name2, scorevolall_agg)
  }
}

###  GRÁFICOS
for (i in 1:length(ticker)){
  cat(i, '\n')
  getSymbols(ticker[i])
  candleChart(get(ticker[i]),subset='2014-12::2016')
  candleChart(get(ls(pa="plot")[i]),subset='2014-12::2016')
}

### FUTURO
library(openNLP)

s <- as.String('Santander bank plans to slash up to 1,200 jobs across Spain')

# Anotadores
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
parse_annotator <- Parse_Annotator()

a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
p <- parse_annotator(s, a2)
ptext <- sapply(p$features, `[[`, "parse")
ptext
Tree_parse(ptext)
