GetGoogleResults <- function(keyword, service, key) {       
  library(RCurl)
  library(rjson)
  base_url <- "https://ajax.googleapis.com/ajax/services/search/"
  keyword <- gsub(" ", "+", keyword)
  keyword <- paste("%22",keyword,"%22", sep="")
  query <- paste(base_url, service, "?v=1.0&q=", keyword, sep="")
  if(!is.null(key))
    query <- paste(query, "&key=", key, sep="")
  
  query <- paste(query, "&start=", 0, sep="")
  results <- fromJSON(getURL(query))
  return(results)
}

b <- read.csv("TH.Final.txt", sep="\t")
formas  <- b$form

freq = NULL
for (i in 1:length(formas)) {
  keyword = URLencode(as.character(formas[i]))
  google = GetGoogleResults(keyword, "web", "AIzaSyAPPJKyAuO2af9j6VTyDZXeJEQ3nVlEvlw")
  print(google$responseData$cursor$estimatedResultCount)
  freq = c(freq, google$responseData$cursor$estimatedResultCount)
  Sys.sleep(1)
}