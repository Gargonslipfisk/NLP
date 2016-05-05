#####PRIMERA PARTE
###Carga de los paquetes a utilizar
library(tm)
library(stringi)

####Extracción de los textos
#Declara los objetos a usar por la función
epub <- "http://www.gutenberg.org/cache/epub/"
titulos <- c("2701/pg2701", "84/pg84", "345/pg345", "11/pg11",
            "6130/pg6130", "4363/pg4363", "1727/pg1727", "55/pg55",
            "996/pg996", "1998/pg1998", "20/pg20", "76/pg76",
            "1661/pg1661", "5200/pg5200")
libros <- character(length(titulos))

#Función para la extracción de textos mediante la función stri_flatten del paquete stringi:
#une los elementos de un vector de caracteres en una cadena
for (i in 1:length(titulos)) {
  libros[i] = stri_flatten(readLines(stri_paste(epub, titulos[i])), col = " ")
}

###Preparación de los textos
#Creación de un corpus mediante la función VectorSource del paquete tm:
#interpreta cada elemento de la cadena libros como un documento. Y la función Corpus del paquete tm:
#representa un corpus a partir de una colección de documentos
data <- Corpus(VectorSource(libros))

#Procesamos los textos mediante la función tm_map del paquete tm:
#convierte en texto plano, elimina los signos de puntuación mediante expresiones regulares 
#y palabras redundantes para nuestro análisiscomo conjunciones, pronombres etc.
data2 <- tm_map(data, function(x) stri_replace_all_regex(x, "<.+?>", " ")) #Elimina signos de puntuación
data3 <- tm_map(data2, function(x) stri_replace_all_fixed(x, "\t", " ")) #Elimina tabulación
data4 <- tm_map(data3, PlainTextDocument) #Convierte a texto plano
data5 <- tm_map(data4, stripWhitespace) #Elimina espacios en blanco
data6 <- tm_map(data5, removeWords, stopwords("english")) #Elimina léxico funcional
data7 <- tm_map(data6, removePunctuation) #Elimina signos de puntuación
data8 <- tm_map(data7, tolower) #Convierte texto a minúscula

#Creamos una matriz de términos mediante la función TermDocumentMatrix del paquete tm:
#convierte un corpus en una matriz de términos descritos por su frecuencia asociada a cada documento
dataTDM <- TermDocumentMatrix(data8)

#Crea una matriz de similitud en base a la matriz de términos mediante la función dissimilarity.
#Acepta los métodos de distancia entre términos: 
#affinity, cosine, dice, euclidean, jaccard, matching, pearson, toivonen y gupta
datacos <- dissimilarity(dataTDM, method = "cosine")

###Visualización
datacos2 <- as.matrix(datacos) #Convierte el objeto dist en un objeto matriz
rownames(datacos2) = titulos #Nombra las filas
colnames(datacos2) = titulos #Nombra las columnas
#Creación del árbol jerárquico de conglomerados mediante la función hclust.
#Acepta los métodos de distancia entre grupos: 
#centroid, ward, median, average, complete, single y mcquitty
hward <- hclust(datacos, method = "ward")
hcent <- hclust(datacos, method = "centroid")

#Grafica los resultados
plot(hward, labels = titulos, sub = "Distancia de ward")
plot(hcent, labels = titulos, sub = "Distancia entre centroides")

###Exportación en formato csv (cambiar extensión)
#Mediante la función inspect del paquete tm: muestra información detallada de un corpus o una matriz de terminos
m <- inspect(dataTDM)
DF <- as.data.frame(m, stringsAsFactors = FALSE) #Convierte el objeto matriz a tabla
write.table(DF, file = "ordenado.txt", sep = ",", 
            eol = "\n", dec = ".", row.names = TRUE,
            col.names = TRUE)

###Código adicional
#Usando la distancia euclidiana
dataeuc <- dissimilarity(dataTDM, method = "euclidean")
hwardeuc <- hclust(dataeuc, method = "ward")
hcenteuc <- hclust(dataeuc, method = "centroid")
plot(hwardeuc, labels = titulos, sub = "Distancia de ward")
plot(hcenteuc, labels = titulos, sub = "Distancia entre centroides")

#Usando la distancia de pearson
datapea <- dissimilarity(dataTDM, method = "pearson")
hwardpea <- hclust(datapea, method = "ward")
hcentpea <- hclust(datapea, method = "centroid")
plot(hwardpea, labels = titulos, sub = "Distancia de ward")
plot(hcentpea, labels = titulos, sub = "Distancia entre centroides")

#Mapa de calor mediante
#Correlación acepta métodos spearman, kendall y pearson 
corr <- round(cor(m, method="pearson")*100)
library(gplots)
generate_heat_map = function(correlationMatrix, title)
{
  heatmap.2(x = correlationMatrix,    
            cellnote = correlationMatrix,   
            main = title,    	
            symm = T,			
            dendrogram="none",		
            Rowv = F,			
            trace="none",			
            density.info="none",		
            notecol="black")		  
}
#Visualización
generate_heat_map(corr, "Correlaciones entre documentos")




#####SEGUNDA PARTE
libros <- read.csv("corregido.csv") #Carga del csv corregido para aplicar función distancia
libros <- as.data.frame(t(libros)) #Transposición de la matriz
colnames(libros) = libros[1, ] #Se añaden los nombres de las observaciones
libros <- libros[-1, ] #Se elimina la primera fila
libros$myfactor = factor(row.names(libros)) #Se añaden los factores (identificadores)

#Función distancia del paquete base admite los métodos:
#euclidean, maximum, manhattan, canberra, binary y minkowski
distlibros <- dist(as.matrix(libros)) #por defecto method = "euclidean"

#hclibros = hclust(libros) #Error in if (is.na(n) || n > 65536L) stop("size cannot be NA nor exceed 65536") 
#plot(hclibros)

#Dado que la función por defecto hclust no acepta 0, se crea la siguiente función para el análisis jerarquico de 
#clústers. Acepta los métodos: centroid, ward, median, average, complete, single y mcquitty
hclustfunc <- function(x) hclust(x, method="complete")
#distfunc <- function(x) as.dist((1-cor(t(x)))/2)
#d <- distfunc(as.matrix(distlibros))
fit <- hclustfunc(d)
plot(fit)

#Para poder usar el método cosenos se necesita:
library(proxy)
distlibros <- dist(as.matrix(libros), method = "cosine")
fit <- hclustfunc(distlibros)
plot(fit)

#Estadísticos
  summary(distlibros)
  tabla <- summary(distlibros)
  tabla <- as.matrix(tabla)

#Matriz de distancias
  tabla <- as.matrix(distlibros)


#####Otras pruebas (poco a poco)
#Algoritmo k-means
resultados <- kmeans(libros[,3:40000], 5)
resultados$cluster
resultados$size
corregido <- read.csv("corregido.csv")
plot(corregido, col=resultados$cluster)

#Advertencia: precisa de varias horas
#Encontrar el mejor modelo
library(mclust)
fit <- Mclust(libros)
plot(fit)
summary(fit)

#Advertencia: precisa de varias horas
#Gráfico cluster
library(cluster) 
clusplot(libros, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

#Cotejando soluciones
#Error: subscript out of bounds // returning Inf
#Documentación del paquete fpc: Because cluster.stats processes a full dissimilarity matrix, 
#it isn’t suitable for large data sets. You may consider distcritmulti in that case.
library(fpc)
d <- dist(as.matrix(libros), method = "cosine")
cluster.stats(d, fit1$cluster, fit2$cluster)
#Probando con distcritmulti
set.seed(20000)
options(digits=3)
face <- rFace(50,dMoNo=2,dNoEy=0,p=2)
clustering <- as.integer(attr(face,"grouping"))
distcritmulti(face,clustering,ns=3,seed=100000,criterion="pearsongamma")