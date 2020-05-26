#A priori
load("~/archivos de programas/Christian/Documents/R/Data Mining/Data-Mining/Tema6/compra.RData")
head(compra) # escribe los seis primeros casos
library(arules) # carga el paquete arules, que debe estar instalado previamente
transacciones <- as(compra, "transactions") # convierte los datos en transacciones

reglas <- apriori(transacciones, parameter = list(support = 0.1, confidence = 0.90))
reglas.ordenadas <- sort(reglas, by = "confidence")
as(reglas.ordenadas, "data.frame") # escribe las reglas, en formato conjunto de datos

#PageRank
Dataset <- read.csv2(file = "Tema6/conoce.csv", header=TRUE,encoding="latin1")
names(Dataset) # escribe los nombres de las variables del conjunto Dataset
head(Dataset) # escribe los primeros 6 casos (tail escribiría los 6 últimos)
table(Dataset$ORIGEN) # tabla de frecuencias de la variable ORIGEN
table(Dataset$DESTINO) # tabla de frecuencias de la variable DESTINO

library(igraph) # cargamos el paquete igraph (debe estar instalado previamente)
grafo <- graph.data.frame(Dataset) # creamos el objeto ‘grafo’, transformación de Dataset
set.seed(123)
plot(grafo) # representamos el grafo

I <- page.rank(grafo) ; I # calcula el índice PageRank y lo escribe
I <- page.rank(grafo); I$vector


set.seed(123)
plot(grafo, vertex.size=I$vector*200)
