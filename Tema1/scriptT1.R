#K-means set de datos algas
load("~/archivos de programas/Christian/Documents/R/Data Mining/Data-Mining/Tema1/algas.Rdata")
names(algas)
summary(algas)


set.seed(12345)
modelo <- kmeans(subset(algas, select = -clase), centers = 6, nstart=10)
modelo

table(modelo$cluster, algas$clase)

#E-M

load("~/archivos de programas/Christian/Documents/R/Data Mining/Data-Mining/Tema1/vinos.RData")
names(vinos)
summary(vinos)
library(mclust) 
modelo <- Mclust(subset(vinos, select = -var), G=2) 
table(modelo$classification, vinos$var)
#Probabilidades
round(modelo$z,3)


summary(modelo, parameters = TRUE)
