#KNN
load("~/archivos de programas/Christian/Documents/R/Data Mining/Data-Mining/Tema5/algas.Rdata")

set.seed(123) # fija el arranque aleatorio, lo que permite reproducir la misma muestra
muestra <- sample(1:nrow(algas), 20) # muestra de 20 números de caso entre 1 y 31
entrenamiento <- subset(algas[muestra, ], select = -clase) # casos de entrenamiento
prueba <- subset(algas[-muestra, ], select = -clase) # casos de validación 
library(class)
resultados <- knn(entrenamiento, prueba, cl = algas[muestra,"clase"])
table(resultados, algas[-muestra,"clase"])


predictores <- subset(algas, select = -clase) # toda la muestra, excluyendo la variable clase
resultados <- knn(predictores,predictores, cl = algas$clase)
table(resultados, algas$clase)

#SVM

load("~/archivos de programas/Christian/Documents/R/Data Mining/Data-Mining/Tema5/empresas.RData")

set.seed(123)
muestra <- sample(1:nrow(empresas), 1000)
entrenamiento <- empresas[muestra, ]
prueba <- empresas[-muestra, ] 

library(e1071)
modelo <- svm(CULTURAL ~ ., data = entrenamiento)
summary(modelo)

resultados.entrenamiento <- predict(modelo, newdata = entrenamiento, type = "class")
table(resultados.entrenamiento, entrenamiento$CULTURAL)

resultados.prueba <- predict(modelo, newdata = prueba, type = "class")
t <- table(resultados.prueba, prueba$ CULTURAL)
t ; 100 * sum(diag(t)) / sum(t)
