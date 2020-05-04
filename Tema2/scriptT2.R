#Adaboost
load("~/archivos de programas/Christian/Documents/R/Data Mining/Data-Mining/Tema2/deudas.RData")
names(deudas) # listado de variables
summary(deudas) # resumen descriptivo
library(adabag)
set.seed(123) # inicializa el arranque del generador aleatorio
muestra <- sample(1:nrow(deudas), 80) # muestra de 80 números entre 1 y 100
entrenamiento <- deudas[muestra, ] # muestra de entrenamiento
prueba <- deudas[-muestra, ]

modelo <- boosting(Impago ~ ., data = entrenamiento)
modelo$trees
modelo$weights
modelo$prob
modelo$importance

resultados.entrenamiento <- predict(modelo, newdata = entrenamiento, type = "class")
resultados.entrenamiento$confusion


resultados.prueba <- predict(modelo, newdata = prueba, type = "class")
t <- resultados.prueba$confusion
t ; 100 * sum(diag(t)) / sum(t) # calcula el porcentaje global de acierto



modelo <- boosting(Impago ~ ., data = deudas)
resultados <- predict(modelo, newdata = deudas, type = "class")
t <- resultados$confusion
t ; 100 * sum(diag(t)) / sum(t)


#Random Forest

load("~/archivos de programas/Christian/Documents/R/Data Mining/Data-Mining/Tema2/vinos.RData")
library(randomForest)
set.seed(12345)
muestra <- sample(1:nrow(vinos), 40) # 40 números de fila o caso elegidos al azar
entrenamiento <- vinos[muestra, ]
prueba <- vinos[-muestra, ] 
set.seed(12345)
modelo <- randomForest(var~ gal+tar+mal+shi+cit+suc, data=entrenamiento)
modelo
modelo$predicted
round(modelo$votes, 3)
modelo$importance
varImpPlot(modelo)


predicciones <- predict(modelo, prueba)
t <- with(prueba, table(predicciones, var)) # Matriz de confusión
t ; 100 * sum(diag(t)) / sum(t)
