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
