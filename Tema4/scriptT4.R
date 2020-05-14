#CART

load("~/archivos de programas/Christian/Documents/R/Data Mining/Data-Mining/Tema4/deudas.RData")
set.seed(12345)
muestra <- sample(1:nrow(deudas), 80)
entrenamiento <- deudas[muestra, ]
prueba <- deudas[-muestra, ] 
library(rpart) # carga el paquete rpart de R, que debe ser instalado previamente
modelo <- rpart(Impago ~ . , data = entrenamiento)
resultados <- predict(object = modelo, newdata = prueba, type = "class")
t <- table(resultados, prueba$Impago)
t ; 100 * sum(diag(t)) / sum(t)

modelo <- rpart(Impago ~ . , data = deudas) # con toda la muestra
resultados <- predict(object = modelo, newdata = deudas, type = "class")
t <- table(resultados, deudas$Impago)
t ; 100 * sum(diag(t)) / sum(t)

modelo

library(rpart.plot)
rpart.plot(modelo)


deudas[101,1:4] = c("mas de 40", "elemental", "más de 10 años", "más de 10 años")
deudas[101,5:9] = c(154.0 , 9.5, 12.56, 10.56, NA)
predict(modelo, deudas[101,], type = "class")
predict(modelo, deudas[101,], type = "prob")


#C5.0

load("~/archivos de programas/Christian/Documents/R/Data Mining/Data-Mining/Tema4/vinos.RData")
set.seed(123)
muestra <- sample(1:nrow(vinos), 40) # 40 números de caso elegidos al azar
entrenamiento <- vinos[muestra, ]
prueba <- vinos[-muestra, ] 

library(C50)
modelo <- C5.0(var ~ gal+tar+mal+shi+cit+suc, data = entrenamiento)
summary(modelo) # Información sobre el modelo

plot(modelo) # Gráfico
resultados.entrenamiento <- predict(modelo, newdata = entrenamiento, type = "class")
table(resultados.entrenamiento, entrenamiento$var)

resultados.prueba <- predict(modelo, newdata = prueba, type = "class")
table(resultados.prueba, prueba$var)


modelo <- C5.0(var ~ gal+tar+mal+shi+cit+suc, data = vinos)
resultados <- predict(modelo, vinos, type = "class")
table(resultados, vinos$var)
