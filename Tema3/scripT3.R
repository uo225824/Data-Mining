#Metodo CHAID
install.packages("CHAID", repos="http://R-Forge.R-project.org") 
compra <- read.csv2(file = "Tema3/compra.csv",header=TRUE,encoding="latin1")
names(compra)
summary(compra)
compra$edad <- ordered(compra$edad, levels=c('< 35','35 -55','> 55'))
compra$formacion <- ordered(compra$formacion, levels=c('elemen','media','univers')) 

library(CHAID) # carga CHAID 
chaid_control(minsplit = 50, minprob = 0.10) # configura algunas opciones de chaid
# minsplit: número de casos por debajo del cual no se hace la partición
# minprob: frecuencia mínima de los nodos terminales. 

ch <- chaid(comprador ~ ., data = compra)
print(ch) # imprime los resultados de chaid
plot(ch) # representa gráficamente el árbol de segmentación 


#Naive Bayes
library(car)
library(e1071)
load("~/archivos de programas/Christian/Documents/R/Data Mining/Data-Mining/Tema3/pacientes.RData")
names(pacientes)
summary(pacientes)
pacientes$hba1c7 <- recode(pacientes$hba1c, ' lo:7="hasta 7"; 7:hi="mayor que 7" ', as.factor=T)
table(pacientes$hba1c7) 

datos = pacientes[ , c('edad', 'sexo', 'alcohol', 'tabaco', 'dieta', 'peso', 'talla', 'tad', 'tas',
                       'colesterol', 'pericintura', 'peripelvis', 'trigl', 'creat', 'alcoholgrdia','imc', 'icc', 'hta2',
                       'hba1c7')]

entrenamiento <- subset(datos[1:600, ], select = -hba1c7)
prueba <- subset(datos[601:881, ], select = -hba1c7)

modelo <- naiveBayes(x = entrenamiento, y = datos[1:600,"hba1c7"])
modelo

resultados <- predict(object = modelo, newdata = prueba, type = "class")
t <- table(resultados, pacientes[601:881,"hba1c7"])
t ; 100 * sum(diag(t)) / sum(t)


datos[882,] <- c(62, "mujer", "si", "no", 0, 92, 1.62, 100, 160, 263, 104, 112, 116, 0.9, 8,
                 35.05563, 0.9285714, "si", NA)
predict(modelo, datos[882,], type = "raw")
