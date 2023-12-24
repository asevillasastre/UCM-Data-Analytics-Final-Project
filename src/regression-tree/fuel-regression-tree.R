library(dplyr)
library(caret)
library(rpart) 
library(rpart.plot)

setwd("F:/MDAT")

load("automocion_limpio2.RData")

load("PCA_CLUST_Opino.RData")

# Semilla: ----
set.seed(100)

# Eliminamos observaciones que contienen NAs en la variable a predecir
datos <- automocion[!is.na(automocion$Combustible_Coche),]

# Diferentes categorias de combustible:
unique(datos$Combustible_Coche)

# Redefinimos Combustible_Coche:
datos <- datos %>%
  filter(Combustible_Coche %in% c("Diesel", "Gasolina"))


# Nos quedamos con las variables predictoras que queremos
datos <- datos %>%
  select(ID, Combustible_Coche, Sex, Age, Income, Composicion_Hogar, Propiedad_Coche, Marca_Coche, Tipo_Coche,
         Canal_Compra, Tiempo_Coche, Estado_Coche, Metodo_Pago, Intencion_Compra, Habitat, TV_TOTAL,
         Radio_TOTAL, Province, Facebook_App, Twitter_App, Instagram_App, TikTok_App, Automocion_Apps, Maps_App) %>%
  left_join(PCA_CLUST_OPINO, by = "ID")

datos <- na.omit(datos)

conteo <- table(datos$Combustible_Coche) %>% as.data.frame()
colnames(conteo) <- c("Tipo de combustible", "Frecuencia")
ggplot(data=conteo, aes(x=`Tipo de combustible`, y=Frecuencia)) +
  geom_bar(stat="identity", fill="steelblue", width = 0.5)+
  geom_text(aes(label=Frecuencia), vjust=1.6, color="white", size=3.5)+
  theme_minimal()

# Separamos ids
ids <- datos[,1]
datos <- datos[,-1]

# Pasamos a factor las variables de tipo "character"
datos[,-which(colnames(datos) %in% c("Age", "TV_TOTAL", "Radio_TOTAL"))] <- lapply(datos[,-which(colnames(datos) %in% c("Age", "TV_TOTAL", "Radio_TOTAL"))], as.factor)


# Split Data: ----
datos_idx <- createDataPartition(datos$Combustible_Coche, p = 0.75, list = FALSE)
datos_train <- datos[datos_idx,]
datos_test <- datos[-datos_idx,]

# Separamos los datos test en datos para cv y datos para test:
datos_idx_2 <- createDataPartition(datos_test$Combustible_Coche, p = 0.5, list = FALSE)
datos_cv <- datos_test[-datos_idx_2,]
datos_test <- datos_test[datos_idx_2,]


# Veamos para que cp el arbol tiene mejor precision en los datos de train, el cp controla como 
# de grande va a ser el arbol. Guardamos las precisones de los difefrentes modelos en un vector
# y vemos cuando ocurre el máximo.
acc <- c()
for (cp_n in seq(0.001, 0.1, by = 0.001 )){
  fit.tree = rpart(Combustible_Coche ~ ., data=datos_train, method = "class", cp=cp_n)
  pred.tree = predict(fit.tree, datos_train, type = "class")
  x <- confusionMatrix(pred.tree, datos_train[,1])$overall["Accuracy"]
  acc <- c(acc, x)
}

# Hacemos la gráfica de los datos obtenidos:
df_2_plot <- data.frame(cp = seq(0.001, 0.1, by = 0.001 ),
                        acc = acc)
df_2_plot %>% 
  ggplot(aes(cp, acc)) +
  geom_line(colour="blue", size =0.5) +
  geom_point(colour = "blue") +
  labs(x = "Valor de cp en el modelo", y = "% precisión")


# En la gráfica vemos cómo cuanto mas cerca del 0, mas alta es la precisión pero también 
# mayor riesgo de sobreajustar el modelo y que no sea muy preciso en nuestro test. Tambien observamos en la grafica 
# que al llegar a cierto valor de cp se hace constante la precisión ya que en ese momento el árbol es
# lo más simple posible.

# Para que valor de cp, la precisión es máxima:
cp_max_train <- seq(0.001, 0.01, by = 0.001)[which(acc == max(acc))]

# Veamos el arbol:
fit.tree = rpart(Combustible_Coche ~ ., data=datos_train, method = "class", cp=cp_max_train)
fit.tree
pred.tree = predict(fit.tree, datos_train, type = "class")
confusionMatrix(pred.tree, datos_train[,1])

# Llegamos a obtener un 80% de precisión en el train, sin embargo, no será igual en el test.
# Usaremos el set de cross-validation para ajustar mejor los hiperparámetros.

# Visualizar
rpart.plot(fit.tree)
# Observamos que el árbol es demasiado grande y poco visual, por tanto se estará sobre ajustando mucho
# a los datos del set de entrenamiento. Usamos un cp más pequeño

# Veamos el arbol para un cp más pequeño:
fit.tree = rpart(Combustible_Coche ~ ., data=datos_train, method = "class", cp=0.008)
rpart.plot(fit.tree, tweak = 2)

# Se puede apreciar un árbol mucho más visual que es posible de interpretar.
pred.tree = predict(fit.tree, datos_train, type = "class")
confusionMatrix(pred.tree, datos_train[,1])
# Los resultados son peores en cuanto a precisión, pero si despues el modelo se ajusta bien
# al set de testeo, no habrá sido malo el sacrificio.

# Para ver como actuaría usamos el dataset de cross_validation para ver como actuaría cada 
# cp en el test final:
acc <- c()
for (cp_n in seq(0.001, 0.01, by = 0.001)){
  fit.tree = rpart(Combustible_Coche ~ ., data=datos_train, method = "class", cp=cp_n)
  pred.tree = predict(fit.tree, datos_cv, type = "class")
  x <- confusionMatrix(pred.tree, datos_cv[,1])$overall["Accuracy"]
  acc <- c(acc, x)
}

# Hacemos la gráfica de los datos obtenidos:
df_2_plot <- data.frame(cp = seq(0.001, 0.01, by = 0.001 ),
                        acc = acc)
df_2_plot %>% 
  ggplot(aes(cp, acc)) +
  geom_line(colour="blue", size =0.5) +
  geom_point(colour = "blue") +
  labs(x = "Valor de cp en el modelo", y = "% precisión")

cp_max_cv <- seq(0.001, 0.01, by = 0.001)[which(acc == max(acc))]

# Justo al contrario que en el set de entrenamiento a cuanto mayor es el cp mayor es
# la precisión ya que el modelo es más general y no sobre-ajusta. Las precisiones son muy similares.
# Todo indica a que cp = 0.007 podría ser un buen valor para el hiperparámetro de crecimiento ya que 
# generaliza bastante bien. Por tanto probamos, como último paso las predicciones sobre el set de testeo:

fit.tree = rpart(Combustible_Coche ~ ., data=datos_train, method = "class", cp = 0.01)
rpart.plot(fit.tree)# Se puede apreciar un árbol mucho más visual que es posible de interpretar.
pred.tree = predict(fit.tree, datos_test, type = "class")
confusionMatrix(pred.tree, datos_test[,1])

# Analicemos el arbol final obtenido: 
fit.tree$frame["var"]
# Se tiene en cuenta las variables: Marca_Coche,Tipo_Coche,Estado_Coche,Tiempo_Coche yProvince.
# Por tanto, el combustible parece que no va a tener en cuenta quién posee el coche si no cómo es el coche.
# Veamos la importancia de las variables:
fit.tree$variable.importance
# Aunque no esté en nuestro arbol Canal_Compra destaca por encima del resto de variables que no aparecen en el arbol,
# seguida de Age, Metodo_Pago e Income y ya con mucha menos importancia tenemos:
# TV_TOTAL, Habitat, Intencion_Compra, Maps_App, Radio_TOTAL y Sex . 
# Estas variables las usaremos como variables predictores en el modelo KNN.

# Este sería el modelo que usaríamos de árbol.
fit.tree = rpart(Combustible_Coche ~ Marca_Coche+Tipo_Coche+Province+Tiempo_Coche+Estado_Coche, 
                 data=datos_train, 
                 method = "class", 
                 cp=0.007)

rpart.plot(fit.tree, tweak = 2) # Se puede apreciar un árbol mucho más visual que es posible de interpretar.
pred.tree = predict(fit.tree, datos_test, type = "class")
confusionMatrix(pred.tree, datos_test[,1])


#               Reference
# Prediction Diesel Gasolina
# Diesel      189       88
# Gasolina     52       91
# 
# Accuracy : 0.6667
# Sensitivity : 0.7842          
# Specificity : 0.5084    
# 'Positive' Class : Diesel