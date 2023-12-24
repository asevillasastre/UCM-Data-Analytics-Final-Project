library(dplyr)
library(caret)
library(rpart) 
library(rpart.plot)

setwd("F:/MDAT")

load("automocion_limpio2.RData")

load("PCA_CLUST_Opino.RData")

# PREPARACIÓN DE LOS DATOS Y LOS SETS DE ENTRENAMIENTO, VALIDACIÓN Y TESTEO: ----
set.seed(112)

# Eliminamos observaciones que contienen NAs en la variable a predecir
datos <- automocion[!is.na(automocion$Intencion_Compra),]

# Diferentes categorias de combustible:
unique(datos$Intencion_Compra)

conteo <- table(datos$Intencion_Compra) %>% as.data.frame()
colnames(conteo) <- c("Intención de compra", "Frecuencia")
ggplot(data=conteo, aes(x=`Intención de compra`, y=Frecuencia)) +
  geom_bar(stat="identity", fill="steelblue", width = 0.5)+
  geom_text(aes(label=Frecuencia), vjust=1.6, color="white", size=3.5)+
  theme_minimal()

# Nos quedamos con las variables predictoras que queremos
datos <- datos %>%
  select(ID, Intencion_Compra, Sex, Age, Income, Composicion_Hogar, Propiedad_Coche, Marca_Coche, Tipo_Coche,
         Canal_Compra, Tiempo_Coche, Estado_Coche, Combustible_Coche, Metodo_Pago, Habitat, TV_TOTAL,
         Radio_TOTAL, Province, Facebook_App, Twitter_App, Instagram_App, TikTok_App, Automocion_Apps, Maps_App) %>%
  left_join(PCA_CLUST_OPINO, by = "ID")


datos <- na.omit(datos)

ids <- datos[,1]
datos <- datos[,-1]

# Pasamos a factor las variables de tipo "character"
datos[,-which(colnames(datos) %in% c("Age", "TV_TOTAL", "Radio_TOTAL"))] <- lapply(datos[,-which(colnames(datos) %in% c("Age", "TV_TOTAL", "Radio_TOTAL"))], as.factor)


# Split Data:
datos_idx <- createDataPartition(datos$Intencion_Compra, p = 0.75, list = FALSE)
datos_train <- datos[datos_idx,]
datos_test <- datos[-datos_idx,]

# Separamos los datos test en datos para cv y datos para test:
datos_idx_2 <- createDataPartition(datos_test$Intencion_Compra, p = 0.5, list = FALSE)
datos_cv <- datos_test[-datos_idx_2,]
datos_test <- datos_test[datos_idx_2,]


# AJUSTES DEL ÁRBOL DE CLASIFICACIÓN: ----

# Veamos para que cp el arbol tiene mejor precision en los datos de train, el cp controla como 
# de grande va a ser el arbol. Guardamos las precisones de los difefrentes modelos en un vector
# y vemos cuando ocurre el máximo.
acc <- c()
for (cp_n in seq(0.001, 0.1, by = 0.001 )){
  fit.tree = rpart(Intencion_Compra ~ ., data=datos_train, method = "class", cp=cp_n)
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

# Para que valor de cp, la precisión es máxima:
cp_max_train <- seq(0.001, 0.01, by = 0.001)[which(acc == max(acc))]

# Veamos el arbol:
fit.tree = rpart(Intencion_Compra ~ ., data=datos_train, method = "class", cp=cp_max_train)
fit.tree
pred.tree = predict(fit.tree, datos_train, type = "class")
confusionMatrix(pred.tree, datos_train[,1])

# Visualizar
rpart.plot(fit.tree)
# Observamos que el árbol es demasiado grande y poco visual, obteniendo una precisión del 81%,
# por tanto se estará sobre ajustando mucho
# a los datos del set de entrenamiento. Usamos un cp más pequeño


# Veamos el arbol para un cp más pequeño:
fit.tree = rpart(Intencion_Compra ~ ., data=datos_train, method = "class", cp=0.008)
rpart.plot(fit.tree)
# Se puede apreciar un árbol mucho más visual que es posible de interpretar.
pred.tree = predict(fit.tree, datos_train, type = "class")
confusionMatrix(pred.tree, datos_train[,1])
# Los resultados son peores en cuanto a precisión, pero si despues el modelo se ajusta bien
# al set de testeo, no habrá sido malo el sacrificio.


# Para ver como actuaría usamos el dataset de cross_validation para ver como actuaría cada 
# cp en el test final:
acc <- c()
for (cp_n in seq(0.001, 0.01, by = 0.001)){
  fit.tree = rpart(Intencion_Compra ~ ., data=datos_train, method = "class", cp=cp_n)
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

# Justo al contrario que en el set de entrenamiento a cuanto mayor es el cp mayor es
# la precisión ya que el modelo es más general y no sobre ajusta.  Las precisiones son muy similares.
# Todo indica a que cp = 0.007 podría ser un buen valor para el hiperparámetro de crecimiento ya que 
# generaliza bastante bien. Por tanto probamos, como último paso las predicciones sobre el set de testeo:

fit.tree = rpart(Intencion_Compra ~ ., data=datos_train, method = "class", cp=0.007)
rpart.plot(fit.tree)# Se puede apreciar un árbol mucho más visual que es posible de interpretar.
pred.tree = predict(fit.tree, datos_test, type = "class")
confusionMatrix(pred.tree, datos_test[,1])

# Obtenemos un 60% de precisión.

# Analicemos el arbol final obtenido: 
unique(fit.tree$frame["var"])
# Tiene en cuenta 5 variables: PCA_CLUST_Opino, Tiempo_Coche, Age, Province, Marca_Coche
# Por tanto la intención de compra se relaciona con el tipo de conductor que es la persona 
# (Cluster de las preguntas de identificación) así como la edad y su CCAA. Por otro lado, como se preveía,
# se tienen en cuenta el tiempo de coche y la marca del coche.

# Veamos la importancia de las variables:
fit.tree$variable.importance
# En este caso, el resto de variables que hemos añadido al arbol no aportan prácticamente información 
# Las que más serían Income y Tipo_Coche.


# RESULTADOS: ----

# Este sería el modelo que usaríamos de árbol.
fit.tree = rpart(Intencion_Compra ~ Tiempo_Coche + PCA_CLUST_Opino + Marca_Coche + Province + Age, 
                 data=datos_train, 
                 method = "class", 
                 cp=0.007)
rpart.plot(fit.tree, tweak = 2)# Se puede apreciar un árbol mucho más visual que es posible de interpretar.
pred.tree = predict(fit.tree, datos_test, type = "class")
confusionMatrix(pred.tree, datos_test[,1])

#            Reference
# Prediction  No  Sí
# No          135  83
# Sí           93 133
# 
# Accuracy : 0.6036          
# Sensitivity : 0.5921          
# Specificity : 0.6157          
#  
