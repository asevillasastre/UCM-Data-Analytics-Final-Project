library(dplyr)
library(caret)
library(rpart) 
library(rpart.plot)

setwd("F:/MDAT")

load("automocion_limpio2.RData")

load("PCA_CLUST_Opino.RData")

# Semilla: ----
set.seed(112) # o 200

# Eliminamos observaciones que contienen NAs en la variable a predecir
datos <- automocion[!is.na(automocion$Metodo_Pago),]

# Diferentes categorias de combustible:
unique(datos$Metodo_Pago)

# Redefinimos Metodo_Pago:
datos <- datos %>%
  filter(Metodo_Pago %in% c("Financiacion","Al_Contado"))

# Añadimos todas las variables que puedan predecir el método de pago
# para ver que arbol crea con todas ellas.
datos <- datos %>%
  select(ID, Metodo_Pago, Sex, Age, Income, Composicion_Hogar, Propiedad_Coche, Marca_Coche, Tipo_Coche,
         Canal_Compra, Tiempo_Coche, Estado_Coche, Combustible_Coche, Intencion_Compra, Habitat, TV_TOTAL,
         Radio_TOTAL, Province, Facebook_App, Twitter_App, Instagram_App, TikTok_App, Automocion_Apps, Maps_App) %>%
  left_join(PCA_CLUST_OPINO, by = "ID")

datos <- na.omit(datos)

# Veamos si esta balanceado el método de pago
table(datos$Metodo_Pago)
barplot(table(datos$Metodo_Pago), 
        main = "Intención de compra",
        xlab = "Intención compra", 
        ylab = "Frecuencia", 
        col = "royalblue"
)

# Separamos ids
ids <- datos[,1]
datos <- datos[,-1]

# Pasamos a factor las variables de tipo "character"
datos[,-which(colnames(datos) %in% c("Age", "TV_TOTAL", "Radio_TOTAL"))] <- lapply(datos[,-which(colnames(datos) %in% c("Age", "TV_TOTAL", "Radio_TOTAL"))], as.factor)


# Split Data: ----
datos_idx <- createDataPartition(datos$Metodo_Pago, p = 0.75, list = FALSE)
datos_train <- datos[datos_idx,]
datos_test <- datos[-datos_idx,]

# Separamos los datos test en datos para cv y datos para test:
datos_idx_2 <- createDataPartition(datos_test$Metodo_Pago, p = 0.5, list = FALSE)
datos_cv <- datos_test[-datos_idx_2,]
datos_test <- datos_test[datos_idx_2,]


# Veamos para que cp el arbol tiene mejor precision en los datos de train, el cp controla como 
# de grande va a ser el arbol. Guardamos las precisones de los difefrentes modelos en un vector
# y vemos cuando ocurre el máximo.
acc <- c()
for (cp_n in seq(0.001, 0.1, by = 0.001 )){
  fit.tree = rpart(Metodo_Pago ~ ., data=datos_train, method = "class", cp=cp_n)
  pred.tree = predict(fit.tree, datos_train, type = "class")
  x <- confusionMatrix(pred.tree, datos_train[,1])$overall["Accuracy"]
  acc <- c(acc, x)
}

# Hacemos la gráfica de los datos obtenidos:
df_2_plot <- data.frame(cp = seq(0.001, 0.1, by = 0.001 ),
                        acc = acc)
df_2_plot %>% 
  ggplot(aes(cp, acc)) +
  geom_line() +
  labs(x = "Valor del cp en el modelo", y = "% de precisión")

# En la gráfica vemos cómo cuanto mas cerca del 0 está mas alta es la precisión pero también 
# mayor riesgo de sobreajustar el modelo y que no sea muy preciso en nuestro test. Tambien observamos en la grafica 
# que al llegar a cierto valor de cp se hace constante la precisión ya que en ese momento el árbol es
# lo más simple posible.

# Para que valor de cp, la precisión es máxima:
cp_max_train <- seq(0.001, 0.01, by = 0.001)[which(acc == max(acc))]

# Veamos el arbol:
fit.tree = rpart(Metodo_Pago ~ ., data=datos_train, method = "class", cp=cp_max_train)
fit.tree
pred.tree = predict(fit.tree, datos_train, type = "class")
confusionMatrix(pred.tree, datos_train[,1])

# Visualizar
rpart.plot(fit.tree)
# Observamos que el árbol es demasiado grande y poco visual, por tanto se estará sobre ajustando mucho
# a los datos del set de entrenamiento. Usamos un cp más pequeño

# Veamos el arbol para un cp más pequeño:
fit.tree = rpart(Metodo_Pago ~ ., data=datos_train, method = "class", cp=0.008)
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
  fit.tree = rpart(Metodo_Pago ~ ., data=datos_train, method = "class", cp=cp_n)
  pred.tree = predict(fit.tree, datos_cv, type = "class")
  x <- confusionMatrix(pred.tree, datos_cv[,1])$overall["Accuracy"]
  acc <- c(acc, x)
}

# Hacemos la gráfica de los datos obtenidos:
df_2_plot <- data.frame(cp = seq(0.001, 0.01, by = 0.001 ),
                        acc = acc)
df_2_plot %>% 
  ggplot(aes(cp, acc)) +
  geom_line() +
  labs(x = "Valor del cp en el modelo", y = "% de precisión")

# Justo al contrario que en el set de entrenamiento a cuanto mayor es el cp mayor es
# la precisión ya que el modelo es más general y no sobre ajusta.  Las precisiones son muy similares.
# Todo indica a que cp = 0.008 podría ser un buen valor para el hiperparámetro de crecimiento ya que 
# generaliza bastante bien. Por tanto probamos, como último paso las predicciones sobre el set de testeo:

fit.tree = rpart(Metodo_Pago ~ ., data=datos_train, method = "class", cp=0.008)
rpart.plot(fit.tree, type = 1, tweak = 3)# Se puede apreciar un árbol mucho más visual que es posible de interpretar.
pred.tree = predict(fit.tree, datos_test, type = "class")
confusionMatrix(pred.tree, datos_test[,1])

# Analicemos el arbol final obtenido: 
fit.tree$frame["var"]
# Solo tiene en cuenta 3 variables: Canal_Compra, Estado_Coche, Marca_Coche.
# Por tanto, el método de pago parece que no va a tener en cuenta quién compra si no el qué y cómo.
# Veamos la importancia de las variables:
fit.tree$variable.importance
# Aunque no esté en nuestro arbol Income destaca por encima del resto de variables que no aparecen en el arbol,
# seguida de Age, Tipo_Coche, Tiempo_Coche, Intencion_Compre y ya con mucha menos importancia tenemos:
# Composicion Hogar, Province y TV_TOTAL. Estas variables las usaremos como variables predictores en el
# modelo KNN.

# Este sería el modelo que usaríamos de árbol.
fit.tree = rpart(Metodo_Pago ~ Canal_Compra+Estado_Coche+Marca_Coche+Income+Age+Tipo_Coche+
                               Tiempo_Coche+Intencion_Compra+Composicion_Hogar+Province+TV_TOTAL, 
                 data=datos_train, 
                 method = "class", 
                 cp=0.008)
rpart.plot(fit.tree, type = 1, tweak = 3)# Se puede apreciar un árbol mucho más visual que es posible de interpretar.
pred.tree = predict(fit.tree, datos_test, type = "class")
confusionMatrix(pred.tree, datos_test[,1])
