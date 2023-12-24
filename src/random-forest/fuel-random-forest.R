library(dplyr)
library(caret)
library(rpart) 
library(rpart.plot)

library(randomForest)

setwd("F:/MDAT")

load("automocion_limpio2.RData")

load("PCA_CLUST_Opino.RData")

# Semilla: ----
set.seed(112)

# Eliminamos observaciones que contienen NAs en la variable a predecir
datos <- automocion[!is.na(automocion$Combustible_Coche),]

# Diferentes categorias de combustible:
unique(datos$Combustible_Coche)

# Redefinimos Combustible_Coche:
datos <- datos %>%
  filter(Combustible_Coche %in% c("Diesel", "Gasolina"))


# Nos quedamos con las variables predictoras que queremos, son aquellas que dan más imformación
# para el árbol de clasificación creado anteriormemnte.

datos <- datos %>%
  select(ID, Combustible_Coche, Marca_Coche, Tipo_Coche, Province, Tiempo_Coche, Estado_Coche,
         Canal_Compra,Age,Metodo_Pago, Income, TV_TOTAL, Habitat)

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
datos[,-which(colnames(datos) %in% c("Age", "TV_TOTAL", "Radio_TOTAL"))] <- 
  lapply(datos[,-which(colnames(datos) %in% c("Age", "TV_TOTAL", "Radio_TOTAL"))], as.factor)


# Split Data: ----
datos_idx <- createDataPartition(datos$Combustible_Coche, p = 0.75, list = FALSE)
datos_train <- datos[datos_idx,]
datos_test <- datos[-datos_idx,]

# Separamos los datos test en datos para cv y datos para test:
datos_idx_2 <- createDataPartition(datos_test$Combustible_Coche, p = 0.5, list = FALSE)
datos_cv <- datos_test[-datos_idx_2,]
datos_test <- datos_test[datos_idx_2,]

#...............................................................................
# Random Forest ----------------------------------------------------------------
#...............................................................................

# Analizar el % de error según el número de variables por arbol.
error <- c()

for (nvar in 4:12) {
  bag.train <- randomForest(Combustible_Coche ~ ., data = datos_train,
                            mtry = nvar, importance = TRUE, ntree = 100)
  error <- c(error, min(bag.train$err.rate[,"OOB"]))
}
df_2_plot <- data.frame(nvar = 4:12,
                        error = error)
df_2_plot %>% 
  ggplot(aes(nvar, error)) +
  geom_line(colour="blue", size =0.5) +
  geom_point(colour = "blue") +
  labs(x = "Número de variables en el modelo", y = "% de error")


# Parece que lo más óptimo son 7 variables aleatoria en cada árbol. Veamos 
# cuantos árboles debe hacer el modelo para obtener una buena precisión en el cross-validation.

error <- c()
for (ntree2 in seq(40,180, by = 10)) {
  bag.train <- randomForest(Combustible_Coche ~ ., data = datos_train,
                            mtry = 7, importance = TRUE, ntree = ntree2)
  bag.prediction <- predict(bag.train, datos_cv[,-1])
  e <- 1 - confusionMatrix(data = bag.prediction, reference = datos_cv[,1])$overall["Accuracy"]
  error <- c(error, round(e*100, 2))
}
df_2_plot <- data.frame(ntree = seq(40,180, by = 10),
                        error = error)
df_2_plot %>% 
  ggplot(aes(ntree, error)) +
  geom_line(colour="blue", size =0.5) +
  geom_point(colour = "blue") +
  labs(x = "Número de árboles en el modelo", y = "% de error")

# Vemos que sobre los 130 árboles tenemos una buena precisión en comparación al 
# resto del modelo. Además computacionalmente no va a precisar de mucho esfuerzo. 
# Probamos, por tanto el modelo para el test

bag.train <- randomForest(Combustible_Coche ~ ., data = datos_train,
                          mtry = 7, importance = TRUE, ntree = 130)
bag.prediction <- predict(bag.train, datos_test[,-1])
confusionMatrix(data = bag.prediction, reference = datos_test[,1])


# RESULTADOS FINALES: ----

# Reference
# Prediction Diesel Gasolina
# Diesel      178       78
# Gasolina     63      101
# 
# Accuracy : 0.6643 
# Sensitivity : 0.7386          
# Specificity : 0.5642