library(caret)
library(dplyr)
library(corrplot)
library(fastDummies)

library(ROCR)
library(caTools)

library(ggplot2)

library(kknn)

# CARGAR SEGÚN TUS DIRECTORIOS

setwd("F:/MDAT")

load("automocion_limpio2.RData")

load("PCA_CLUST_opino.RData")


# PREPARACIÓN DE LOS DATOS Y LOS SETS DE ENTRENAMIENTO, VALIDACIÓN Y TESTEO: ----

# Semilla:
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
  select(ID, Intencion_Compra, Tiempo_Coche, Age, Province, Marca_Coche, Income, Tipo_Coche) %>%
  left_join(PCA_CLUST_OPINO, by = "ID")
  
datos <- na.omit(datos)
ids <- datos[,1]
datos <- datos[,-1]

# Pasamos a factor las variables de tipo "character"
datos[,-which(colnames(datos) %in% c("Age", "TV_TOTAL", "Radio_TOTAL"))] <- 
  lapply(datos[,-which(colnames(datos) %in% c("Age", "TV_TOTAL", "Radio_TOTAL"))], as.factor)


# Split Data
datos_idx <- createDataPartition(datos$Intencion_Compra, p = 0.75, list = FALSE)
datos_train <- datos[datos_idx,]
datos_test <- datos[-datos_idx,]

datos_idx2 <- createDataPartition(datos_test$Intencion_Compra, p = 0.5, list = FALSE)
datos_test <- datos_test[datos_idx2,]
datos_cv <- datos_test[-datos_idx2,]

#...............................................................................
# KNN --------------------------------------------------------------------------
#...............................................................................


# ¿ Cómo elegimos k para knn? 

# Hacemos un plot del % de error para cada valor de k más general y vemos como se comporta.
seq_k <- seq(10, 150, 10) 
error <- c()
for (k in seq_k){
  knn_model <- train.kknn(Intencion_Compra ~ ., data = datos_train, ks = k, kcv = 3)
  knn_prediccion <- predict(knn_model, datos_train[,-1])
  e <- 1 - confusionMatrix(data = knn_prediccion, reference = datos_train$Intencion_Compra)$overall["Accuracy"]
  error <- c(error, round(e*100, 2))
}
df_2_plot <- data.frame(knn = seq_k,
                        error = error)
df_2_plot %>% 
  ggplot(aes(knn, error)) +
  geom_line(colour="blue", size =0.5) +
  geom_point(colour = "blue") +
  labs(x = "Valor de k en el modelo", y = "% error")

# Vemos que el porcentaje de error aumenta según aumenta el k. Sin embargo no podemos escoger un k
# muy pequeño porque sobre-ajustaría mucho el modelo. Veremos como se comporta en el set
# de cross-validation buscando una k en el intervalo 25-75

seq_k <- seq(50, 100, 5) 
error <- c()
for (k in seq_k){
  knn_model <- train.kknn(Intencion_Compra ~ ., data = datos_train, ks = k, kcv = 3)
  knn_prediccion <- predict(knn_model, datos_cv[,-1])
  e <- 1 - confusionMatrix(data = knn_prediccion, reference = datos_cv$Intencion_Compra)$overall["Accuracy"]
  error <- c(error, round(e*100, 2))
}
df_2_plot <- data.frame(knn = seq_k,
                        error = error)
df_2_plot %>% 
  ggplot(aes(knn, error)) +
  geom_line(colour="blue", size =0.5) +
  geom_point(colour = "blue") +
  labs(x = "Valor de k en el modelo", y = "% error")

kmin_cv = seq_k[which.min(error)]

# Observamos como sería la precisión usando el k que mayor precisión aporta la modelo en 
# el set de cross-validation.
knn.fits_train <- train.kknn(Intencion_Compra ~ ., data = datos_train, ks = kmin_cv)
knn.pred_train <- predict(knn.fits_train, datos_train[,-1])
confusionMatrix(data = knn.pred_train, reference = datos_train$Intencion_Compra)


# RESULTADOS: ----

# Probamos en el data test de testeo:
knn_model_final <- train.kknn(Intencion_Compra ~ ., data = datos_train, ks = 80, kcv = 3)
knn_prediccion_final <- predict(knn_model_final, datos_test[,-1])
confusionMatrix(data = knn_prediccion_final, reference = datos_test$Intencion_Compra)

# Como conclusión sacamos que este modelo es un poco peor que el del árbol, por tanto
# usaríamos el árbol creado anteriormente como nuestro modelo para predecir
# la intención de pago.

#              Reference
# Prediction   No     Sí
# No          172    116
# Sí           83    124
# 
# Accuracy : 0.598
# Sensitivity : 0.6745          
# Specificity : 0.5167
# 'Positive' Class : No