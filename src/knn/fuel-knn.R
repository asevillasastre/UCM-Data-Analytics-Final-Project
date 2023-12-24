library(caret)
library(dplyr)
library(corrplot)
library(fastDummies)

library(ROCR)
library(caTools)

library(ggplot2)

library(kknn)

setwd("F:/MDAT")
# setwd("E:/MDAT")

load("automocion_limpio2.RData")

load("PCA_CLUST_opino.RData")


# Semilla: ----
set.seed(112)

# Limpiar datos: ----

# Eliminamos observaciones que contienen NAs en la variable a predecir
datos <- automocion[!is.na(automocion$Combustible_Coche),]

# Diferentes categorias de combustible:
unique(datos$Combustible_Coche)

# Redefinimos Combustible_Coche:
datos <- datos %>%
  filter(Combustible_Coche %in% c("Diesel", "Gasolina"))

# Redefinimos intencion_compra:
datos$Intencion_Compra <- ifelse(datos$Intencion_Compra %in% c("Sí_2_3_años","Sí_este_año","Sí, estoy en el proceso de búsqueda/compra"),
                                 "Sí","No")

conteo <- table(datos$Combustible_Coche) %>% as.data.frame()
colnames(conteo) <- c("Tipo de combustible", "Frecuencia")
ggplot(data=conteo, aes(x=`Tipo de combustible`, y=Frecuencia)) +
  geom_bar(stat="identity", fill="steelblue", width = 0.5)+
  geom_text(aes(label=Frecuencia), vjust=1.6, color="white", size=3.5)+
  theme_minimal()


# Nos quedamos con las variables predictoras que queremos
datos <- datos %>%
  select(ID, Combustible_Coche, Marca_Coche, Tipo_Coche, Estado_Coche, Tiempo_Coche, Province) %>%
  left_join(PCA_CLUST_OPINO, by = "ID")

datos <- na.omit(datos)

ids <- datos[,1]
datos <- datos[,-1]

# Pasamos a factor las variables de tipo "character"
datos <- lapply(datos, as.factor) %>% as.data.frame()

# Split Data: ----
datos_idx <- createDataPartition(datos$Combustible_Coche, p = 0.75, list = FALSE)
datos_train <- datos[datos_idx,]
datos_test <- datos[-datos_idx,]

datos_idx2 <- createDataPartition(datos_test$Combustible_Coche, p = 0.5, list = FALSE)
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
  knn_model <- train.kknn(Combustible_Coche ~ ., data = datos_train, ks = k, kcv = 3)
  knn_prediccion <- predict(knn_model, datos_train[,-1])
  e <- 1 - confusionMatrix(data = knn_prediccion, reference = datos_train$Combustible_Coche)$overall["Accuracy"]
  error <- c(error, round(e*100, 2))
}

df_2_plot <- data.frame(knn = seq_k,
                        error = error)
df_2_plot %>% 
  ggplot(aes(knn, error)) +
  geom_line(colour="blue", size =0.5) +
  geom_point(colour = "blue") +
  labs(x = "Valor de k en el modelo", y = "% error")

# Observamos que para números muy bajos de k, se obtiene un error más bajo. A partir
# de k=70 se obtienen errores muy constantes. Por tanto provaremos un número k cerca d
# del 70 en nuestro set de cross-validation para ver como se comporta.

seq_k <- seq(60, 200, 20) 
error <- c()
for (k in seq_k){
  knn_model <- train.kknn(Combustible_Coche ~ ., data = datos_train, ks = k, kcv = 3)
  knn_prediccion <- predict(knn_model, datos_cv[,-1])
  e <- 1 - confusionMatrix(data = knn_prediccion, reference = datos_cv$Combustible_Coche)$overall["Accuracy"]
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

# Obtenemos que el modelo para dar buena precisión debe ser bastante generalista y el número óptimo 
# de vecinos para obtener el mayor porcentaje de precisión en el set de cross-validation es de unos 150

knn.fits_train <- train.kknn(Combustible_Coche ~ ., data = datos_train, ks = kmin_cv)
knn.pred_train <- predict(knn.fits_train, datos_cv[,-1])
confusionMatrix(data = knn.pred_train, reference = datos_cv$Combustible_Coche)


# Probamos en el data test de testeo con k = número de vecinos considerados = 150:
knn_model_final <- train.kknn(Combustible_Coche ~ ., data = datos_train, ks = 150, kcv = 3)
knn_prediccion_final <- predict(knn_model_final, datos_test[,-1])
confusionMatrix(data = knn_prediccion_final, reference = datos_test$Combustible_Coche)

# Reference
# Prediction Diesel Gasolina
# Diesel      243      138
# Gasolina     30       61
# 
# Accuracy : 0.6441   
# Sensitivity : 0.8901         
# Specificity : 0.3065