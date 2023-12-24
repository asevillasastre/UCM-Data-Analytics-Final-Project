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

# CANALES: ----
# Cargamos PCA de los canales
# load("canales_pca.RData")
# canales_pca <- canales_pca[,1:10]
# # Cargamos CLUSTER de los canales
# load("canales_clust.RData")
# canales_pca <- canales_pca[,1:21]
# 
# 
# # VISITO: ----
# # Cargamos PCA de los visito 
# load("visito_pca.RData")
# visito_pca <- visito_pca[,1:5]
# # Cargamos MCA de los visito 
# load("visito_mca.RData")
# visito_mca <- visito_mca[,1:5]
# # Cargamos CLUSTER de los visito
# load("visito_clust.RData")
# canales_pca <- canales_pca[,1:21]

# OPINO: ----
load("PCA_CLUST_opino.RData")
load("opino_clus.RData")


# Semilla: ----
set.seed(100)

# Limpiar datos: ----

# Eliminamos observaciones que contienen NAs en la variable a predecir
datos <- automocion[!is.na(automocion$Metodo_Pago),]

# Diferentes categorias de combustible:
unique(datos$Metodo_Pago)
datos <- datos %>%
  filter(Metodo_Pago %in% c("Financiacion","Al_Contado"))


table(datos$Metodo_Pago)
barplot(table(datos$Metodo_Pago), 
        main = "Métodos de pago",
        xlab = "Métodos de pago", 
        ylab = "Frecuencia", 
        col = "royalblue"
)

# Nos quedamos con las variables predictoras que queremos
datos <- datos %>%
  # select(ID, Metodo_Pago, Sex, Age, Income, Composicion_Hogar, Habitat, Estado_Coche) %>%
  select(ID, Metodo_Pago, Canal_Compra, Estado_Coche, Marca_Coche, Income, Age, Tipo_Coche,
         Tiempo_Coche, Intencion_Compra, Composicion_Hogar, Province, TV_TOTAL)
  # select(ID, Metodo_Pago, Age, Income, Composicion_Hogar, Metodo_Pago) %>%
  # left_join(PCA_CLUST_OPINO, by = "ID")
  # left_join(canales_pca, by = c("ID"="id")) %>%
  # left_join(visito_mca, by = "ID")
  # left_join(opino_clust, by = "ID")

datos <- na.omit(datos)
ids <- datos[,1]
datos <- datos[,-1]

# Pasamos a factor las variables de tipo "character"
datos[,-which(colnames(datos) %in% c("Age", "TV_TOTAL", "Radio_TOTAL"))] <- 
  lapply(datos[,-which(colnames(datos) %in% c("Age", "TV_TOTAL", "Radio_TOTAL"))], as.factor)

# Split Data: ----
datos_idx <- createDataPartition(datos$Metodo_Pago, p = 0.75, list = FALSE)
datos_train <- datos[datos_idx,]
datos_test <- datos[-datos_idx,]

datos_idx2 <- createDataPartition(datos_test$Metodo_Pago, p = 0.5, list = FALSE)
datos_test <- datos_test[datos_idx2,]
datos_cv <- datos_test[-datos_idx2,]

# Para este KNN no haremos cross-validation por 2 razones:
#  1. Porque ya viene implementado en la función KNN
#  2. Porque en el árbol queríamos obtener un arbol bastante generalista que nos
#     permitiese saber cuáles eran las mejores variables predictoras. En este sobretodo
#     buscamos obtener predicciones correctas.

#...............................................................................
# KNN --------------------------------------------------------------------------
#...............................................................................


# ¿ Cómo elegimos k para knn? 

# Hacemos un plot del % de error para cada valor de k más general y vemos como se comporta.
seq_k <- seq(10, 150, 10) 
error <- c()
for (k in seq_k){
  knn_model <- train.kknn(Metodo_Pago ~ ., data = datos_train, ks = k, kcv = 3)
  knn_prediccion <- predict(knn_model, datos_train[,-1])
  e <- 1 - confusionMatrix(data = knn_prediccion, reference = datos_train$Metodo_Pago)$overall["Accuracy"]
  error <- c(error, round(e*100, 2))
}
df_2_plot <- data.frame(knn = seq_k,
                        error = error)
df_2_plot %>% 
  ggplot(aes(knn, error)) +
  geom_line() +
  labs(x = "Valor de k en el modelo", y = "% error")

# Vemos que el porcentaje de error comienza a mantenerse con el k = 50 más o menos

# Hacemos un plot del % de error para cada valor de k entre 50 y 120, valores que
# hemos ajustado en el set de entrenamiento. Y probamos en el set de cross-validation y así
# terminar de ajustar los hiper parámetros del modelo.

seq_k <- seq(50, 120, 5) 
error <- c()
for (k in seq_k){
  knn_model <- train.kknn(Metodo_Pago ~ ., data = datos_train, ks = k, kcv = 3)
  knn_prediccion <- predict(knn_model, datos_cv[,-1])
  e <- 1 - confusionMatrix(data = knn_prediccion, reference = datos_cv$Metodo_Pago)$overall["Accuracy"]
  error <- c(error, round(e*100, 2))
}
df_2_plot <- data.frame(knn = seq_k,
                        error = error)
df_2_plot %>% 
  ggplot(aes(knn, error)) +
  geom_line() +
  labs(x = "Valor de k en el modelo", y = "% error")

kmin_cv = seq_k[which.min(error)]

knn.fits_train <- train.kknn(Metodo_Pago ~ ., data = datos_train, ks = kmin_cv)
knn.pred_train <- predict(knn.fits_train, datos_train[,-1])
confusionMatrix(data = knn.pred_train, reference = datos_train$Metodo_Pago)


# Probamos en el data test de testeo con k = número de vecinos considerados = 60:

knn_model_final <- train.kknn(Metodo_Pago ~ ., data = datos_train, ks = 60, kcv = 3)
knn_prediccion_final <- predict(knn_model_final, datos_test[,-1])
confusionMatrix(data = knn_prediccion_final, reference = datos_test$Metodo_Pago)

