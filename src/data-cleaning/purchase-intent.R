library(caret)
library(dplyr)
library(corrplot)
library(fastDummies)

library(ROCR)
library(caTools)

library(ggplot2)

setwd("F:/MDAT")

load("automocion_limpio2.RData")

load("PCA_CLUST_opino.RData")


# PREPARACIÓN DE LOS DATOS Y LOS SETS DE ENTRENAMIENTO, VALIDACIÓN Y TESTEO: ----

set.seed(105)

# Eliminamos observaciones que contienen NAs en la variable a predecir
datos <- automocion[!is.na(automocion$Intencion_Compra),]

# Diferentes categorias de combustible:
unique(datos$Intencion_Compra)

table(datos$Intencion_Compra)
barplot(table(datos$Intencion_Compra), 
     main = "Intención de compra",
     xlab = "Intención compra", 
     ylab = "Frecuencia", 
     col = "royalblue"
     )

# Nos quedamos con las variables predictoras que queremos, provienen de la info que nos 
# daba el árbol realizado previamente.
datos <- datos %>%
  select(ID, Intencion_Compra, Tiempo_Coche, Age, Province, Gama_Coche, Income, Tipo_Coche) %>%
  left_join(PCA_CLUST_OPINO, by = "ID")

datos <- na.omit(datos)

ids <- datos[,1]
datos <- datos[,-1]

# Pasamos a factor las variables de tipo "character":
datos[,-which(colnames(datos) %in% c("Age", "TV_TOTAL", "Radio_TOTAL"))] <- 
  lapply(datos[,-which(colnames(datos) %in% c("Age", "TV_TOTAL", "Radio_TOTAL"))], as.factor)


# Split Data:
datos_idx <- createDataPartition(datos$Intencion_Compra, p = 0.75, list = FALSE)
datos_train <- datos[datos_idx,]
datos_test <- datos[-datos_idx,]



# AJUSTE DE LA REGRESIÓN LOGÍSTICA: ----

# No usaremos el set de validación, ya que no existen muchos hiperparámetros 
# que ajustar.

# Ajustar modelo:
glm.fits <- glm(Intencion_Compra ~ ., # formula
                data = datos_train, # set de entrenamiento 
                family = "binomial")
summary(glm.fits)

threshold = 0.5

# Calcular probabilidades para el set de entrenamiento:
glm.probs_train <- predict(glm.fits,
                           datos_train,
                           type = "response")

# Predicciones dadas las probabilidades:
glm.pred_train <- ifelse(glm.probs_train > threshold, "Sí", "No")
glm.pred_train <- as.factor(glm.pred_train)

confusionMatrix(data = glm.pred_train, reference = datos_train$Intencion_Compra)

# Calcular probabilidades para el set de testeo:
glm.probs_test <- predict(glm.fits, # modelo
                          datos_test,
                          type = "response")
glm.pred_test <- ifelse(glm.probs_test > threshold, "Sí", "No")
glm.pred_test <- as.factor(glm.pred_test)

confusionMatrix(data = glm.pred_test, reference = datos_test$Intencion_Compra)


# RESULTADOS: ----

# Reference
# Prediction  No  Sí
# No         349 210
# Sí         160 269
# 
# Accuracy : 0.6255 
# Sensitivity : 0.6857          
# Specificity : 0.5616     
