# SPLIT DATA: ----

library(caret)

# Devuelve vector de los indices para separar el conjunto de datos en un conjunto de entrenamiento y otro conjunto de prueba.
# createDataPartition(y = "variable a predecir", p = "tamaño del set que se usará para el set de entrenamiento", list = FALSE)

datos_idx <- createDataPartition(datos$Hombre, p = 0.75, list = FALSE)

datos_train <- datos[datos_idx,]
datos_test <- datos[-datos_idx,]

# VER NA's: ----

library(Amelia)

missmap(datos)


# NORMALIZAR: ----

library(dummies)
library(kableExtra)

# Normaliza las columnas de las variables predictoras.

datos_train[,2:ncol(datos_train)] <- lapply(datos_train[,2:ncol(datos_train)],
                                            function(x) (x-min(x))/(max(x)-min(x)))


# ESTUDIO DE CORRELACIONES DE VARIABLES CONTINUAS: ----

library(corrplot)

# Crea una matriz de correlaciones y la grafica con los valores.
m <- cor(datos_train[,2:ncol(datos_train)])
corrplot(m, method="number")

# Si dos variables tienen una correlacion mayor a un cierto nivel, se elimina una de ellas.
m[!lower.tri(m)] <- 0

nivel_correlacion = 0.95

datos_train <- cbind(nombre_variable_predictora = datos_train[,1], 
                        datos_train[, !apply(m, 2, function(x) any(abs(x) > nivel_correlacion))])
datos_test <- cbind(nombre_variable_predictora = datos_test[,1], 
                    datos_test[,!apply(m, 2, function(x) any(abs(x) > nivel_correlacion))])















