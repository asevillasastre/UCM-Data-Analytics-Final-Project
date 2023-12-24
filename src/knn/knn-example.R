library(readr)
library(dplyr)

#Base de datos médicos: 569 observaciones y 32 variables relacionadas con biopsias de tumores, calificados como "M" (maligno) o "B" (benigno).

download.file("https://resources.oreilly.com/examples/9781784393908/raw/ac9fe41596dd42fc3877cfa8ed410dd346c43548/Machine%20Learning%20with%20R,%20Second%20Edition_Code/Chapter%2003/wisc_bc_data.csv", 
              destfile = "data.csv")

data <- read.csv(file = "data.csv")

# Vemos si existe algún NA en el df:
sum(is.na(data))

#Vemos si existen duplicados:
nrow(distinct(data)) == nrow(data)

# Pasamos a factor las categorias
#data$diagnosis <- ifelse(data$diagnosis=="M", 1, 0)

data$diagnosis <- as.factor(data$diagnosis)

#Pasamos a numérico los datos
data[,3:ncol(data)] <- lapply(data[,3:ncol(data)], as.numeric)


# Split quitando la columna id
entrenamiento <-  data[1:469, -1]
prueba        <- data[470:569, -1]


# MODELO 1: 
# MODELO 2: Eliminamos variables correladas
# MODELO 3: Normalizamos datos numéricos
# MODELO 4: Eliminamos variables correladas + Normalizamos datos numéricos


# MODELO 1: ----

# Modelo
modelo1 <- train.kknn(diagnosis ~ ., data = entrenamiento, kmax = 50)

# Predicciones sobre entrenamiento
pred_entrenamiento1 <- predict(modelo1, entrenamiento[,-1])

# Error de entrenamiento
confusionMatrix(data = entrenamiento$diagnosis, reference = pred_entrenamiento)

# Predicciones sobre prueba
pred_prueba1 <- predict(modelo1, prueba[,-1])

# Error de prueba
cm1 <- confusionMatrix(data = prueba$diagnosis, reference = pred_prueba1)



# MODELO 2: ----

# Matriz de correlaciones de predictores (HACER ESTO ANTES DEL SPLIT)
library(corrplot)

correlacion <- cor(entrenamiento[,-1])
correlacion[!lower.tri(correlacion)] <- 0

entrenamiento2 <- cbind(diagnosis = entrenamiento[,1], entrenamiento[, !apply(correlacion, 2, function(x) any(abs(x) > 0.95))])
prueba2 <- cbind(diagnosis = prueba[,1], prueba[,!apply(correlacion, 2, function(x) any(abs(x) > 0.95))])


# Modelo
modelo2 <- train.kknn(diagnosis ~ ., data = entrenamiento2, kmax = 50)

# Predicciones sobre entrenamiento
pred_entrenamiento2 <- predict(modelo2, entrenamiento2[,-1])

# Error de entrenamiento
confusionMatrix(data = entrenamiento2$diagnosis, reference = pred_entrenamiento2)

# Predicciones sobre prueba
pred_prueba2 <- predict(modelo2, prueba2[,-1])

# Error de prueba
cm2 <- confusionMatrix(data = prueba2$diagnosis, reference = pred_prueba2)



# MODELO 3: ----

# Normalizamos datos numéricos:
nor <- function(x) { (x -min(x))/(max(x)-min(x))}

entrenamiento3 <- entrenamiento
prueba3 <- prueba

entrenamiento3[,2:ncol(entrenamiento)] <- lapply(entrenamiento[,2:ncol(entrenamiento)],
                                                 function(x) nor(x))
prueba3[,2:ncol(prueba)] <- lapply(prueba[,2:ncol(prueba)],
                                   function(x) nor(x))

# Modelo
modelo3 <- train.kknn(diagnosis ~ ., data = entrenamiento3, kmax = 50)

# Predicciones sobre entrenamiento
pred_entrenamiento3 <- predict(modelo3, entrenamiento3[,-1])

# Error de entrenamiento
confusionMatrix(data = entrenamiento3$diagnosis, reference = pred_entrenamiento3)

# Predicciones sobre prueba
pred_prueba3 <- predict(modelo3, prueba3[,-1])

# Error de prueba
cm3 <- confusionMatrix(data = prueba3$diagnosis, reference = pred_prueba3)


# MODELO 4: ----

# Matriz de correlaciones de predictores (HACER ESTO ANTES DEL SPLIT)
library(corrplot)

correlacion <- cor(entrenamiento[,-1])
correlacion[!lower.tri(correlacion)] <- 0

entrenamiento4 <- cbind(diagnosis = entrenamiento[,1], entrenamiento[, !apply(correlacion, 2, function(x) any(abs(x) > 0.95))])
prueba4 <- cbind(diagnosis = prueba[,1], prueba[,!apply(correlacion, 2, function(x) any(abs(x) > 0.95))])


# Normalizamos datos numéricos:
nor <- function(x) { (x -min(x))/(max(x)-min(x))}

entrenamiento4[,2:ncol(entrenamiento)] <- lapply(entrenamiento[,2:ncol(entrenamiento)],
                                                 function(x) nor(x))
prueba4[,2:ncol(prueba)] <- lapply(prueba[,2:ncol(prueba)],
                                   function(x) nor(x))

# Modelo
modelo4 <- train.kknn(diagnosis ~ ., data = entrenamiento4, kmax = 50)

# Predicciones sobre entrenamiento
pred_entrenamiento4 <- predict(modelo4, entrenamiento4[,-1])

# Error de entrenamiento
confusionMatrix(data = entrenamiento4$diagnosis, reference = pred_entrenamiento4)

# Predicciones sobre prueba
pred_prueba4 <- predict(modelo4, prueba4[,-1])

# Error de prueba
cm4 <- confusionMatrix(data = prueba4$diagnosis, reference = pred_prueba4)



# COMPARACIÓN: ----

comparacion <- data.frame(matrix(nrow = 0, ncol = 9))

for (i in 1:4){
  
  aux <- get(paste0("cm",i))
  
  nombre <- paste0("Modelo ",i)
  
  fp <- aux$table[1,2]
  fn <- aux$table[2,1]
  
  accu <- as.numeric(aux$overall["Accuracy"])
  
  sens <- as.numeric(aux$byClass["Sensitivity"])
  spec <- as.numeric(aux$byClass["Specificity"])
  
  precision <- as.numeric(aux$byClass["Precision"])
  recall <- as.numeric(aux$byClass["Recall"])
    
  f1 <- as.numeric(aux$byClass["F1"])
  
  comparacion <- rbind(comparacion, c(nombre, fp, fn, accu, sens, spec, precision, recall, f1))
  
}

comparacion[,2:ncol(comparacion)] <- lapply(comparacion[,2:ncol(comparacion)], as.numeric)
colnames(comparacion) <- c("Modelo", "False B", "False M", "Accuracy", 
                           "Sensitivity","Specificity", "Precision", "Recall", "F1 Score")


# CONCLUSIÓN: ----

# CONTAMOS COMO POSITIVOS LOS BENIGNOS(B)

# El mejor modelo es el Modelo 2 por muchas razones:
# Al ser un tema médico, preferimos tener falsos malignos que falsos benignos.
# Mejor F1 Score que relaciona el recall con la precision


