#Vamos a intentar utilizar árboles de regresion para predecir los ingresos (Income2)
#a partir de los datos de los clientes, tanto personales como de uso del vehiculo

#Antes de proceder, cabe destacar dos cosas:
#   Vamos a intentar predecir un valor ficticio, pues realmente no disponemos de los
#datos concretos de ingresos. Esos datos han sido simulados a partir de la variable Income,
#variable categorica que nos da rangos de ingresos pero no datos exactos
#   La gran mayoria de variables en nuestro dataset son categoricas, lo que dificulta
#la aplicacion de otros modelos de regresion.

#Por ello, es posible que el error que obtengamos sea grande, pero vamos a hacer lo
#posible por afinar al maximo y poder sacar unas conclusiones razonables


#Primero cargamos librerias y el dataset modificado con los valores de Income2 simulados

set.seed(1234)
library(ISLR2)
library(tree)

setwd("C:/Users/toled/OneDrive/Documentos/MATEMÁTICAS/MDAT")

load("auto_Income2.RData")

#Vamos a excluir las observaciones en que el usuario no tenga carne de conducir,
#pues todos nuestros predictores requieren que lo tenga

automocion2 <- automocion2[!is.na(automocion2$'Conductor_Habitual'),]

L = nrow(automocion2)

#Convertimos Income2 a variable numerica, y todas las que vienen como tipo char
#las pasamos a factor

automocion2$Income2 <- as.numeric(automocion2$Income2)

automocion2 <- as.data.frame(unclass(automocion2),
                             stringsAsFactors = TRUE)

#Como queremos predecir los ingresos, Income2, obviamos las observaciones NA

automocion2 <- automocion2[!is.na(automocion2$'Income2'),]

#Tomamos como conjunto de entrenamiento dos tercios de la muestra.
#Nuestros predictores van a ser fundamentalmente datos personales y familiares, datos sobre
#el uso del coche y algunas preferencias a la hora de comprar un nuevo vehiculo

train <- sample(1:nrow(automocion2), nrow(automocion2) / 1.5)
tree.auto <- tree(Income2 ~ Sex+Age+Habitat+Children+Activity+EducationLevel+
                    Conductor_Habitual+Composicion_Hogar+Propiedad_Coche+Marca_Coche+
                    Tipo_Coche+Tiempo_Coche+Metodo_Pago+Intencion_Compra+
                    Estado_Coche+Combustible_Coche+Canal_Compra,
                  automocion2, subset = train)
summary(tree.auto)

#Vemos que quedan bastantes nodos terminales y que se utilizan muchas de las variables

plot(tree.auto)
text(tree.auto, pretty = 0)

#La primera ramificacion se produce segun la actividad laboral de los encuestados.
#Despues, el nivel de estudios, y a continuacion la marca de coche

#Vamos a ver si podemos podar el arbol

cv.auto <- cv.tree(tree.auto)
plot(cv.auto$size, cv.auto$dev, type = "b")

#En un modelo de arbol normal podriamos reducir el arbol hasta 4 o 5 nodos,
#pero como queremos obtener el menor error posible vamos a podar hasta 6

prune.auto <- prune.tree(tree.auto, best = 6)
plot(prune.auto)
text(prune.auto, pretty = 0)

#Ademas de las variables antes mencionadas, tambien tenemos ramificaciones
#asociadas al numero de hijos y a la intencion de compra

#Vamos a determinar ahora el MSE en la prediccion utilizando el conjunto de test
#De nuevo, como nuestros datos son simulados y ademas de un orden de magnitud
#relativamente grande, lo normal es que el numero que obtengamos sea grande

yhat <- predict(tree.auto, newdata = automocion2[-train, ])
auto.test <- automocion2[-train, "Income2"]
plot(yhat, auto.test)
abline(0, 1)
err1 = mean((yhat - auto.test)^2)

#El error es del orden de 330000, aunque como vemos la grafica parece bastante razonable
#Merece la pena intentar mejorar el modelo

#Para ello vamos a aplicar Boosting. Y de nuevo, como queremos reducir el error todo lo
#posible, vamos a probar diferentes niveles de profundidad en la interaccion
#y diversas cantidades de arboles para encontrar el mas eficiente

library(gbm)
min=vector(mode="numeric",length=3) #Aqui guardaremos los parametros que mejores resultados den
min[3] = Inf
set.seed(1234)
for (i in 1:5){
  for (j in -1:6){
    boost.auto <- gbm(Income2 ~ Sex+Age+Habitat+Children+Activity+EducationLevel+
                        Conductor_Habitual+Composicion_Hogar+Propiedad_Coche+Marca_Coche+
                        Tipo_Coche+Tiempo_Coche+Metodo_Pago+Intencion_Compra+
                        Estado_Coche+Combustible_Coche+Canal_Compra, data = automocion2[train, ],
                      distribution = "gaussian", n.trees = 5000+2500*j,
                      interaction.depth = i)
    yhat.boost <- predict(boost.auto,
                          newdata = automocion2[-train, ], n.trees = 5000+2500*j)
    temp = mean((yhat.boost - auto.test)^2)
    
    if (temp < min[3]){
      min[1] = i
      min[2] = j
      min[3] = temp
    }
    print(i)
    print(j)
    print(temp)
    
  }
}

#Vemos que los mejores resultados se producen cuando interaction.depth = 1
#En ese caso, el numero de arboles no parece tener mucho en impacto. En algunos test
#el "optimo" estaba en 2500, otras en 5000, otras en 10000

#Para niveles de interaccion mayores si que vemos una tendencia -> a mas arboles, mas error

#Estudiamos ahora en profundidad el mejor caso que hemos obtenido

i = min[1]
j = min[2]

boost.auto <- gbm(Income2 ~ Sex+Age+Habitat+Children+Activity+EducationLevel+
                    Conductor_Habitual+Composicion_Hogar+Propiedad_Coche+Marca_Coche+
                    Tipo_Coche+Tiempo_Coche+Metodo_Pago+Intencion_Compra+
                    Estado_Coche+Combustible_Coche+Canal_Compra, data = automocion2[train, ],
                  distribution = "gaussian", n.trees = 5000+2500*j,
                  interaction.depth = i)

yhat.boost <- predict(boost.auto,
                      newdata = automocion2[-train, ], n.trees = 5000+2500*j)
err2 = mean((yhat.boost - auto.test)^2)

#Ahora el error cuadratico medio obtenido es del orden de 290000, aproximadamente un
#12% mejor que con el arbol normal

summary(boost.auto)

#Vemos que, con una diferencia abismal, la variable con mayor influencia es la marca
#de coche. La segunda, lejos de la primera pero con mucho mas impacto que el resto, es el sexo
#Podemos decir que tiene sentido inferir el sueldo de una persona segun la marca de su coche
#Habra algunos casos particulares, como alguien que tenga un coche heredado, o que tenga
#un modelo de gama baja pero se pueda permitir un coche mejor, pero en general
#parece un medidor bastante acertado. Y en los casos de duda, el sexo de la persona puede
#ser otro factor importante, ya que desgraciadamente sigue habiendo una brecha salarial
#entre hombres y mujeres.

#Para intentar afinar aun mas, vamos a reescalar los datos con un escalador min-max
#Las conclusiones que obtengamos de los datos reescalados las podemos interpretar como
#la diferencia entre el percentil en terminos de salario que predecimos y el real

#De nuevo, hay que recordar que nuestros datos numericos son simulados, pero si
#con esta normalizacion conseguimos un error pequeño, entonces aunque no podamos
#predecir con exactitud los ingresos, si que podriamos encajar a los clientes dentro de
#los rangos de la variable original Income

minmaxscale <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}

automocion2$Income2 = minmaxscale(automocion2$Income2)

boost.auto2 <- gbm(Income2 ~ Sex+Age+Habitat+Children+Activity+EducationLevel+
                    Conductor_Habitual+Composicion_Hogar+Propiedad_Coche+Marca_Coche+
                    Tipo_Coche+Tiempo_Coche+Metodo_Pago+Intencion_Compra+
                    Estado_Coche+Combustible_Coche+Canal_Compra, data = automocion2[train, ],
                  distribution = "gaussian", n.trees = 5000+2500*j,
                  interaction.depth = i)

yhat.boost2 <- predict(boost.auto2,
                      newdata = automocion2[-train, ], n.trees = 5000+2500*j)
auto.test2 <- automocion2[-train, "Income2"]
err3 = mean((yhat.boost2 - auto.test2)^2)

#Vemos que el error 3 se ubica en torno a 0.008, es decir, menos de un 1% en terminos
#de percentil. Podemos asumir entonces que, como sospechabamos, la variable Marca_Coche
#es bastante buena para predecir los ingresos de una persona

summary(boost.auto2)

#Para terminar, vemos el resumen del Boosting reescalado para comprobar que efectivamente
#los resultados son consistentes con lo obtenido anteriormente