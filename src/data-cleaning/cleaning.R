library(dplyr)
library(readxl)

library(stringr)

library(caret)
library(fastDummies)
library(corrplot)


setwd("F:/MDAT")

automocion <- read_excel("automocion_final.xlsx") %>% as.data.frame()

# 1. Cambios en los nombres de columnas: ----
colnames(automocion)[1:11] <- c("ID",
                                "Permiso_Conducir",
                                "Conductor_Habitual",
                                "Composicion_Hogar",
                                "Propiedad_Coche",
                                "Marca_Coche",
                                "Tipo_Coche",
                                "Tiempo_Coche",
                                "Estado_Coche",
                                "Combustible_Coche",
                                "Canal_Compra")

colnames(automocion)[12:21] <- paste("Visito_",c("Concesionario_Oficial",
                                                 "Concesionario_Multi",
                                                 "Conocido",
                                                 "Subasta",
                                                 "Web_Anuncios",
                                                 "App_Anuncios",
                                                 "Talleres",
                                                 "Web_Oifcial",
                                                 "App_Oficial",
                                                 "Ninguno"))

colnames(automocion)[22:24] <- c("Metodo_Pago",
                                 "Posibilidad_Descarga_App",
                                 "Intencion_Compra")

# Busqueda automatica
colnames(automocion)[25:57] <- paste0("Considero_",
                                      gsub(" ","_",str_match(colnames(automocion)[25:57] , "\\[\\s*(.*?)\\s*\\]")[,2]))

colnames(automocion)[58:61] <- paste0("Considero_",
                                      gsub(" ","_",str_match(colnames(automocion)[58:61] , "\\[\\s*(.*?)\\s*\\]")[,2]))

colnames(automocion)[62:63] <- paste0("Considero_",
                                      c("Gas_Natural","Gas_Licuado"))

colnames(automocion)[66:77] <- str_match(colnames(automocion)[66:77] , "\\[\\s*(.*?)\\s*\\]")[,2]


# 2. Cambiar los "N/A" por NA: ----

automocion[automocion == "N/A"] <- NA


# 3. Cambios en las respuestas: ----
automocion$Tipo_Coche <- car::recode(automocion$Tipo_Coche, 
                                     "'4x4 / Todoterreno' = 'Todoterreno';
                                      'Utilitario o Compacto' = 'Compacto';
                                      'Monovolumen (de 5 a 7 plazas)' = 'Monovolumen';
                                      'SUV mediano' = 'SUV_mediano';
                                      'SUV grande' = 'SUV_grande';
                                      'Furgoneta (más de 7 Plazas)' = 'Furgoneta'")

automocion$Marca_Coche <- gsub(" ", "_", automocion$Marca_Coche)

automocion$Conductor_Habitual <- car::recode(automocion$Conductor_Habitual, 
                                            "'De manera ocasional'='Ocasional'")

automocion$Composicion_Hogar <- car::recode(automocion$Composicion_Hogar, 
                                            "'Vivo con mi padre y/o mi madre'='Padres';
                                             'Vivo con mi pareja e hijo/as'='Pareja_Hijos';
                                             'Vivo con mi/s hijo/as'='Hijos';
                                             'Vivo con mi pareja' = 'Pareja';
                                             'Comparto piso/casa' = 'Piso_Compartido';
                                             'Vivo solo/a' = 'Solo'")

automocion$Composicion_Hogar <- car::recode(automocion$Composicion_Hogar, 
                                            "'Es tuyo propio'='Propio';
                                             'Te lo presta o lo compartes con otra/s persona/s'='Compartido'")

automocion$Tiempo_Coche <- car::recode(automocion$Tiempo_Coche, 
                                       "'1 año o menos' = '<_1';
                                        '2 años' = '2';
                                        '3 años' = '3';
                                        'Entre 4-6 años' = '4-6';
                                        'Entre 7-10 años' = '7-10';
                                        'Más de 10 años' = '>_10'")

automocion$Estado_Coche <- car::recode(automocion$Estado_Coche, 
                                       "'De Kilómetro 0' = 'km_0';
                                        'Seminuevo (de 0 a 25.000 km)' = 'Seminuevo';
                                        'De segunda mano (más de 25.000 km o más de 3 años de antigüedad)' = 'Segunda_Mano'")

automocion$Combustible_Coche <- car::recode(automocion$Combustible_Coche, 
                                            "'GNC (Gas Natural Comprimido)' = 'GNC';
                                             'GLP (Gas Licuado de Petróleo/autogás)' = 'GLP'")

automocion$Canal_Compra <- car::recode(automocion$Canal_Compra, 
                                            "'Concesionario oficial de la marca' = 'Concesionario_Oficial';
                                             'Conocido, familiar.' = 'Conocido';
                                             'Concesionario multimarca' = 'Concesionario_Multi';
                                             'Portal web de anuncios de compraventa' = 'Web_Anuncios';
                                             'Taller mecánico' = 'Taller';
                                             'App de anuncios de compraventa' = 'App_Anuncios';
                                             'Subasta de automóviles' = 'Subasta'")

automocion$Metodo_Pago <- car::recode(automocion$Metodo_Pago, 
                                      "'Financiación bancaria' = 'Financiacion_Banco';
                                       'Leasing (financiación flexible)' = 'Leasing';
                                       'Pago al contado de la totalidad del importe' = 'Al_Contado';
                                       'Renting' = 'Renting';
                                       'Financiación en el concesionario' = 'Financiacion_Concesionario'")

automocion$Intencion_Compra <- car::recode(automocion$Intencion_Compra, 
                                      "'Sí, en este próximo año' = 'Sí_este_año';
                                       'Sí, en 2-3 años' = 'Sí_2_3_años';
                                       'Sí, estoy en el proceso de búsqueda/compra' = 'Sí_buscando';
                                       'No me lo he planteado todavía' = 'No_todavia'")

automocion$Children = car::recode(automocion$Children, "'0'='0_Hijo';'1'='1_Hijo';'2'='2_Hijo';'3'='3_Hijo';'Más de 3'='Mas_3_Hijo'")


# Ceación de nuevas variables que puedan ser útiles:
automocion$TV_TOTAL <- rowSums(automocion[,c("La1","La2","Clan","Antena3","Atreseries",
                                             "Neox", "Nova", "Mega","Energy","Cuatro",
                                             "Telecinco","Boing","FDF","Divinity","Bmad",
                                             "LaSexta","Disney","Dmax","Paramount")],
                               na.rm = TRUE)

automocion$Radio_TOTAL <- rowSums(automocion[,c("Cadena100","CadenaDial","Cope","EuropaFM","Los40",
                                                "Melodia", "Ondacero", "RadioNacional","Ser")],
                                  na.rm = TRUE)

gama_alta = c("Mercedes", "BMW", "Audi", "MINI", "Volvo", "Tesla", "Jaguar",
              "Lexus", "Land_Rover", "Ssang_Yong")
gama_media = c("Nissan", "Suzuki", "Hyundai" , "Subaru",
               "KIA", "Mazda",  "Alfa_Romeo", "Smart",
               "Skoda", "Toyota", "Honda", "Volkswagen", "Ford")
gama_baja = c("Opel", "Peugeot" , "Seat", "Citroën", "Renault", "Fiat", "Dacia")

automocion$Gama_Coche = ifelse(automocion$Marca_Coche %in% gama_alta, "Gama_Alta", 
                 ifelse(automocion$Marca_Coche %in% gama_media, "Gama_Media", "Gama_Baja"))

#Vamos a convertir la variable Income, que es categorica (en forma de intervalos)
#en una variable numerica. Para ello vamos a simular los ingresos reales de los encuestados
set.seed(1234)

#Cargamos el dataset original



#Agregamos la columna Income2

automocion <- mutate(automocion, Income2 = Income, .after = Income)

L = nrow(automocion)

#Vamos a hacerlo en forma de bucle

for (i in 1:L) {
  if (!is.na(automocion[i,"Income"])){
    #Comprobamos si han respondido a la pregunta
    if (automocion[i,"Income"] == 'Hasta 1.000 EUR'){
      #La mayoria se aglutinan cerca del 1000, utilizamos una exponencial
      #Como el parametro es grande no nos van a salir valores mayores que 1
      automocion[i,"Income2"] = c((1-rexp(1,12))*1000)
    }
    if (automocion[i,"Income"] == 'De 1.001 a 1.500 EUR'){
      #Asumimos distribucion normal en este intervalo
      automocion[i,"Income2"] = c(rnorm(1,1250,80))
    }
    if (automocion[i,"Income"] == 'De 1.501 a 2.000 EUR'){
      #Asumimos distribucion normal en este intervalo
      automocion[i,"Income2"] = c(rnorm(1,1750,80))
    }
    if (automocion[i,"Income"] == 'De 2.001 a 2.500 EUR'){
      #El salario medio del pais es aproximadamente 2250 EUR 
      #Consideramos este intervalo como uniforme
      automocion[i,"Income2"] = c(2000+500*runif(1))
    }
    if (automocion[i,"Income"] == 'De 2.501 a 3.000 EUR'){
      #Asumimos distribucion normal en este intervalo
      automocion[i,"Income2"] = c(rnorm(1,2750,80))
    }
    if (automocion[i,"Income"] == 'De 3.001 a 4.000 EUR'){
      #Tomamos una exponencial que acumule valores en torno al inicio del intervalo
      #La truncamos para evitar outliers
      temp = rexp(1,2)
      while (temp > 1){
        temp = rexp(1,2)
      }
      automocion[i,"Income2"] = c(3000+1000*temp)
    }
    if (automocion[i,"Income"] == 'De 4.001 a 5.000 EUR'){
      #Tomamos una exponencial que acumule valores en torno al inicio del intervalo
      #La truncamos para evitar outliers
      temp = rexp(1,2)
      while (temp > 1){
        temp = rexp(1,2)
      }
      automocion[i,"Income2"] = c(4000+1000*temp)
    }
    if (automocion[i,"Income"] == 'Más de 5.000 EUR'){
      #Tomamos una exponencial que acumule valores en torno al inicio del intervalo
      #Aqui no hace falta truncar pues el intervalo no es acotado
      automocion[i,"Income2"] = c((1+rexp(1,10))*5000)
    }
  }
}


# Probando en los diferentes modelos, hemos analizado que, realizando estas agrupaciones en
# los datos, obtenemos diferencias en la precisión menores al 0.01% y sin embargo 
# permiten entender y simplificar las variables.

table(automocion$Composicion_Hogar)
automocion$Composicion_Hogar <- ifelse(automocion$Composicion_Hogar %in% c("Hijos","Otro","Piso_Compartido","Solo"),
                                       "Otro", automocion$Composicion_Hogar)

table(automocion$Estado_Coche)
automocion$Estado_Coche <- ifelse(automocion$Estado_Coche == "Nuevo", "Nuevo", "Seminuevo")

automocion$Combustible_Coche <- ifelse(automocion$Combustible_Coche == "Gasolina", "Gasolina",
                                       ifelse(automocion$Combustible_Coche == "Diesel", "Diesel","Eco"))

table(automocion$Metodo_Pago)
automocion$Metodo_Pago <- ifelse(automocion$Metodo_Pago == "Al_Contado", "Al_Contado",
                                 ifelse(automocion$Metodo_Pago %in% c("Financiacion_Banco","Financiacion_Concesionario"), "Financiacion","Renting/Leasing"))

table(automocion$Intencion_Compra)
automocion$Intencion_Compra <- ifelse(automocion$Intencion_Compra %in% c("Sí_2_3_años","Sí_este_año","Sí, estoy en el proceso de búsqueda/compra"),
                                 "Sí","No")



save(automocion, file = "automocion_limpio2.RData")




# 4. Funcion interesante para graficar la aparicion de NAs: ----

install.packages("Amelia")
library(Amelia)
missmap(automocion[,1:15])
missmap(automocion[,16:30])
missmap(automocion[,31:45])
missmap(automocion[,46:60])
missmap(automocion[,61:75])
missmap(automocion[,76:93])

