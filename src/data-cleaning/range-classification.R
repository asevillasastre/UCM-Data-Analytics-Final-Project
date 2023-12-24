automocion
vector <- as.vector(automocion$Marca_Coche)
vector
unique(vector)
gama_alta = c("Mercedes", "BMW", "Audi", "MINI", "Volvo", "Tesla", "Jaguar",
              "Lexus", "Land_Rover", "Ssang_Yong")
gama_media = c("Nissan", "Suzuki", "Hyundai" , "Subaru",
              "KIA", "Mazda",  "Alfa_Romeo", "Smart",
              "Skoda", "Toyota", "Honda", "Volkswagen", "Ford")
gama_baja = c("Opel", "Peugeot" , "Seat", "Citroën", "Renault", "Fiat", "Dacia")
vector2 = c()
vector2 = ifelse(vector %in% gama_alta, 2, 
                 ifelse(vector %in% gama_media, 1, 0))

vector2
mean(vector2)
