dataset <- read.csv("candyhierarchy2017.csv", fileEncoding = "latin1")

## Clean AGE
# Not numbers to => NA
dataset$Q3..AGE = as.numeric(as.character(dataset$Q3..AGE))
#Delete values > 100 age
dataset$Q3..AGE = ifelse(dataset$Q3..AGE > 100, mean(dataset$Q3..AGE, na.rm = T), dataset$Q3..AGE)
# if NA => mean
dataset$Q3..AGE = ifelse(is.na(dataset$Q3..AGE), mean(dataset$Q3..AGE, na.rm = T), dataset$Q3..AGE)
# Round age 0 decimals
dataset$Q3..AGE = as.numeric(format(round(dataset$Q3..AGE, 0)))

#normalizacion lineal
normalize = function(x) {
  return (((x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))))
}

#normalizacion por el valor maximo de los elementos
normalize2 = function(x) {
  return(x / max(x, na.rm = T))
}

# Normalize AGE
dataset$Q3..AGE =  normalize(dataset$Q3..AGE)

#Guardar en dat
saveRDS(dataset$Q3..AGE, file = "normalizacion.dat")

View(dataset$Q3..AGE)

#100 Grand bar column
# Convertir de categorico a numerico 
dataset$Q6...100.Grand.Bar=factor(dataset$Q6...100.Grand.Bar, levels = c('JOY','MEH','DESPAIR'),labels = c(1,2,3))

dataset$Q6...100.Grand.Bar

f = function(x){
  as.numeric(factor(x, levels = c('JOY','MEH','DESPAIR'),labels = c(1,2,3)))
}

# Convertir a numeric
dataset$Q6...100.Grand.Bar = as.numeric(dataset$Q6...100.Grand.Bar)

#  <-------------------------------- 30.37% == NA -------------------------------->  #
#  (length(which(is.na(dataset$Q6...100.Grand.Bar)))/length(dataset$Q6...100.Grand.Bar))*100

#Normalizar el grand bar
dataset$Q6...100.Grand.Bar = normalize(dataset$Q6...100.Grand.Bar)

### Evaluar la distancia ###

# Obtener tabla con las 2 columnas a evaluar
tableDistance1 = data.frame(Age = dataset$Q3..AGE,Q6GrandBar = dataset$Q6...100.Grand.Bar )

# Omit NAs
tableDistance1 = na.omit(tableDistance1)


# Distancia euclideana # ALGORITMO 1
result1 = as.matrix(dist(tableDistance1,method = "euclidean"))

# Distancia minkowski # ALGORITMO 2
result1 = as.matrix(dist(tableDistance1,method = "minkowski"))

# Distancia minkowski # ALGORITMO 3
#result1 = as.matrix(mahalanobis(tableDistance1))

# Distancia minkowski # ALGORITMO 3
result1 = as.matrix(dist(tableDistance1,method = "manhattan"))

help(dist)

View(tableDistance1)
View(result1)

plot(tableDistance1)
plot(result1)

plot(dataset)

#Guardar en dmat
saveRDS(result1, file = "resultado1.dmat")

#Abrir
#dmat = readRDS("resultado1.dmat")
#View(asd)

