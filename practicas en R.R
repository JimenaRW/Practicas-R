
#max
maximo <- function(v) {
  largo<-length(v)
  if (largo == 0) return (NA)
  val<-v[1]
  i<-2
  while(i<=largo) {
    if (v[i] > val) val<-v[i]
    i<-i+1
  }
  val
}

# media aritmetica
#mean
promedio <- function(v) {
  if (length(v) == 0) return (NA)
  sum(v)/length(v)
}

#media ponderada
mediaPonderada <- function(x,w){
  if(length(x) == 0) return (NA)
  sum(x * w)
}

# media 
mediaGeom <- function(v) {
  largo<-length(v)
  if (largo == 0) return (NA)
  acum<-v[1]
  i<-2
  while (i<=largo) {
    acum<-acum*v[i]
    i<-i+1
  }
  acum^(1/largo)
}

#median
mediana <- function(v) {
  v<-sort(v)
  largo<-length(v)
  if (largo %% 2 == 1) {
    v[(largo+1)/2]
  } else {
    (v[largo/2]+v[largo/2+1])/2
  }
}

distintos <- function(v) {
  v<-sort(v)
  largo<-length(v)
  if (largo == 0) return (c())
  ant<-v[1]
  salida<-c(ant)
  i<-2
  while(i<=largo) {
    if (ant != v[i]) {
      salida<-c(salida,v[i])
      ant<-v[i]
    }
    i<-i+1
  }
  salida
}

mode <- function(x) { return(as.numeric(names(which.max(table(x))))) } 

moda <- function(v) {
  largov<-length(v)
  dist<-distintos(v)
  largod<-length(dist)
  if (largod == 0) return(NA)
  id<-1
  cantMax<-0
  valMax<-dist[1]
  while(id<=largod) {
    cant<-0
    iv<-1
    while(iv<=largov) {
      if (v[iv] == dist[id]) {
        cant<-cant+1
      }
      iv<-iv+1
    }
    if (cant>cantMax) {
      cantMax<-cant
      valMax<-dist[id]
    }
    id<-id+1
  }
  valMax
}

varianza <- function(v) {
  if (length(v) <= 1) return (NA)
  sum((v-promedio(v))^2)/(length(v)-1)
}
# medidas de dispersion: desviaciones medias
desviacion <- function(x, centro) {
  n <- length(x)  
  suma_desv <- sum(abs(x-centro))
  return (suma_desv / n)
}

#medidas de dispersion: coeficientes de desvio medio
coef_desvio_medias <- function(v, m) {
  return (desviacion(v, m) / m)
}

# coeficiente de desvio medio es una medida de dispersion relativa que expresa la variabilidad de los datos en relacion con una medida de tendencia central (media, mediana y moda)
# su valor nos indica que tan homogeneos o disperson son los datos en comparacion con su valor central
# cmd cercano a 0, valores muy agrupados; cmd de 0.15 relativamente equivatito; cmd de 0.5 gran desigualdad.