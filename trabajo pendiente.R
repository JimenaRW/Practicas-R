#datos de clientes
clientes <- read.csv("clientes.csv", header = TRUE)

str(clientes)
summary(clientes)

#imprimimos datos
misGrupos <- kmeans(clientes[,-1],2)
print(summary(misGrupos))

#gradicamos los puntos coloreados por grupos
plot(clientes[,-1],col = misGrupos$cluster)

#graficampos los centroides
points(misGrupos$centers, col=1:3, pch= 2, csx= 2)

print(misGrupos$tot.withinss)



# Vector de ejemplo
v <- c(1, 2, 4,8,10,20,40,45,66,1000)

# Calculando cuantiles con probabilidades 0, 0.3333, 0.6666, 1
quantile(v, probs = seq(0, 1, 1/3))


