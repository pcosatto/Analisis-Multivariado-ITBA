#Ejemplos de multidimensional scaling (MDS) -------------

### Librerias necesarias
require(readxl)
require(gower) ## Para calcular distancia de Gower
require(smacof) ## Para hacer MDS con SMACOF
require(tidyverse)

### Ciudades (ejemplo simple)-----------------------------
ciudades <- read.delim("C:/Users/pcosa/Desktop/82.17/82.17 Presentaciones/Data multivariada/ciudades.txt", comment.char="#")
mds <- mds(ciudades,2, 'ratio')
plot(mds, lwd=4.)
mds$stress

fitting.plot(ciudades,mds)


### Ejemplo Indumentaria-----------------
rm(list = setdiff(ls(), lsf.str()))
data <- read_excel("Data multivariada/indumentaria.xlsx", 
                   col_types = c("skip", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric"))
n <- nrow(data)
names <- colnames(data)
data_matrix <- as.matrix(data)
rownames(data_matrix) <- names

jaccard <- matrix(NA, nrow=n, ncol=n)
for(i in 1:14){
  jaccard[i,i] <- 1
  if(i <14){
    for(j in (i+1):14){
      a <- data_matrix[i,j]
      b <- sum(data_matrix[i,-i]) 
      c <- sum(data_matrix[-j,j]) 
      
      #Calculamos la similaridad a partir de la
      #similaridad de Jaccard
      jaccard[i,j] <- (a/ (sum(data_matrix[i,]) +sum(data_matrix[,j]) - a))
      jaccard[j,i] <- jaccard[i,j]
    }
  }
}
4

#Analisis en R2 con libreria smacof, MDS interval
mds <- mds(delta,2,'interval')
plot(mds, col='firebrick', lwd=4)
mds$stress
fitting.plot(data_matrix,mds,'maroon4')

#Stress per point
# Dataset 1: one value per group
data <- data.frame(
  name=names,
  val=as.matrix(unname(mds$spp))
)
data %>%
  mutate(name = fct_reorder(name, val)) %>%
  ggplot( aes(x=name, y=val)) +
  geom_bar(stat="identity", fill="maroon", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

# Analisis en R3
mds <- mds(delta,3, 'interval')
X <- mds$conf
group <- kmeans(X,3)$cluster
pc.plot3d(X, col=as.factor(group), id=names)
mds$stress

#Gráfico de fitting
fitting.plot(data_matrix,mds,'maroon')

#Dimensionalidad de los datos
stress <- c()
for(i in 1:10){
  stress[i] <- mds(delta,i, 'interval')$stress
}
stress.gplot(1:10,stress)


#Stress per point
# Dataset 1: one value per group
data <- data.frame(
  name=names,
  val=as.matrix(unname(mds$spp))
)
data %>%
  mutate(name = fct_reorder(name, val)) %>%
  ggplot( aes(x=name, y=val)) +
  geom_bar(stat="identity", fill="maroon4", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()






#Ejemplo cerveza-------------------------------------
rm(list = setdiff(ls(), lsf.str()))
cerveza <- read_excel("Data multivariada/percepcion cerveza.xlsx")[,-1]
cerveza <- as.matrix(cerveza)
View(cerveza)

#Parte I--Hacemos un MDS de los estudiantes--------------

#Vamos a partir de una disimilaridad hecha con la métrica de Manhattan
#¿Por qué? ¿Conviene otra métrica?
dist <- as.matrix(dist(cerveza, method = 'manhattan'))
mds <- mds(dist,2,type='interval')
X <- mds$conf
colnames(X) <- c('dim1', 'dim2')
plot(mds, col='steelblue')


#Medida de bondad de ajuste
mds$stress

#Biplot (superponemos con las cervezas)
biplot <- biplotmds(mds, cerveza)
plot(biplot, col='steelblue')

#Podemos hacer un analisis cluster de los puntos originales
#y agregar colores al biplot de MDS
#Usamos el metodo de kmeans
cluster <- kmeans(cerveza,4)
grupos <- cluster$cluster
plot(biplot, col=as.factor(grupos))

#Ahora probamos en R3, tal vez una dimension mas
#agrega informacion
mds <- mds(dist,3,type='interval')
X <- mds$conf
colnames(X) <- c('dim1', 'dim2', 'dim3')
pc.plot3d(X, col=as.factor(grupos))


#¿Cuantas dimensiones tenemos que usar?
#Dimensionalidad de los datos
stress <- c()
for(i in 1:10){
  stress[i] <- mds(dist,i, 'interval')$stress
}
barplot(stress, main='Stress versus dimensión')

#Consideramos ahora datos ordinales
#Calculamos ahora una métrica de minkowski con p=0.5
cerveza_rank <- t(apply(cerveza,1,rank,ties.method='average'))
View(cerveza_rank)

cluster <- kmeans(cerveza_rank,3)
grupos <- cluster$cluster

#Obtenemos la matriz de distancias
dist <- dist(cerveza_rank, 'minkowski',p=0.5 )
mds <- mds(dist,2,type='interval')
X <- mds$conf
colnames(X) <- c('dim1', 'dim2')

#Medida de bondad de ajuste
mds$stress

biplot <- biplotmds(mds, cerveza_rank, scale=TRUE)
plot(biplot, col=as.factor(grupos))

#Ahora probamos en R3, tal vez una dimension mas
#agrega informacion
mds <- mds(dist,3,type='interval')
X <- mds$conf
colnames(X) <- c('dim1', 'dim2', 'dim3')
pc.plot3d(X, col=as.factor(grupos))


#Parte II - Escalamiento metrico de las cervezas--------------
#Usamos como disimilaridad: 1 - matriz de correlaciones
dist <- 1 - cor(cerveza)
dist
mds <- smacof::mds(dist, 2, type='interval')
X <- mds$conf
colnames(X) <- c('dim1', 'dim2')
View(dist)

#Medida de bondad de ajuste
mds$stress

#Visualizamos las cervezas en 2 dimensiones
plot(mds, col='firebrick')

#Estabilidad de la solución - Método bootstrap
#Cosa piola
boot <- bootmds(mds,dist)
plot(boot, col='firebrick')46



# Ejemplo Bank Churners-----------------
rm(list = setdiff(ls(), lsf.str()))
churn <- read.csv("Data multivariada/churn.csv")
churn <- churn[,-c(1,2,3)]
churn$NoBalance <- as.factor(as.numeric(churn$Balance == 0))

#Ponemos en la mediana a todos los que tienen balance 0
med <- median(churn$Balance[churn$Balance >0])
churn$Balance[churn$Balance == 0] <- med

churn$Geography <- as.factor(churn$Geography)
churn$Gender <- as.factor(churn$Gender)
churn$HasCrCard <- as.factor(churn$HasCrCard)
churn$NumOfProducts <- as.factor(churn$NumOfProducts)
churn$IsActiveMember <- as.factor(churn$IsActiveMember)
churn$Exited <- as.factor(churn$Exited)

## Sampleamos una muestra de 50 personas
set.seed(16497)
n <- 50
id <- sample(1:nrow(churn),n)
churn_subset <- churn[id,]
View(churn_subset)

#Obtenemos la matriz de distancias de Gower
dist <- matrix(NA,n,n)
for(i in 1:n){
  for(j in i:n){
    dist[i,j] <- gower_dist(churn_subset[i,],churn_subset[j,])
    if(i != j) {dist[j,i] <- dist[i,j]}
  }
}
View(dist)

#Hacemos escalaminto multidimensional
mds <- mds(dist,2,'interval')
plot(mds,col=c('forestgreen','firebrick')[churn_subset$Exited], pch=20, lwd=5)
X <- mds$conf

X <- cbind(X,X[,1]^2 + X[,2]^2)
pc.plot3d(X, col=c('forestgreen','firebrick')[churn_subset$Exited])

#Probamos MDS en 3 dimensiones
mds <- mds(dist,3,'interval')
X <- mds$conf
pc.plot3d(X, col=c('forestgreen','firebrick')[churn_subset$Exited])
mds$stress

#Que hubiera pasado si haciamos PCA de entrada
6





