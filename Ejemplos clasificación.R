# Toy example 1 de clase teorica--------------------------------
options(scipen=9990); rm(list = ls())
library(MASS)
mu_1 <- c(2,6)
mu_2 <- c(8,4)
Sigma_1 <- matrix(c(5,0,0,5),2,2)
Sigma_2 <- matrix(c(6,4,4,6),2,2)

set.seed(12)
X1 <- mvrnorm(n=100, mu_1, Sigma_1)
X2 <- mvrnorm(n=100, mu_2, Sigma_2)
group <- c(rep(1,100),rep(2,100))
plot(X1, pch=20, col='steelblue', xlim=c(-5,12), ylim=c(-5,12),
     xlab='X1', ylab='X2')
points(X2, pch=20, col='orange')
train <- cbind.data.frame(rbind(X1,X2), group)
names(train) <- c('X1','X2','group')

#Datos para testeo (grilla)
h <- 0.15
x <- seq(-5,12,h)
y <- seq(-5,12,h)
test <- data.frame(expand.grid(x,y))
names(test) <- c('X1','X2')

#Vecinos mas cercanos
library(class)
knn.pred <- knn(train[,1:2], test, train$group, k=20)

#LDA
lda.fit <- lda(group ~ X1 + X2, data=train)
lda.pred <- predict(lda.fit, test)$class

#QDA
qda.fit <- qda(group ~ X1 + X2, data=train)
qda.pred <- predict(qda.fit, test)$class


#Regresion Logistica
reglog.fit <- glm(group-1 ~ X1 + X2, data=train, family=binomial)
summary(reglog.fit)
reglog.pred <- predict(reglog.fit, newdata=test,type='response')
reglog.pred[reglog.pred > 0.5] <- 1
reglog.pred[reglog.pred <= 0.5] <- 2

#Vectores de soporte con variables originales
library(e1071)
library(dplyr)
svc.fit1 <- svm(group ~ X1 + X2, data=mutate(train, group=replace(group,group==2, -1)),
                kernel='linear', cost=0.1)
plot(svc.fit1,train)
svc.pred1 <- predict(svc.fit1, test)
svc.pred1[svc.pred1 > 0] <- 1
svc.pred1[svc.pred1 <= 0] <- 2

#Plot de grilla para ver fronteras
pal <- c('lightskyblue','wheat')
palette(pal)
plot(test, pch=4, col=svc.pred1)
points(X1, pch=20, col='steelblue')
points(X2, pch=20, col='orange')

#-------------Toy example 2--------------------
rm(list = ls())
options(scipen=999)
library(MASS)
mu_1 <- c(2,6)
mu_2 <- c(6,3)
mu_3 <- c(1.5,-0.5)
mu_4 <- c(8.5,-0.5)
Sigma_1 <- matrix(c(5,0,0,5),2,2)
Sigma_2 <- matrix(c(5,1,1,6),2,2)

set.seed(12)
X1 <- rbind(mvrnorm(n=40, mu_1, Sigma_1),
            mvrnorm(n=30, mu_3, Sigma_1/2),
            mvrnorm(n=30, mu_4, Sigma_1/2))
X2 <- mvrnorm(n=100, mu_2, Sigma_2)
group <- c(rep(1,100),rep(2,100))
plot(X1, pch=20, col='darkblue', xlim=c(-5,12), ylim=c(-5,12),
     xlab='X1', ylab='X2')
points(X2, pch=20, col='maroon3')
train <- cbind.data.frame(rbind(X1,X2), group)
names(train) <- c('X1','X2','group')

#Datos para testeo (grilla)
h <- 0.15
x <- seq(-5,12,h)
y <- seq(-5,12,h)
test <- data.frame(expand.grid(x,y))
names(test) <- c('X1','X2')

#Vecinos con k=20
library(class)
knn.pred <- knn(train[,1:2], test, train$group, k=20)

#LDA
lda.fit <- lda(group ~ X1 + X2, data=train)
lda.pred <- predict(lda.fit, test)$class

#QDA
qda.fit <- qda(group ~ X1 + X2, data=train)
qda.pred <- predict(qda.fit, test)$class

#Regresion Logistica
reglog.fit <- glm(group-1 ~ X1 + X2, data=train, family=binomial)
summary(reglog.fit)
reglog.pred <- predict(reglog.fit, newdata=test,type='response')
reglog.pred[reglog.pred > 0.5] <- 1
reglog.pred[reglog.pred <= 0.5] <- 2

#Margen maximo con las variables originales
library(e1071)
library(dplyr)
svc.fit1 <- svm(group ~ X1 + X2, data=mutate(train, group=replace(group,group==2, -1)),
                kernel='linear', cost=8)
plot(svc.fit1,train)
summary(svc.fit1)
svc.pred1 <- predict(svc.fit1, test)
svc.pred1[svc.pred1 > 0] <- 1
svc.pred1[svc.pred1 <= 0] <- 2

#Agregamos una variable
pc.plot3d(mutate(train, X3 = (X1-6)^2 + (X2-3)^2)[,c(1,2,4)], col=as.factor(train$group))
train <- mutate(train, X3 = (X1-6)^2 + (X2-3)^2)

#Margen maximo con una variable agregada
svc.fit2 <- svm(group ~ X1 + X2 + X3, data=mutate(train, group=replace(group,group==2, -1)),
                kernel='linear', cost=8)
svc.pred2 <- predict(svc.fit2, mutate(test, X3 = (X1-6)^2 + (X2-3)^2))
svc.pred2[svc.pred2 > 0] <- 1
svc.pred2[svc.pred2 <= 0] <- 2


#Margen maximo con kernel polinomico de d=2
svm.fit1 <- svm(group ~ X1 + X2, data=mutate(train, group=replace(group,group==2, -1)),
                kernel='polynomial', degree=2, cost=8)
svm.pred1 <- predict(svm.fit1, test)
svm.pred1[svm.pred1 > 0] <- 1
svm.pred1[svm.pred1 <= 0] <- 2

#Margen maximo con kernel polinomico de d=3
svm.fit2 <- svm(group ~ X1 + X2, data=mutate(train, group=replace(group,group==2, -1)),
                kernel='polynomial', degree=3, cost=8)
svm.pred2 <- predict(svm.fit2, test)
svm.pred2[svm.pred2 > 0] <- 1
svm.pred2[svm.pred2 <= 0] <- 2 

#Margen maximo con kernel radial
svm.fit3 <- svm(group ~ X1 + X2, data=mutate(train, group=replace(group,group==2, -1)),
                kernel='radial', cost=8)
svm.pred3 <- predict(svm.fit3, test)
svm.pred3[svm.pred3 > 0] <- 1
svm.pred3[svm.pred3 <= 0] <- 2
 

#Plot de grilla para ver fronteras
metodo <- 6
pal <- c('lightskyblue','rosybrown2')
palette(pal)
plot(test, pch=4, col=switch(metodo,knn.pred,lda.pred,
                             qda.pred,
                             reglog.pred,svc.pred1,svc.pred2,
                             svm.pred1, svm.pred2, svm.pred3))
points(X1, pch=20, col='darkblue')
points(X2, pch=20, col='maroon3')



#-------Dataset con varias variables y dos clases------------------
rm(list = ls())
library(ISLR2)
attach(Smarket)
Smarket <- Smarket[,-1]
Smarket$Direction <- as.factor(Smarket$Direction)

#Splitting the data
set.seed(1)
train <- Smarket[sample(1:nrow(Smarket),875),] 
test <- Smarket[-sample(1:nrow(Smarket),875),-c(7,8)]
true_test <- Smarket[-sample(1:nrow(Smarket),875),8]

#Vecinos
library(class)
library(dplyr)
fit1.pred <- knn(select(train,-Direction),
                 test,
                 factor(train$Direction),
                 k=3)

#QDA
fit2 <- qda(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, 
            data=train)
fit2.pred <- predict(fit2, test)$class


#Logistica
fit3 <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=train, family=binomial) 
fit3.pred <- predict(fit3, newdata=test,type='response')
fit3.pred[fit3.pred > 0.5] <- 1
fit3.pred[fit3.pred <= 0.5] <- 2
fit3.pred <-as.factor(fit3.pred)
levels(fit3.pred) <- c("Down", "Up")

#SVM con kernel polinomico orden 3
library(e1071)
fit4 <- svm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, 
            data=train,
            kernel='radial', cost=8)
fit4.pred <- predict(fit4, test)



#Tablas de confusion y error de prediccion testing
fit <- fit3.pred
table(true_test,fit)
mean(true_test!=fit)

