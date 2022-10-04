#Ejemplos de coordenadas discriminantes

# Default-----------------
library(ISLR)
library(MASS)

data <- Default

head(data)

X <- scale(cbind(data$balance,
                 data$income))
plot(X, col=data$default, pch=20)

LDA <- lda(X, grouping=data$default)
V <- X %*% LDA$scaling

#Que nos dice esto?
pc.manova(cbind(data[,3],log(data[,4])), data$default)

par(mfrow=c(2,1))
par(mar=c(2,2,2,2))
hist(V[data$default=='Yes'], xlim=c(-5,5),main="",
     col = 'forestgreen')
abline(v=mean(V[data$default=='Yes']))
hist(V[data$default=='No'], xlim=c(-5,5), main="",
     col='firebrick')
abline(v=mean(V[data$default=='No']))

#Armamos una regla de clasificacion, ¿Cómo sería?

#Y si incorporamos la variable student?
Y <- cbind(data$student,X)
LDA <- lda(Y, grouping=data$default)
V <- Y %*% LDA$scaling

par(mfrow=c(2,1))
par(mar=c(2,2,2,2))
hist(V[data$default=='Yes'], xlim=c(-4,5),main="",
     col = 'forestgreen')
abline(v=mean(V[data$default=='Yes']))
hist(V[data$default=='No'], xlim=c(-4,5), main="",
     col='firebrick')
abline(v=mean(V[data$default=='No']))

## Airline employees----------------------
#(Customer Service, Mechanic, and Dispatcher) 
rm(list = setdiff(ls(), lsf.str()))
library(pcosatto)
library(dplyr)
library(haven)

AirlineData <-read_sav("https://stats.idre.ucla.edu/stat/data/discrim.sav")
AirlineData$JOB[AirlineData$JOB==1] <-"CS"
AirlineData$JOB[AirlineData$JOB==2] <-"Mech"
AirlineData$JOB[AirlineData$JOB==3] <-"Disp"
AirlineData$JOB <- factor(AirlineData$JOB)
AirlineData <- AirlineData %>% 
  select(OUTDOOR,SOCIAL,CONSERVATIVE,JOB)

X <- AirlineData %>% 
  select(OUTDOOR,SOCIAL,CONSERVATIVE) %>% 
  as.matrix

pc.plot3d(X, col=AirlineData$JOB,id=AirlineData$JID)

pc.manova(X,AirlineData$JOB) #Que nos dice esto?

LDA <- lda(X, grouping=AirlineData$JOB)
V <- X %*% LDA$scaling
LDA$scaling

names(V) <- c('LD1','LD2')
pc.plot2d(V, col=AirlineData$JOB)

#Qué puedo agregar al análisis?

# Melbourne-----------------
rm(list = setdiff(ls(), lsf.str()))
library(lubridate)
library(dplyr)
melbourne <- read.csv("C:/Users/pcosa/Desktop/82.17/82.17 Presentaciones/Data multivariada/melbourne properties.csv")
melbourne[melbourne$Rooms > 4,]$Rooms <- 4
melbourne$Rooms <- as.factor(melbourne$Rooms)
levels(melbourne$Rooms) <- c('1','2','3','4+')
melbourne <- melbourne %>% na.omit() %>%
  mutate(Age = 2022-YearBuilt,
         Type = factor(Type),
         Regionname = factor(Regionname),
         Price = Price/1000,
         Date = as.Date(Date, "%d/%m/%Y")) %>% 
  filter(BuildingArea > 0, Landsize > 0,
         Landsize < 20000,
         BuildingArea < 2000,
         year(Date)==2017,
         month(Date) %in% c(07,08),
         Age <200) %>% 
  dplyr::select(Rooms, Type, Regionname, Date, Distance,
         Landsize, BuildingArea, Age, Price)

X <- melbourne %>% 
  dplyr::select(Age, Distance,Landsize, BuildingArea) %>% 
  as.matrix() %>% scale()

#Que pasa con PCA
pc.biplot.pca(X,col=melbourne$Rooms,
              adjust_axis_limits = FALSE)

LDA <- lda(X, grouping=melbourne$Rooms)
V <- X %*% LDA$scaling

cor(V,X)
names(V) <- c('LD1','LD2','LD3')
pc.plot3d(V,col=melbourne$Rooms)

#Que se puede agregar al análisis?









