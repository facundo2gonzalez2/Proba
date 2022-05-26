
rm(list=ls(all=TRUE)) # esto borra las variables definidas hasta el momento

### practica 0

#ej 1a
x <- c(1,2,3)
y <- c(6,5,4)

x * 2
x * y
x[1] * y[2]
1/x
(1:10) * x[2]
rep(c(1,1,2), times = 2)

# ej 2a
a<-rep("A",20)
b<-rep("B",18)
c<-rep("C",22)

c(a,b,c)

# ej 2b
J<-seq(5,100,5)

#ej 3
q = c(3, 0, 1, 6) 
r = c(1, 0, 2, 4)

#ej 3a
q%*%r
sum(q*r)

#ej 3b
a <- rbind(q,r)
matrix(c(q,r),2,4,byrow = TRUE)

#ej 3c
b <- cbind(q,r)

#ej 3d
a%*%b

#ej 3e
solve(a%*%b)

#ej 3f
a[,1]<-c(1,1)
a

#ej 3g
b[,2]

#ej 4
sum((1:100)^2)

#ej 5
help(mtcars)
mtcars

names(mtcars)
colnames(mtcars)  # rownames(mtcars)

#ej 5a
rownames(mtcars)[mtcars$gear==4]

#ej 5b
mtcars[mtcars$disp > 150 & mtcars$mpg >20,]
rownames(mtcars)[mtcars$disp > 150 & mtcars$mpg > 20]

#ej 5c
rownames(mtcars)[mtcars$gear==4 & mtcars$am==1]

#ej 5d
mean(mtcars$mpg[mtcars$carb==2])

#ej 6
library("readxl") #librería necesaria para levantar datos en formato xls o xlsx

arboles<-read_excel(file.choose()) # la ruta donde está el archivo

#ej 6a
length(rownames(arboles))
length(colnames(arboles))
names(arboles)

#ej 6b
mean(arboles$ALTURA_TOT)

#ej 6c
sum(arboles$ESPACIO_VE == "ARENALES")

#ej 6d
arboles_favoritos<-arboles[arboles$ESPACIO_VE == "ARENALES",]
arboles_favoritos$ESPACIO_VE  #verifico

#ej 6e
help(unique)
arboles$NOMBRE_COM

unique(arboles_favoritos$NOMBRE_COM)
unique(arboles$NOMBRE_COM[arboles$ESPACIO_VE == "ARENALES"])

#ej 7
help(sample)

#ej 7a
sample(10,2,replace=FALSE)

#ej 7b
elegir_alumnos <- function(m,n)
{
  sample(1:n,m,replace=FALSE)
}

alumnos<-80
elegir_alumnos(3,alumnos)

#ej 7c
set.seed(1)
alumnos_elegidos <- replicate(10, elegir_alumnos(3,alumnos))
alumnos_elegidos

#ej 8
library(PASWR2)
help("TITANIC3")

#ej 8a
prop_clas_1 <-  mean(TITANIC3$survived[TITANIC3$pclass == "1st"])
prop_clas_2 <-  mean(TITANIC3$survived[TITANIC3$pclass == "2nd"])
prop_clas_3 <-  mean(TITANIC3$survived[TITANIC3$pclass == "3rd"])

#ej 8b
cuales <- TITANIC3$pclass == "1st" & TITANIC3$sex == "female"
mean(TITANIC3$survived[cuales])

outcome <- TITANIC3$survived
factor1 <- TITANIC3$pclass
factor2 <- TITANIC3$sex

lf1 <- length(levels(factor1))
lf2 <- length(levels(factor2))

prop_de_sobrev <- matrix(0,lf1,lf2)
for(i in 1:lf1)
{
  for(j in 1:lf2)
  {
    prop_de_sobrev[i,j] <- mean(outcome[factor1== levels(factor1)[i]& factor2 == levels(factor2)[j]])
  }
}

colnames(prop_de_sobrev) <- c("female", "male")
rownames(prop_de_sobrev) <- c("1st", "2nd", "3rd")

prop_de_sobrev

#ej 8c

help(hist)
help(histogram)

par(mfrow=c(1,2))
hist(c(1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3))  #histograma de frecuencias
histogram(c(1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3))  #histograma de porcentajes
par(mfrow=c(1,1))

par(mfrow=c(1,2))
hist(TITANIC3$age, breaks=20)
histogram(TITANIC3$age, breaks=20)
par(mfrow=c(1,1))

summary(histogram(TITANIC3$age, breaks=20))
length(rownames(TITANIC3))

a<-c(rep(1,20),rep(2,80),rep(3,60),rep(4,40))
aNA<-c(rep(1,20),rep(NA,80),rep(3,60),rep(4,40))

par(mfrow=c(1,2))    # set the plotting area into a 1*2 array
hist(a,ylim=c(0,80))
hist(aNA,ylim=c(0,80))
par(mfrow=c(1,1))

histogram(a,ylim=c(0,80))
histogram(aNA,ylim=c(0,80))

##ej 8d

sort(TITANIC3$age[TITANIC3$sex == "female" & TITANIC3$survived == 1])

## ej9

help(CARS2004)
CARS2004
names(CARS2004)

## ej9a

total_cars <- CARS2004$cars*CARS2004$population

## ej9b

death_rate <- CARS2004$deaths/(CARS2004$cars*10000)
death_rate <- CARS2004$deaths/CARS2004$cars

## ej9c

barplot(death_rate, xlab = "Country", ylab = "Death Rate", names.arg = CARS2004$country)
barplot(sort(death_rate), xlab = "Country", ylab = "Death Rate",names.arg = CARS2004$country[ order(death_rate)])

## ej9d

CARS2004$country[order(death_rate)[1]]

## ej9e

plot(CARS2004$cars,CARS2004$population)

## ej9f

plot(CARS2004$cars,CARS2004$deaths)








