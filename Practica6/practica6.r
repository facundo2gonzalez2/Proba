#PRACTICA 6
####ejercicio 1####
data=read.table(file= "alfajores.txt" ,header=TRUE)
n=500

#a
proba_quilmes = sum(data$fabrica==0)/n
proba_quilmes

#b
proba_quilmesy3defec = sum(data$defectuosos==3 & data$fabrica==0)/n
proba_quilmesy3defec

#c
pxy = t(table(data)) / n
pxy

#d
mean(data$defectuosos)

var(data$defectuosos)

#e
#P(X=3|Y=0)= Pxy(3,0)/Py(0)
pxy[1,4] / sum(pxy[1,])

#f
# Px(3)
sum(pxy[,4])

#g
#P(Y=0|X=3) = Pxy(0,3) / Px(3)
pxy[1,4] / sum(pxy[,4])

#h
#P(Y=1|X=3) = Pxy(1,3) / Px(3)
pxy[2,4] / sum(pxy[,4])

####ejercicio 2####
lamparas=read.table(file="lamparas.txt", header = FALSE)
cant_lamparas=26

#a

proba_mas30=sum(lamparas>30)/cant_lamparas
proba_mas30

#b


#c
quantile(lamparas$V1,0.1)

####ejercicio 3####

graduados=read.table(file="graduados.txt", header = FALSE)
#a
mean(graduados$V1)
median(graduados$V1)

#b
sd(graduados$V1)
distanciaIntercuantil = quantile(graduados$V1,0.75)-quantile(graduados$V1,0.25)
distanciaIntercuantil
IQR(graduados$V1)

#c
hist(graduados$V1)

#d
boxplot(graduados$V1)

#e
#supongo una distribucion normal

#f
hist(graduados$V1, prob=TRUE)
curve(dnorm(x, mean = mean(graduados$V1), sd= sd(graduados$V1)), add=TRUE)

help("density")
d<- density(graduados$V1)
plot(d)

#g
#veo con respecto a un qqplot

with(data=graduados, qqnorm(V1))
qqline(graduados$V1, col = "steelblue", lwd = 2)



####ejercicio 4####
ciudades=read.table(file="ciudades.txt", header=TRUE)
ciudades
#a
boxplot(ciudades$Argentina, ciudades$EEUU, ciudades$Holanda, ciudades$Japon, 
        names=c("Arg","EEUU","Hol","Jap"))

#b
mediaArg=mean(ciudades$Argentina)
mediaEEUU=mean(ciudades$EEUU)
mediaHol=mean(ciudades$Holanda)
mediaJap=mean(ciudades$Japon)


medianaArg=median(ciudades$Argentina)
medianaEEUU=median(ciudades$EEUU)
medianaHol=median(ciudades$Holanda)
medianaJap=median(ciudades$Japon)

sdArg=sd(ciudades$Argentina)
sdEEUU=sd(ciudades$EEUU)
sdHol=sd(ciudades$Holanda)
sdJap=sd(ciudades$Japon)


centralidadfunc <- data.frame( 
  Media = c(mediaArg,mediaEEUU,mediaHol,mediaJap),
  Mediana = c(medianaArg,medianaEEUU,medianaHol,medianaJap),
  Desvio = c(sdArg, sdEEUU, sdHol, sdJap))
centralidadfunc

#viendo los datos, parece ser Holanda el país más homogeneamente poblado.

####ejercicio 5####
ingresos=read.table(file="ingresos.txt",header=FALSE)
ingresos

#a
ingresoMinimo=min(ingresos$V1)
proba_ingresoMinimo=sum(ingresos$V1==ingresoMinimo)/1000
proba_ingresoMinimo


#b
quantile(ingresos$V1,0.9)

#c
mean(ingresos$V1)
median(ingresos$V1)
mean(ingresos$V1,0.1)

#d
sd(ingresos$V1)
IQR(ingresos$V1)

#e

hist(ingresos$V1)
boxplot(ingresos$V1)
#hay muchos outliers

#f
#si algo no es es una distribucion normal




####ejercicio 6####
# (descomentar de a una linea, comentando la otra para plotear diferentes funciones)

#Muestras de 25,50 y 100 de una variable con dist normal
set.seed(1001001)
for (n in c(25,50,100)){
  ej6muestra <- rnorm(n); nombre <- "Normal 0, 1"
  #ej6muestra <- rgamma(n,scale=5, shape=1/2); nombre <- "Gamma 5, 1/2)"
  #ej6muestra <- rnorm(n)/runif(n,min=0,max=1); nombre <- "Normal 0, 1 dividido Uniforme 0, 1"
  #ej6muestra <- runif(n,min=0,max=1); nombre <- "Uniforme 0, 1"
  #ej6muestra <- rexp(n); nombre <- "Exponencial 1"
  
  qqnorm(ej6muestra, main=nombre)
}

#se distingue facilmente entre una variable normal y una que no lo es







####ejercicio 7####
#7.a
n <- 1000
vector <- runif(n)
l2 <- function(c) {
  res <- 0
  for(val in vector) {
    res <- res + (val-c)**2
  }
  return(res)
}
x <- seq(-500, 500, by=1)
plot(x, l2(x))

#la funciÃ³n se minimiza en 0
#con distintos valores de n sigue siguendo una cuadrÃ¡tica centrada en 0
#sin embargo, si obtengo mi vector de una distribuciÃ³n uniforme muy grande (100000000), aparece una recta en lugar de una cuadrÃ¡tica.

#falta demostrarlo


#7.b
l1 <- function(c) {
  res <- 0
  for(val in vector) {
    res <- res + abs(val-c)
  }
  return(res)
}
plot(x, l1(x))

#nuevamente la función se minimiza en 0, y es un módulo








####ejercicio 8####

#a
vectorAl <- rnorm(n)
x1=mean(vectorAl)
x2=median(vectorAl)
vectorAux = c(vectorAl+5)
y1=mean(vectorAux)
y2=median(vectorAux)
#y1,x1 y y2,x2 se diferencian en la constante agregada 5

vectorAux2 = c(vectorAl*5)
y11=mean(vectorAux2)
y22=median(vectorAux2)
x1*5==y11 #da false porque debe variar en algun decimal
x2*5==y22 #devuelve true
#x1 *5 = y11 y x2*5  =  y22

#b
vectorFuncX=rnorm(100)
vectorFuncY=c(vectorFuncX+10)


s2x=function(x){
  res=0
  for (val in vectorFuncX) {
    res=res+ (val-mean(vectorFuncX))**2
  }
  return= (1/99)*res
}

s2y=function(y){
  res=0
  for (val in vectorFuncY) {
    res=res+ (val-mean(vectorFuncY))**2
  }
  return= (1/99)*res
}


varianzaDeX=s2x(100)
varianzaDeY=s2y(100)
varianzaDeX
varianzaDeY
#efectivamente son iguales para Yi=Xi+10
#falta demostración formal

vectorFuncY2=c(vectorFuncX*10)
s2y2=function(y){
  res=0
  for (val in vectorFuncY2) {
    res=res+ (val-mean(vectorFuncY2))**2
  }
  return= (1/99)*res
}
varianzaDeY2=s2y2(100)
varianzaDeY2
10**2*varianzaDeX
#efectivamente son iguales para Yi=Xi*10


s2x2=function(x){
  res=0
  for (val in vectorFuncX) {
    res=res+ (val)**2
  }
  return= (1/99)*res- (100/99)*((mean(vectorFuncX))**2)
}
varianzaDeX2=s2x2(100)
varianzaDeX2
varianzaDeX
#efectivamente valen lo mismo




