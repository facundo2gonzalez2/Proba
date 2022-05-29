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
