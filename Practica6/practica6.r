#PRACTICA 6

#ejercicio 1
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


#ejercicio 2
lamparas=read.table(file="lamparas.txt", header = FALSE)
cant_lamparas=26

#a

proba_mas30=sum(lamparas>30)/cant_lamparas
proba_mas30

#b


#c
quantile(lamparas$V1,0.1)


#ejercicio 3

graduados=read.table(file="graduados.txt", header = FALSE)
#a
mean(graduados$V1)
median(graduados$V1)

#b
sd(graduados$V1)
distanciaIntercuantil = quantile(graduados$V1,0.75)-quantile(graduados$V1,0.25)
distanciaIntercuantil

#c
hist(graduados$V1)

#d
boxplot(graduados$V1)












