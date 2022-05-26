# Juego: Tenemos una urna con 3 bolitas verdes, 3 rojas y 5 negras. 
# Sacamos dos bolitas SIN reposición. Por cada verde que sale ganamos $10, pero
# por cada roja perdemos $10. Con las negras, no ganamos ni perdemos.

#comencemos simulando una realización del experimento
set.seed(27)
urna<-c("V","V","V","R","R","R",rep("N",5))
exp<-sample(urna,2,replace=FALSE)
# help("sample") para pedir ayuda si no me acuerdo como era

# puedo pensarlo en terminos de la ganancia

# queremos entender la probabilidad de perder 10$
sum(exp=="R") #es contar cuantas rojas tengo

#pierdo 20 si
pierdo_20<-c()
Nrep<-1000
prob_estimada<-c()
for(j in 1:Nrep)
{
  for (i in 1:j)
  {
    exp<-sample(urna,2,replace=FALSE)
    pierdo_20[i]<-ifelse(sum(exp=="R")==2,1,0)
  }
  prob_estimada[j]<-sum(pierdo_20)/length(pierdo_20)
}

plot(1:Nrep,prob_estimada, pch=20, 
     xlab = "cant de repeticiones", col="deepskyblue", 
     ylim=c(0,1))

# para observar como converge la estimacion mediante la frecuencia relativa

#probamos con una
menos20<-ifelse(sum(exp=="R")==2,1,0)
menos10<-ifelse(sum(exp=="R")==1 & sum(exp=="N")==1,1,0)
cero<-ifelse(sum(exp=="N")==2 | (sum(exp=="R")==1 & sum(exp=="V")==1) ,1,0)
mas10<-ifelse(sum(exp=="V")==1 & sum(exp=="N")==1,1,0)
mas20<-ifelse(sum(exp=="V")==2,1,0)

#ahora muchos

resultados<-matrix(NA,ncol=5,nrow=Nrep)
for(i in 1:Nrep)
{
  exp<-sample(urna,2,replace=FALSE)
  resultados[i,1]<-ifelse(sum(exp=="R")==2,1,0)
  resultados[i,2]<-ifelse(sum(exp=="R")==1 & sum(exp=="N")==1,1,0)
  resultados[i,3]<-ifelse(sum(exp=="N")==2 | (sum(exp=="R")==1 & sum(exp=="V")==1) ,1,0)
  resultados[i,4]<-ifelse(sum(exp=="V")==1 & sum(exp=="N")==1,1,0)
  resultados[i,5]<-ifelse(sum(exp=="V")==2,1,0)
}

#lo mismo que frec rel es
mean(resultados[,1])

#una funcion util
probas_estimadas<-apply(resultados, 2, mean)
