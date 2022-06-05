####ejercicio 1####
carbohidratos=c(76.93, 76.88, 77.07, 76.68, 76.39, 75.09, 77.67, 76.88, 78.15, 76.50, 77.16, 76.42)

esperanza=mean(carbohidratos)
esperanza

mediana=median(carbohidratos)
mediana
proba=sum(carbohidratos<76.5)/length(carbohidratos)
proba

####ejercicio 3####

#a
data=c(210, 197, 187, 217, 194, 208, 220,199, 193, 203, 181, 212, 188, 196, 185)
n=length(data)
desvio=((sd(data)**2)*(n-1))/n
desvio

#b

mediciones=c(25.11, 25.02, 25.16, 24.98, 24.83, 25.05, 24.94, 25.04, 24.99, 24.96, 25.03, 24.97, 24.93, 25.12, 25.01, 25.12, 24.90, 24.98, 25.10, 24.96)
#X es normal(mu, 0,001)
mu=mean(mediciones)
mu
