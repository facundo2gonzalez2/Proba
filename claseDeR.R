#practica 0 ejercicio 2 a y b:
tratamiento<-c(rep("A",20),rep("B",18),rep("C",22))
j<-seq(5,100,by=5)
j[1]+j[8]
mean(j)

#practica 2 ejercicio 18c
n=5
p=1/3


fpp_0=dbinom(0,n,p)

realizaciones=rbinom(1000000,prob=p,size=n)
proba0=sum(realizaciones==0)/1000000


#ejemplo de funciónes
calculo_largo <- function(x,y){
  resultado=x*y+2*y+2
  return(resultado)  #resultado que espero
}

calculo_largo2 <- function(x,y){
  if (x>y){
    result = x+y
    return(result)
  } else{
    result=x*y+2*y+2
    return(result)
  }
}


#practica 3 ejercicio 20

espVar = function(f,s){
  faux<-function(s){
    return(x*f(x))
  }
  
  esperanza<- integrate(faux,min(s),max(s))&value
  faux2<-function(s){
    return( x**2 * f(x))
  }
  varianza<- integrate(faux2,min(s),max(s))&value - esperanza**2
  
  return(c(paste(c("Esperanza", esperanza))),c(paste(c("Varianza", varianza))))
}

a<-function(x){0.75*(1-x**2)}         #no funciona pero es algo asi
s<-c(-1,1)