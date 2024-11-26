##### Ejemplos de loops #####

## ejemplo de loop for
x=1
for ( i in 1:10 ) { x = x + 1 }

## ejemplo de loop while
i=1
x=1
a=2
b=10
while ( i <= b & x<=200 )  { x = x*a
i= i + 1	}
while ( i <= b)  { x = x*a
i= i + 1	}

## ejemplo de loop if
if ( x <= 1500  )  { x = x*a } else { x = x*a/3	}


##### Ecuaci�n log�stica ##### 

a=3.8  ### �nico par�metro
q=seq(0,1,0.001)
logist=q
for (i in 1:length(q)) {
  logist[i]=a*q[i]*(1-q[i])  
}

result=vector("numeric",500)
result[1]=0.3                                    # valor inicial
for (i in 2:length(result)){
  result[i]=a*result[i-1]*(1-result[i-1])
}

plot(result,type="l") # ver una l?nea

plot(q,logist, type="l", xlab="y(t)", ylab = "y(t+1)",lwd=2)  # ecuación logística
lines(q,q,lwd=2)                # t = t + 1 
segments(result[1],0,result[1],result[2],col=3)
segments(result[1],result[2],result[2],result[2],col=3)
segments(result[2],result[2],result[2],result[3],col=3)
segments(result[2],result[3],result[3],result[3],col=3)

plot(q,logist, type="l", xlab="y(t)", ylab = "y(t+1)",lwd=2)  # ecuación logística
lines(q,q,lwd=2)                # t = t + 1 
for (i in 200:length(result)){                                # cambiar por: " for (i in 200:length(result)){ "
  points(result[i-1],result[i-1])                   
  points(result[i-1],result[i],lwd=2)                # puntos por donde pasa     
  segments(result[i-1],result[i-1],result[i-1],result[i],col=2)    # líneas
  segments(result[i-1],result[i],result[i],result[i],col=2)
}

#### probar: a = 2, a = 3, a = 3.4, a = 3.84, a=3.9, a=3.99998, a=3.99999


#### diagrama de bifurcación ####
n <- 1
R <- seq(2.5,4,length=2000)     # valores del parámetro de interés (renombramos "a" como "R")
f <- expression(a*x*(1-x))      # función a analizar
data <- matrix(0,200,2001)      # matriz de ceros, con nrow=200 y ncol=2001

for(a in R){
  x <- runif(1)                 # condición inicial
  ## convergencia al atractor (aplico la logística 200 veces para llegar al atractor)
  for(i in 1:200){
    x <- eval(f)                # le aplico la logística       
  } 
  # guardamos los puntos (simulamos 200 períodos más)
  for(i in 1:200){
    x <- eval(f)
    data[i,n] <- x              # guardo en cada momento n
  }
  n <- n+1                      # paso al período siguiente  
}

data <- data[,1:2000]           # elimino el valor inicial
plot(R,data[1,], pch=".", xlab="a", ylab="X",ylim=c(0,1),main="Diagrama de bifurcación")
segments(3,0,3,1)
segments(3.45,0,3.449,1)
segments(3.84,0,3.84,1)
text(x=c(3,3.449,3.84),y=c(0.01,0.01,0.01),labels = c("a=3","a=3.45","a=3.84"),col="blue",cex = 1.3)
for(i in 100:200) points(R,data[i,],pch=".")
