### MODELO DE SOLOW ###

######## en tiempo discreto (recursi�n) ########

### Extensi�n (1): tasa de crecimiento decreciente
s = 0.25
delta = 0.05
alpha = 0.40
times=500
k = rep(0, times)
k[1] = 1
y = rep(0, times)
n = rep(0, times)
L = rep(0, times)
n[1] = 0.02
L[1] = 100

#par�metros de ec. de Verhulst
r=0.03
K=300

for (t in 2:times) {
  L[t] <- L[t-1]*exp(r*(1-(L[t-1])/K))  #Verhulst
  n[t] <- (L[t]/L[t-1])-1
  k[t]<- ((1-delta)*k[t-1]+s*y[t-1])/(1+n[t])
  y[t] <- k[t]^alpha
    }
plot(n,main="tasa de crecimiento de la poblaci�n")
plot(L,main="poblaci�n")
###### gr�ficos #######

ka=seq(0,15,0.1)
plot(ka,ka^alpha,type="l",xlab="k",ylab=" ",lwd=2,lty=3,col="blue",main="Modelo de Solow")
lines(ka,s*ka^alpha,col="red",lwd=2)
lines(ka,(delta+n[t])*ka,col="green",lwd=2)
segments(k[times],0,k[times],k[times]*(delta+n[t]),lty=2,lwd=2)

segments(k[times],k[times]*(delta+n[times]),k[times],k[times]^alpha,lty=3,lwd=2)

k_star=(s/(delta+n[times]))^(1/(1-alpha))
y_star=(s/(delta+n[times]))^(alpha/(1-alpha))

# evoluci�n de k 
plot(k,type="l",main="Evoluci�n de k",xlab="t")
segments(0,k_star,times,k_star,col="red",lty=3)

# evoluci�n de y
plot(y,type="l",main="Evoluci�n de y",xlab="t")
segments(0,y_star,times,y_star,col="red",lty=3)

# evoluci�n de la acumulaci�n de capital
plot(s*k^alpha-k*(delta+n[times]),ylab="s*f(k)-k*(delta+n)",
     main="Acumulaci�n de capital per c�pita",xlab="t")

