### MODELO DE SOLOW ###

######## en tiempo discreto (recursi?n) ########
s = 0.25
delta = 0.05
n = 0.02
alpha = 0.40
k = rep(0, 200)
k[1] = 1
y = rep(0, 200)
#c <- delta + n
for (t in 2:200) {
  k[t]<- ((1-delta)*k[t-1]+s*y[t-1])/(1+n)
  y[t] <- k[t]^alpha
    }
###### gr?ficos #######

ka=seq(0,15,0.1)
plot(ka,ka^alpha,type="l",xlab="k",ylab=" ",lwd=2,lty=3,col="blue",main="Modelo de Solow")
lines(ka,s*ka^alpha,col="red",lwd=2)
lines(ka,(delta+n)*ka,col="green",lwd=2)
segments(k[200],0,k[200],k[200]*(delta+n),lty=2,lwd=2)
segments(k[200],k[200]*(delta+n),k[200],k[200]^alpha,lty=3,lwd=2)
text(x=c(k[200]*1.65,k[200]*1.65,k[200]*1.65,k[200],k[200]),
     y=c(0.95*((k[200]*1.6)^alpha),0.95*(s*(k[200]*1.6)^alpha),
         1.05*((delta+n)*(k[200]*1.6)),(s*s*k[200])/2,
         s*s*k[200]+(k[200]^alpha-s*s*k[200])/2),
#     labels=c("k^alpha","s*k^alpha","(delta+n)*k","inversi?n","consumo"),cex=1.5)
      labels=c("f(k)","s*f(k)","(delta+n)*k","inversi?n","consumo"),cex=1.5)

k_star=(s/(delta+n))^(1/(1-alpha))
y_star=(s/(delta+n))^(alpha/(1-alpha))

# evoluci?n de k 
plot(k,type="l",main="Evoluci?n de k",xlab="t")
segments(0,k_star,200,k_star,col="red",lty=3)

# evoluci?n de y
plot(y,type="l",main="Evoluci?n de y",xlab="t")
segments(0,y_star,200,y_star,col="red",lty=3)

# evoluci?n de la acumulaci?n de capital
plot(s*k^alpha-k*(delta+n),ylab="s*f(k)-k*(delta+n)",
     main="Acumulaci?n de capital per c?pita",xlab="t")

######## en tiempo continuo (ODE's) ########
library(deSolve)
s = 0.25
delta = 0.05
n = 0.02
alpha = 0.40
y_ini=c(k=1)
derivs <- function(t, k, parms)
  list( s*k^alpha-k*(delta+n))
times <- seq(from = 0, to = 200, by = 1)
out <- ode(y = y_ini, times = times, func = derivs,
           parms = NULL)
#out
plot(out[,1],out[,2],type="l",xlab="t",ylab="k",main="Evoluci?n de k")

