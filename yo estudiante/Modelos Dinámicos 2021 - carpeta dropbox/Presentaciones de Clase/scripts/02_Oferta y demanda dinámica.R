#install.packages("rootSolve")
#install.packages('pracma')
#install.packages("R.matlab")
library('rootSolve')
library('pracma')
library('R.matlab')

############# Modelo de oferta y demanda agregada ############### 
# parámetros
a = 10       # parámetro beta
b = 5        # parámetro delta 
Qd_bar = 1000 # parámetro alpha / autonomous quantity demanded
Qs_bar = 250  # parámetro (-gamma) / autonomous quantity offered

# máximo número de periodos 
tmax = 50

# iniciamos el modelo
t = 1
P = vector("numeric", 2)
Qs = vector("numeric",2)
Qd = vector("numeric",2)

# valores inicales de los parámetros
P[t] = 25 

t = 2
Qs[t] = Qs_bar + b * P[t - 1]
P [t] = -b/a* P[t - 1]+ ( Qd_bar-Qs_bar ) / a ;
Qd[t] = Qd_bar - a * P[t]

#fprintf ( " %d \t %5.2f \t %5.2f \n " , t - 1 ,Qs[t] ,P[t] )


while ( t < tmax ) {
  t = t + 1;
  Qs[t] = Qs_bar + b*P[t-1] ;
  P [t] = -b/a*P[t-1] + ( Qd_bar-Qs_bar ) / a 
  Qd[t] = Qd_bar - a * P[t]

  fprintf ( " %d \t %5.2f \t %5.2f \n " , t-1, Qs[t] ,P[t] )
}

ymin = min (P)*.95; 
ymax = max (P)*1.05;
yy = c(ymin, ymax)

### plot 1
plot (P,type="l",xlab="t", main="Precio") 

### plot 2
ymax
Pt=seq(ymin , ymax,((ymax-ymin)/ (length(P)-1) ))
plot(Qd_bar - a*Pt,Pt, type="l",ylab="Pt",xlab="Qt",main= "Oferta, demanda y cobweb")
lines(Qs_bar + b*Pt,Pt)

for (i in 2:length(P)){
  #  lines(Qs_bar + b*P[i-1],P[i-1],type="p")
  #  lines(Qd_bar - a*P[i-1], P[i-1],type="p")
  segments(Qs[i],P[i-1],Qd_bar - a*P[i-1],P[i-1],col="red",lwd=2)
  segments(Qd_bar - a*P[i-1],P[i-1],Qs_bar + b*P[i],P[i],col="green",lwd=2)
}

#### plot 3
plot(seq(0,ymax,1),seq(0,ymax,1),xlim=c(ymin,ymax),ylim=c(ymin,ymax),type="l",xlab="P(t)",ylab="P(t+1)",main = "diagrama de fase")

for (i in 2:length(P)){
  points(P[i-1],P[i-1])
  points(P[i-1],P[i])
  segments(P[i-1],P[i-1],P[i-1],P[i],col=rainbow(i))
  segments(P[i-1],P[i],P[i],P[i],col=rainbow(i))
}

##### plot 4
plot(Qs_bar + b*P[2:length(P)],seq(2,length(P),1),type="l",xlim=c(350,750),xlab="Qs, Qd",lwd=2,col=2,ylab="t",
     main="d) Cantidad ofertada y demandada") 
lines(Qd_bar - a*P[1:length(P)-1],seq(1,length(P)-1,1),type="l",xlab="t",lwd=2,col=4) 
#lines(Qd[1:length(P)],seq(1,length(P),1),type="l",xlab="t",lwd=2,col=4) 
legend ("topright", c ("Qs", "Qd"),col=c(2,4), lwd =c(2,2), cex = 2)

#### ahora, los 4 grficos en forma simultánea ####

op <- par(mfrow = c(2, 2))
### plot 1
plot (P,type="l",xlab="t",main="a) Precio") 
### plot 2
ymax
Pt=seq(ymin , ymax,((ymax-ymin)/ (length(P)-1) ))
plot(Qd_bar - a*Pt,Pt, type="l",ylab="Pt",xlab="Qt",main= "b) Oferta, demanda y cobweb") #,ylim=c(20,70),xlim=c(350,775))
lines(Qs_bar + b*Pt,Pt)

for (i in 2:length(P)){
  lines(Qs_bar + b*P[i-1],P[i-1],type="p",pch=17)
  lines(Qd_bar - a*P[i], P[i],type="p")
  segments(Qs[i],P[i-1],Qd_bar - a*P[i-1],P[i-1],col="red",lwd=2)
  segments(Qd_bar - a*P[i-1],P[i-1],Qs_bar + b*P[i],P[i],col="green",lwd=2)
}
#### plot 3
plot(seq(0,ymax,1),seq(0,ymax,1),xlim=c(ymin,ymax),ylim=c(ymin,ymax),type="l",xlab="P(t)",ylab="P(t+1)",
     main="c) Diagrama de fase")
#phase diagram ( price )
for (i in 2:length(P)){
  points(P[i-1],P[i-1])
  points(P[i-1],P[i])
  segments(P[i-1],P[i-1],P[i-1],P[i],col=rainbow(i))
  segments(P[i-1],P[i],P[i],P[i],col=rainbow(i))
}

##### plot 4
plot(Qs_bar + b*P[2:length(P)],seq(2,length(P),1),type="l",xlim=c(350,750),xlab="Qs, Qd",lwd=2,col=2,ylab="t",
     main="d) Cantidad ofertada y demandada") 
lines(Qd_bar - a*P[1:length(P)-1],seq(1,length(P)-1,1),type="l",xlab="t",lwd=2,col=4) 
#legend ("topright", c ("Qs", "Qd"),col=c(2,4), lwd =c(2,2), cex = 1.5)

dev.off()





