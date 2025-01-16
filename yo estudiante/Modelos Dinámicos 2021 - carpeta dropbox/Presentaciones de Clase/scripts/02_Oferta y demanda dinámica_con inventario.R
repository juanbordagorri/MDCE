################# Modelo con Inventario #####################
# par?metros

a = 10       # parámetro beta
b = 5        # parámetro delta 
Qd_bar = 1000 # parámetro alpha / autonomous quantity demanded
Qs_bar = 250  # parámetro (-gamma) / autonomous quantity offered
sigma = 0.05; # ajuste de precios por variación de inventarios

# maximum nb of periods and tolerance
tmax = 50 
tol = 0.00001 


t = 1
P = vector("numeric",2)
Qs = vector("numeric",2)
Qd = vector("numeric",2)

P[t] = 25 # initial price
Qs[t] = NaN # no value for Q
Qd[t] = NaN

t = 2
Qs[t] = Qs_bar + b * P[t]
P [t] = ( 1 - sigma *( a + b )) * P[t - 1]+ ( Qd_bar-Qs_bar ) * sigma 
Qd[t] = Qd_bar - a * P[t ]

while ( t < tmax & abs ( P[t] - P[t-1] ) > tol * P [t]) {
  t = t + 1;
  P [t] = ( 1 - sigma * ( a + b ) )*P[t - 1 ]+ sigma * ( Qd_bar-Qs_bar )
  Qs[t] = Qs_bar+b*P[t ] 
  Qd[t] = Qd_bar - a * P[t]
}
pmin=min(P[1:length(P)])
pmax=max(P[1:length(P)])
plot(seq(0,pmax,1),seq(0,pmax,1),xlim=c(pmin,pmax),ylim=c(pmin,pmax),type="l",xlab="P(t)",ylab="P(t+1)",main="Diagrama de fase")
#phase diagram ( price )
for (i in 2:length(P)){
  points(P[i-1],P[i-1])
  points(P[i-1],P[i])
  segments(P[i-1],P[i-1],P[i-1],P[i],col=rainbow(i))
  segments(P[i-1],P[i],P[i],P[i],col=rainbow(i))
}

ymin = min (P)*.95; 
ymax = max (P)*1.05;
yy = c(ymin, ymax)

### plot 1
plot (P,type="l",xlab="t") 

### plot 2
ymax
Pt=seq(ymin , ymax,((ymax-ymin)/ (length(P)-1) ))
plot(Qd_bar - a*Pt,Pt, type="l",ylab="Pt",xlab="Qt",xlim=c(200,800))
lines(Qs_bar + b*Pt,Pt)

for (i in 2:length(P)){
  lines(Qs_bar + b*P[i-1],P[i-1],type="p")
  lines(Qd_bar - a*P[i-1], P[i-1],type="p")
  segments(Qs_bar + b*P[i-1],P[i-1],Qd_bar - a*P[i-1],P[i-1],col="red",lwd=2)
  segments(Qd_bar - a*P[i-1],P[i-1],Qs_bar + b*P[i],P[i],col="green",lwd=2)
}

#### plot 3
plot(seq(0,ymax,1),seq(0,ymax,1),xlim=c(ymin,ymax),ylim=c(ymin,ymax),type="l",xlab="P(t)",ylab="P(t+1)")
#phase diagram ( price )
for (i in 2:length(P)){
  points(P[i-1],P[i-1])
  points(P[i-1],P[i])
  segments(P[i-1],P[i-1],P[i-1],P[i],col=rainbow(i))
  segments(P[i-1],P[i],P[i],P[i],col=rainbow(i))
}

##### plot 4
plot(Qs[1:length(P)],seq(1,length(P),1),type="l",xlab="t",lwd=2,col=2,ylab="Qs,Qd")#,xlim=c(350,750)) 
lines(Qd[1:length(P)],seq(1,length(P),1),type="l",xlab="t",lwd=2,col=4) 

## ahora, los 4 gr?ficos en forma simult?nea

op <- par(mfrow = c(2, 2))
### plot 1
plot (P,type="l",xlab="t",main="a) Precio") 
### plot 2
ymax
Pt=seq(ymin , ymax,((ymax-ymin)/ (length(P)-1) ))
plot(Qd_bar - a*Pt,Pt, type="l",ylab="Pt",xlab="Qt",main= "b) Oferta, demanda y cobweb",xlim=c(Qs[2],800)) #,ylim=c(20,70))
lines(Qs_bar + b*Pt,Pt)

for (i in 2:length(P)){
  lines(Qs_bar + b*P[i-1],P[i-1],type="p")
  lines(Qd_bar - a*P[i-1], P[i-1],type="p")
  segments(Qs_bar + b*P[i-1],P[i-1],Qd_bar - a*P[i-1],P[i-1],col="red",lwd=2)
  segments(Qd_bar - a*P[i-1],P[i-1],Qs_bar + b*P[i],P[i],col="green",lwd=2)
}
#### plot 3
plot(seq(0,ymax,1),seq(0,ymax,1),xlim=c(ymin,ymax),ylim=c(ymin,ymax),type="l",xlab="P(t)",ylab="P(t+1)",main="c) Diagrama de fase")
#phase diagram ( price )
for (i in 2:length(P)){
  points(P[i-1],P[i-1])
  points(P[i-1],P[i])
  segments(P[i-1],P[i-1],P[i-1],P[i],col=rainbow(i))
  segments(P[i-1],P[i],P[i],P[i],col=rainbow(i))
}

##### plot 4
plot(Qs[1:length(P)],seq(1,length(P),1),type="l",xlim=c(Qs[2],Qd[2]),xlab="t",lwd=2,col=2,ylab="Qs,Qd",main="d) Cantidad ofertada y demandada") 
lines(Qd[1:length(P)],seq(1,length(P),1),type="l",xlab="t",lwd=2,col=4) 
#legend ("topright", c ("Qs", "Qd"),col=c(2,4), lwd =c(2,2), cex = 1.5)

#dev.off()


