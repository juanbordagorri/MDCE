### EXTENSIONES DEL MODELO DE SOLOW ###

######## dos países (sin interacción) ########
s_A = 0.25
delta_A = 0.05
n_A = 0.02
alpha_A = 0.40
k_A = rep(0, 200)
k_A[1] = 1
y_A = rep(0, 200)
c_A = rep(0, 200)
#c <- delta + n
for (t in 2:200) {
  k_A[t] <- ((1-delta_A)*k_A[t-1]+s_A*y_A[t-1])/(1+n_A)
  y_A[t] <- k_A[t]^alpha_A
  c_A[t] <- (1-s_A)*y_A[t]
}

s_B = 0.4
delta_B = 0.05
n_B = 0.02
alpha_B = 0.40
k_B = rep(0, 200)
k_B[1] = 1
y_B = rep(0, 200)
c_B = rep(0, 200)

for (t in 2:200) {
  k_B[t] <- ((1-delta_B)*k_B[t-1]+s_B*y_B[t-1])/(1+n_B)
  y_B[t] <- k_B[t]^alpha_B
  c_B[t] <- (1-s_B)*y_B[t]
}
plot(c_A,type="l",col=1,ylim=c(0,2), main="Consumo")
lines(c_B,col=2)
legend("bottomright",legend=c("país A","país B"),lty=1,lwd=1,col=c(1,2),cex=0.7)

plot(k_A,type="l",col=1, ylim=c(0,18), main="k")
lines(k_B,col=2)
legend("bottomright",legend=c("país A","país B"),lty=1,lwd=1,col=c(1,2),cex=0.7)


#### con interacción (s_B > s_A) ######

# generamos 1000 individuos #
ind=matrix(0,nrow = 1000,ncol=4)
row.names(ind)=1:length(ind[,1])
colnames=c("país","colB","colC","colD")
for (i in 1:length(ind[,1])){
  if (runif(1)<0.5){ind[i,1]="1"} else{ind[i,1]="0"} 
}
sigma=0.99
s_A = 0.25
s_B = 0.3
delta_A = 0.05
delta_B = delta_A
alpha_A = 0.40
alpha_B = 0.40
H_A = rep(0, 250)
K_A = rep(0, 250)
K_A[1] = 5000
Y_A = rep(0, 250)
C_A = rep(0, 250)
H_B=H_A
K_B=K_A
Y_B=Y_A
C_B=C_A
c_A=C_A
c_B=C_A

H_A[1] =mean(as.numeric(ind[1:1000,1]))*1000
H_B[1] =1000-H_A[1]
Y_A[1]=(K_A[1]^alpha_A)*(H_A[1]^(1-alpha_A))
Y_B[1]=(K_B[1]^alpha_B)*(H_B[1]^(1-alpha_B))

fit=0      # valor inicial de (c_A - c_B)
prob_A=0.5 # probabilidad inicial
for (t in 2:250) {
  K_A[t] <-((1-delta_A)*K_A[t-1]+s_A*Y_A[t-1])
  H_A[t] =mean(as.numeric(ind[1:1000,1]))*1000
  Y_A[t]=(K_A[t]^alpha_A)*(H_A[t]^(1-alpha_A))
  C_A[t] <- (1-s_A)*Y_A[t]
  c_A[t] = C_A[t]/H_A[t]
  K_B[t] <-((1-delta_B)*K_B[t-1]+s_B*Y_B[t-1])
  H_B[t] =1000-H_A[t]
  Y_B[t]=(K_B[t]^alpha_B)*(H_B[t]^(1-alpha_B))
  C_B[t] <- (1-s_B)*Y_B[t]
  c_B[t] = C_B[t]/H_B[t]
  fit=c_A[t]-c_B[t]
  prob_A=1/(1+exp(-sigma*fit))
    for (i in 1:length(ind[,1])){
      if (runif(1)<prob_A){ind[i,1]="1"} else{ind[i,1]="0"} 
      }
}

c_starA=(1-s_A)*(s_A/(delta_A))^(alpha_A/(1-alpha_A))
c_starB=(1-s_B)*(s_B/(delta_B))^(alpha_B/(1-alpha_B))

# consumo per capita
plot(c_A[2:250],col=1,main="consumo per cápita",type="l",ylim=c(min(c_A[2:250],c_B[2:250]),max(c_A[2:250],c_B[2:250])))
lines(c_B[2:250],col=2)
legend("bottomright",legend=c("país A","país B"),lty=1,lwd=1,col=c(1,2),cex=0.7)
segments(x0=0,y0=c_starA,x1=250,y1=c_starA)
segments(x0=0,y0=c_starB,x1=250,y1=c_starB, col=2)

# cantidad de personas
plot(H_A,ylim=c(400,600),main="H=Trabajadores por país",type="l")
lines(H_B,type="l",col=2)
legend("bottomleft",legend=c("país A","país B"),lty=1,lwd=1,col=c(1,2),cex=0.7)

# capital per cápita
plot(K_A/H_A,ylim=c(10,22),main="k=K/H, capital per cápita",type="l") 
lines(K_B/H_B,col=2)

# producto per cápita
plot(Y_A/H_A,type="l",ylim=c(2.4,3.6),main = "y=Y/H, producto per cápita") # y star
lines(Y_B/H_B,col=2)

(s_A/(delta_A))^(1/(1-alpha_A)) #k star
(s_A/(delta_A))^(alpha_A/(1-alpha_A)) #y star



