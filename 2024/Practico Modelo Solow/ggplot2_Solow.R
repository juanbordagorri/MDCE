### MODELO DE SOLOW ###

######## en tiempo discreto ########

if (!require("data.table")) install.packages("data.table",dependencies = T)
library(ggplot2)

solow <- function(s = 0.25, # tasa de ahorro
                  delta = 0.05, # depreciación de K
                  n = 0.02, # tasa de crecimiento de la población
                  alpha = 0.4, # elasticidad producto del capital
                  cond_inicial = 1,
                  gold = F, # muestra el k de la regla de oro
                  iteraciones = 200){ 
  
  data <- data.table(
    k = rep(0,iteraciones),
    y = rep(0,iteraciones),
    ka = seq(0,100,length.out = iteraciones)
  )
  data[1, k := cond_inicial]
  
  
  for (t in 2:iteraciones) {
    data$k[t]<- ((1-delta)*data$k[t-1]+s*data$y[t-1])/(1+n)
    data$y[t] <- data$k[t]^alpha 
  }
  
  k_star = (s/(delta+n))^(1/(1-alpha))
  y_star = (s/(delta+n))^(alpha/(1-alpha))
  k_gold = (alpha/(delta+n))^(1/(1-alpha))
  
  ref_x = max(k_star,k_gold)
  
  # curva y, s*y, delta+n
  g1 <- data[,
       ggplot(.SD, aes(ka, ka**alpha))+
         geom_line(col = "blue")+
         geom_line(aes(y=s*ka^alpha), col = "red")+
         geom_abline(slope = delta+n, col = "green")+
         
         geom_segment(aes(x = k[iteraciones],
                          y = 0, 
                          yend = k[iteraciones]*(delta+n)), linetype = 2)+ # ahorro de equilibrio
         
         annotate("text", x = k_star,
                  y = 0.5*k[iteraciones]*(delta+n),
                  label = "inversión", 
                  hjust = 1.2)+
         
         geom_segment(aes(x = k[iteraciones],
                          yend = k[iteraciones]**alpha, 
                          y = k[iteraciones]*(delta+n)), linetype = 3)+ # consumo de eq
         
         annotate("text", x = k_star,
                  y = 0.5*(1+s)*k[iteraciones]**alpha,
                  label = "consumo", 
                  hjust = 1.1)+
         
         annotate("text", 
                  x = ref_x*1.1,
                  y = ref_x**alpha,
                  label = "f(k)",
                  vjust = 1.1, col = "blue")+ # f(k)
         annotate("text", 
                  x = ref_x*1.1,
                  y = (ref_x**alpha)*s,
                  label = "s.f(k)",
                  vjust = 1.2, col = "red")+ # s.f(k)
         
         annotate("text", 
                  x = ref_x*1.1,
                  y = ref_x*(delta+n),
                  label = expression(paste("(",delta,"+n)k")),
                  vjust = -0.5, col = "green")+ # (delta+n)k
         
         annotate("text", 
                  x = k_star,
                  y = 0,
                  label = "k*",
                  vjust = 1.3)+
         
         
         labs(title = "Modelo de Solow", 
              x=expression(paste(k[t])), y = expression(paste(f(k[t]))))+
         xlim(c(0, ref_x*1.8))+
         ylim(c(0, 1.3*ref_x**alpha))+
         theme_minimal()]
  
  if(gold){
    g1 <- g1 + geom_segment(aes(x = k_gold,
                                yend = k_gold**alpha, 
                                y = 0), 
                            linetype = 1, col = "gold", linewidth = 1) +
      annotate("text", 
               x = k_gold,
               y = k_gold**alpha,
               label = expression(paste(k^{gold})),
               vjust = -0.5, col = "gold")
  }
  
  # grafico evolucion k
  g2 <- data[,
       ggplot(.SD,aes(x = ka, y = k))+
         geom_line()+
         labs(title = "Evolución de k", x = "t", y = "k")+
         geom_hline(yintercept = k_star, linetype = 9, col = "red", lwd =1)+
         theme_minimal()]
  
    # evolucion de y
  g3 <- data[,
       ggplot(.SD,aes(x = ka, y = y))+
         geom_line()+
         labs(title = "Evolución de y", x = "t", y = "y")+
         geom_hline(yintercept = y_star, linetype = 9, col = "red", lwd =1)+
         theme_minimal()]
  
  # evolucion acumulacion de capital per capita
  g4 <- data[,
       ggplot(.SD,aes(x = ka, y = s*k^alpha-k*(delta+n)))+
         geom_line()+
         labs(title = "Acumulación de capital per cápita", x = "t", 
              y = expression(paste(s.f(k)-k.(delta+n))))+
         theme_minimal()]
  
  return(list(
    eqs = data.frame(k_star = k_star, 
                     c_star = (1-s)*y_star, 
                     y_star=y_star,
                     k_gold=k_gold, 
                     c_gold = (1-s)*k_gold**alpha,
                     y_gold = k_gold**alpha),
    modelo = g1,
    ev_k = g2,
    ev_y = g3,
    ev_ack = g4))
}

mod1 <- solow(s = 0.25, # tasa de ahorro
              delta = 0.05, # depreciación de K
              n = 0.02, # tasa de crecimiento de la población
              alpha = 0.4, # elasticidad producto del capital
              cond_inicial = 15,
              iteraciones = 200, 
              gold = T)

# El objeto mod1 es una lista que contiene varios elementos. Tiene un vector de valores que corresponden a valores del capital y producto de interés, a saber; el capital del estado estacionario (k_star), el producto del estado estacionario (y_star) y el capital de la regla de oro (k_gold).
# Además tiene gráficos que muestran la dinámica de las variables de interés.

View(mod1)
mod1[["modelo"]]
mod1[["eqs"]] # vector de valores
mod1[["ev_k"]]
mod1[["ev_y"]]
mod1[["ev_ack"]]



######## en tiempo continuo (ODE's) ########
if (!require("deSolve")) install.packages("deSolve",dependencies = T)
s = 0.25
delta = 0.05
n = 0.02
alpha = 0.40
y_ini=c(k=1)
derivs <- function(t, k, parms)
  list( s*k^alpha-k*(delta+n))
times <- seq(from = 0, to = 200, by = 1)
out <- ode(y = y_ini, times = times, func = derivs,
           parms = NULL) |> as.data.table()

out[,
    ggplot(.SD, aes(time, k))+
      geom_line()+
      labs(title = "Evolución de k", x = "t", y = "k")+
      theme_minimal()]


