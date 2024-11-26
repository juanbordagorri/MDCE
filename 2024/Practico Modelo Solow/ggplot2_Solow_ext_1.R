### MODELO DE SOLOW ###

if (!require("data.table")) install.packages("data.table",dependencies = T)
library(ggplot2)

solow_ex1 <- function(s = 0.25, # tasa de ahorro
                  delta = 0.05, # depreciacion de K
                  n_inicial = 0.02, # tasa de crecimiento inicial
                  L_inicial = 100, # poblacion inicial
                  alpha = 0.4, # elasticidad producto del capital
                  k_inicial = 1,
                  # parametros de ec. de Verhulst
                  r = 0.03,
                  K = 300, 
                  gold = F,
                  iteraciones = 500){
  
  data <- data.table(
    k = rep(0,iteraciones),
    y = rep(0,iteraciones),
    n = rep(0, iteraciones),
    L = rep(0, iteraciones),
    ka = seq(0,100,length.out = iteraciones)
  )

  data[1, `:=`(k = k_inicial,
               n = n_inicial,
               L = L_inicial)]
  
  for (t in 2:iteraciones) {
    data$L[t] <- data$L[t-1]*exp(r*(1-(data$L[t-1])/K))  #Verhulst
    data$n[t] <- (data$L[t]/data$L[t-1])-1
    data$k[t]<- ((1-delta)*data$k[t-1]+s*data$y[t-1])/(1+data$n[t])
    data$y[t] <- data$k[t]^alpha 
    }
  
  k_star = (s/(delta+data$n[iteraciones]))^(1/(1-alpha))
  y_star = (s/(delta+data$n[iteraciones]))^(alpha/(1-alpha))
  k_gold = (alpha/(delta+data$n[iteraciones]))^(1/(1-alpha))
  
  ref_x = max(k_star,k_gold)
  
  # curva y, s*y, delta+n
  g1 <- data[,
             ggplot(.SD, aes(ka, ka**alpha))+
               geom_line(col = "blue")+
               geom_line(aes(y=s*ka^alpha), col = "red")+
               geom_abline(slope = delta+n[iteraciones], col = "green")+
               
               geom_segment(aes(x = k[iteraciones],
                                y = 0, 
                                yend = k[iteraciones]*(delta+n[iteraciones])), linetype = 2)+ # ahorro de equilibrio
               
               annotate("text", x = k_star,
                        y = 0.5*k[iteraciones]*(delta+n[iteraciones]),
                        label = "inversión", 
                        hjust = 1.2)+
               
               geom_segment(aes(x = k[iteraciones],
                                yend = k[iteraciones]**alpha, 
                                y = k[iteraciones]*(delta+n[iteraciones])), linetype = 3)+ # consumo de eq
               
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
                        y = ref_x*(delta+n[iteraciones]),
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

  # grafico evolucion n
  g5 <- data[,
             ggplot(.SD,aes(x = ka, y = n))+
               geom_line()+
               labs(title = "Evolución de n", x = "t", y = "n")+
               #geom_hline(yintercept = n[iter], linetype = 9, col = "red", lwd =1)+
               #ylim(c(0, k_star*1.1))+
               theme_minimal()]
  
  # grafico evolucion L
  g6 <- data[,
             ggplot(.SD,aes(x = ka, y = L))+
               geom_line()+
               labs(title = "Evolución de L", x = "t", y = "L")+
               #geom_hline(yintercept = n[iter], linetype = 9, col = "red", lwd =1)+
               #ylim(c(0, k_star*1.1))+
               theme_minimal()]
  
  # grafico evolucion k
  g2 <- data[,
             ggplot(.SD,aes(x = ka, y = k))+
               geom_line()+
               labs(title = "Evolución de k", x = "t", y = "k")+
               geom_hline(yintercept = k_star, linetype = 9, col = "red", lwd =1)+
               ylim(c(0, k_star*1.1))+
               theme_minimal()]
  
  # evolucion de y
  g3 <- data[,
             ggplot(.SD,aes(x = ka, y = y))+
               geom_line()+
               labs(title = "Evolución de y", x = "t", y = "y")+
               geom_hline(yintercept = y_star, linetype = 9, col = "red", lwd =1)+
               ylim(c(0, y_star*1.1))+
               theme_minimal()]
  
  # evolucion acumulacion de capital per capita
  g4 <- data[,
             ggplot(.SD,aes(x = ka, y = s*k^alpha-k*(delta+n[iteraciones])))+
               geom_line()+
               labs(title = "Acumulación de capital per cápita", x = "t", 
                    y = expression(paste(s*f(k)-k*(delta+n))))+
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
    ev_ack = g4,
    ev_n = g5,
    ev_L = g6))
}

mod2 <- solow_ex1(s = 0.4, # tasa de ahorro
                  delta = 0.05, # depreciación de K
                  n_inicial = 0.02, # tasa de crecimiento inicial
                  L_inicial = 100, # población inicial
                  alpha = 0.4, # elasticidad producto del capital
                  k_inicial = 1,# parámetros de ec. de Verhulst
                  r = 0.03,
                  K = 300, # B
                  gold = T, # muestra el k de la regla de oro
                  iteraciones = 500)

View(mod2)
mod2[["modelo"]]
mod2[["eqs"]]
mod2[["ev_k"]]
mod2[["ev_y"]]
mod2[["ev_ack"]]
mod2[["ev_n"]]
mod2[["ev_L"]]
