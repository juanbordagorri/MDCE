library('igraph')
library('dplyr')
library('magrittr')
library('plot3D')

### Experimento 1: vemos la cantidad de insatisfechos,
#     para una densidad del 97% y un umbral del 40%.

#     Cargamos el archivo:
segr=read.csv("Segregation2 experiment1-table.csv",sep=",", 
                    stringsAsFactors = FALSE, skip=6, header = T)
colnames(segr)=c("run","density","similar","similar_max","visualization","step","insatisfechos")

#     graficamos en escala logar?tmica
plot(segr$step,segr$insatisfechos,log="xy")
#     analizamos si la cantidad de individuos insatisfechos decae siguiendo una ley potencial
BB=fit_power_law(segr$insatisfechos)
BB$alpha  # valor del par?metro alpha
BB$xmin   # valor m?nimo 
BB$KS.p   # p-valor de la prueba K-S, con H0) power law (p-valor > 0.05)  
segments(1,BB$xmin,50,BB$xmin, col="red")  #valor a partir del cual se cumple la ley potencial


### Experimento 2: vemos la cantidad de insatisfechos y vecinos similares
#     para una densidad entre 80% y 99% y un umbral entre 10% y 60%.

#     Cargamos el archivo:
segr2=read.csv("Segregation2 experiment2-table.csv",sep=",", 
              stringsAsFactors = FALSE, skip=6, header = T)
colnames(segr2)=c("run","density","similar","step","insatisfechos","percent_sim","percent_unh")
result=matrix(0,ncol=20,nrow=51)
summary(segr2)
resultado=0

# observamos cu?ntos pasos demora en converger
for (i in 1:51){
  for (j in 1:20){
    res <- segr2 %>%
      filter(.,density == ( (j-1) + 80 ) & similar == 10 + (i-1) )   %>%    summarise(median(step))
      result[i,j]  = res$`median(step)`
  }
}
image2D(result,seq(10,60,1),seq(80,99,1),xlab="similar",ylab="densidad",main="Experimento 2: 
        pasos para la convergencia",rasterImage = T)


# ahora, observamos si t es menor o igual a 1000
result2=result
for (i in 1:51){
  for (j in 1:20){
    result2[i,j]  = if_else (result[i,j] < 1000,0,1)
  }
}
image2D(result2,seq(10,60,1),seq(80,99,1),xlab="similar",ylab="densidad",main="Experimento 2: convergencia?",rasterImage = T)


# vemos c?mo cambia el porcentaje de vecinos similares
for (i in 1:51){
  for (j in 1:20){
    res <- segr2 %>%
      filter(.,density == ( (j-1) + 80 ) & similar == 10 + (i-1) )   %>%    summarise(median(percent_sim))
    result[i,j]  = res$`median(percent_sim)`
  }
}
image2D(result,seq(10,60,1),seq(80,99,1),xlab="similar",ylab="densidad",main="Experimento 2:
        porcentaje de vecinos similares",rasterImage = T)


### Experimento 3: vemos la cantidad de insatisfechos y vecinos similares
#     para una densidad del 85%, un umbral m?nimo entre 0% y 30% 
#     y un umbral m?ximo entre 60% y 90%.

#     Cargamos el archivo:
segr3=read.csv("Segregation2 experiment3-table.csv",sep=",", 
               stringsAsFactors = FALSE, skip=6, header = T)
colnames(segr3)=c("run","density","similar_min","similar_max","step","insatisfechos","percent_sim","percent_unh")
result3=matrix(0,ncol=31,nrow=31)
summary(segr3)

# vemos c?mo cambia el porcentaje de vecinos similares
for (i in 1:31){
  for (j in 1:31){
    res <- segr3 %>%
      filter(.,similar_min == (i-1) & similar_max == 60 + (j-1) )   %>%    summarise(median(percent_sim))
    result3[i,j]  = res$`median(percent_sim)`
  }
}
image2D(result3,seq(0,30,1),seq(60,90,1),xlab="min_similar",ylab="max_similar",main="Experimento 2:
        porcentaje de vecinos similares",rasterImage = T)

# vemos c?mo cambia la cantidad de individuos insatisfechos (pasos para la convergencia)
for (i in 1:31){
  for (j in 1:31){
    res <- segr3 %>%
      filter(.,similar_min == (i-1) & similar_max == 60 + (j-1) )   %>%    summarise(median(insatisfechos))
    result3[i,j]  = res$`median(insatisfechos)`
  }
}
image2D(result3,seq(0,30,1),seq(60,90,1),xlab="min_similar",ylab="max_similar",main="Experimento 2: 
          pasos para la convergencia",rasterImage = T)

