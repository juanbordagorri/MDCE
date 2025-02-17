library('dplyr')
library('magrittr')
library('plot3D')

### Experimento 1 
# cargo el archivo
fire_model=read.csv("Fire Simple experimento 1-table.csv",sep=",", 
                    stringsAsFactors = FALSE, skip=6, header = T)
colnames(fire_model)=c("run","density","step","burned") # cambio el nombre de las columnas

res <- fire_model %>%
  aggregate(. ~ density, data = ., FUN = function(x) round(mean(x), 2) )
# gr�fico con el porcentaje quemado en relaci�n a la densidad 
plot(res$density,res$burned,xlab="densidad",ylab="% quemado", main="Experimento 1",type="b")

## pregunta: �en d�nde se encuentra el tipping point?


### Experimento 2
fire_2=read.csv("Fire Simple experimento 2-table.csv",sep=",", stringsAsFactors = FALSE, skip=6, header = T)
colnames(fire_2)=c("run","density","prob","step","burned") # cambio el nombre de las columnas
res <- fire_2 %>%
  group_by(c(density)) %>%
  summarise(median(burned))
  # gr�fico con el porcentaje quemado en realci�n a la densidad 

fire_2$prob=as.factor(fire_2$prob)

result=matrix(0,ncol=36,nrow=31)
row.names(result)=seq(50,80,1)
colnames(result)=seq(0.65,1,0.01)

for (i in 1:31){
  for (j in 1:36){
    res <- fire_2 %>%
      filter(., density == ( (i-1) + 50 ) & prob == (0.65 + ((j-1)*0.01)) ) %>%
      summarise(mean(burned))
      result[i,j]=res$`mean(burned)`
  }
}

image2D(result,seq(50,80,1),seq(0.65,1,0.01),xlab="density",ylab="prob",main="Experimento 2")
contour2D(result,seq(50,80,1),seq(0.65,1,0.01),xlab="density",ylab="prob",main="Experimento 2")

## D�nde encontramos el tipping point? depende de ambas variables?


