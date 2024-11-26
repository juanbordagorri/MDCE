library(ggplot2)
library(data.table)
library(patchwork)

gg_logistica <- function(lambda = 3.8, cond.inicial = 0.3, 
                         iteraciones = 600, cortar = 200){
    
    if(cortar>iteraciones){
        cat("cortar debe ser menor a la cantidad de iteraciones\n")
        stop()
    }
    
    a = lambda 
    q=seq(0,1,0.001)
    logist=q
    for (i in 1:length(q)) {
        logist[i]=a*q[i]*(1-q[i])  
    }
    
    data <- data.table(
        index = q,
        logist = logist
    )
    
    plot <- ggplot(data, aes(index, logist))+
        geom_line(linewidth= 1)+
        geom_abline(slope = 1, intercept = 0, col = "red", linewidth= 1)
    
    out <- data.table(
        result = vector("numeric",iteraciones),
        idx = 1:iteraciones
    )
    
    out[1, result := cond.inicial]
    for (i in 2:out[,.N]){
        #out[i, result := a*result[i-1]*(1-result[i-1])]
        out$result[i]=a*out$result[i-1]*(1-out$result[i-1])
    }
    
    # punto fijo P_lambda
    p1 <- out |> ggplot(aes(idx,result))+
        geom_line()+
        geom_hline(yintercept = (a-1)/a, col ="green",linewidth =1)+
        theme_minimal()+labs(x = NULL,y = NULL)
    
    # dinamica
    #comienzo <- if(cortar){200} else {2}
    
out[, back := shift(result,n=-1)]
p2 <- plot +
    geom_point(data = out[cortar:.N], aes(back,back), shape = 1)+
    geom_point(data = out[cortar:.N], aes(result,back), shape = 1)+
    geom_segment(data = out[cortar:.N], aes(x = result, 
                                         yend = back,
                                         y=result))+
    geom_segment(data = out[cortar:.N], aes(x=back,
                                         y= back,
                                         xend=result))+
    theme_minimal()+labs(x = NULL,y = NULL)

    p1+p2
}

gg_logistica(lambda = 3.8,
             cond.inicial = 0.8,
             iteraciones = 600,
             cortar = 500)

