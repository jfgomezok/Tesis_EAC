library(tidyverse)
library(NlcOptim)


data <- read.csv("raw_data/retornos.csv")[,-1]
# str(data)

source('momentos.R')



# vector fila de ceros: limite inferior que puede adoptar 'x'
v0 <- rep(0,dim(data)[2])
# class(v0)
# dim(v0)
# length(v0)


# vector fila de unos: limite superior que puede adoptar 'x'
v1 <- rep(1, dim(data)[2])
# class(v1)
# dim(v1)
# length(v1)


# vector fila de Condicion Inicial de tenencias: un guess de 'x'
v2 <- c(1, 0, 0, 0)
# class(v2)
# dim(v2)
# length(v2)





# vector fila de unos
f <- t(rep(1, dim(data)[1]))
# class(f)
# dim(f)
# length(f)


# funcion de utilidad exponencial negativa sobre los retornos
eval_f <- function(x) {
  
  return( 
    
    # x <- v2 # para chequear que la funcion de un numero como resultado.
    # j <- 1
    
    -1*(-1*exp(-1*j*((t(x) %*% (1+ t(t(M1)))))) * (1+((j^2)/2)*(t(x) %*% t(t(M2)) %*% x)))
                                         
  )
}




demandas <- data.frame()

for (j in 1:5){

# j <- 1
  
T1 <- solnl(
  X      = v2,
  objfun = eval_f,
  Aeq    = t(v1),
  Beq    = 1,
  lb     = v0,
  ub     = v1
)

demandas <- rbind(demandas, round(t(T1$par),6))

}


colnames(demandas) <- colnames(data)

demandas$aversion_riesgo <- c(1,2,3,4,5)

demandas_aux <- gather(demandas, -5, key = 'activo', value = 'tenencia')

ggplot(demandas_aux,
       aes(x = aversion_riesgo, y = tenencia, color=activo))+
  geom_line()+
  geom_point()+
  labs(title = "Optimización de cartera: Maximización de una Aproximación de Taylor (N=2) a la función de utilidad",
       subtitle = "Tenencias óptimas de activos para distintos niveles de averisón al riesgo",
       caption = "Fuente: Tesis Doctoral de Eduardo Ariel Corso") +
  theme_bw() +
  theme(legend.position="top",
        panel.border = element_blank(),
        plot.title = element_text(face="bold",hjust =0.5),
        plot.subtitle = element_text(hjust =0.5),
        plot.caption = element_text(size=8, colour = "gray40", hjust =1),
        axis.title = element_blank(),
        panel.grid.minor = element_blank())




