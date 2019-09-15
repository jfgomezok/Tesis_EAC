
# Si no está instalado, instalamos el paquete "pacman"
if ( !require("pacman") ) install.packages("pacman")

# Instalamos y Cargamos los paquetes necesarios
pacman::p_load( tidyverse, NlcOptim)


# carga de datos
data <- read.csv("raw_data/retornos.csv")[,-1]

#Lectura de su estructura
class(data)
str(data)


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
v2 <- t(c(1, 0, 0, 0))
# class(v2)
# dim(v2)
# length(v2)


# Vector columa de costos de transaccion para cada activo.
k <- t(t(c(0.005, 0.026, 0.042, 0.062)))
# k <- t(t(c(0, 0, 0, 0)))

# class(k)
# dim(k)
# length(k)


# vector fila de unos
f <- t(rep(1, dim(data)[1]))
# class(f)
# dim(f)
# length(f)


# funcion de utilidad exponencial negativa sobre los retornos
# Tiene un -1 adelante porque la funcion "solnl" MINIMIZA, y  nosotros estamos buscando un maximo.

eval_f <- function(x) {
  
  return( 
    
    # x <- c(0, 0.639427, 0.115038, 0.245536) # para chequear que la funcion de un numero como resultado.
    
    -1 * ( (1/n) * (f %*% t(   -exp (-j*( f + x %*% t(data) -  as.numeric(abs(x-v2) %*% k)  ))
                            )
                    )
          )
  )
}



n <- dim(data)[1]
demandas <- data.frame()

# j es la aversión al riesgo que vamos a probar para 5 valores:

for (j in 1:5){


T1 <- solnl( X = v2,
             objfun = eval_f,
             Aeq = t(v1),
             Beq = 1,
             lb = v0,
             ub = v1
             )

demandas <- rbind(demandas, round(T1$par,6))

}


colnames(demandas) <- colnames(data)

demandas$aversion_riesgo <- c(1,2,3,4,5)

demandas_aux <- gather(demandas, -5, key = 'activo', value = 'tenencia')




ggplot(demandas_aux,
            aes(x = aversion_riesgo, y = tenencia, color=activo))+
  geom_line() + geom_point() +
  labs(title = "Optimización de cartera",
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

