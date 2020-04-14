
# Si no está instalado, instalamos el paquete "pacman"
if ( !require("pacman") ) install.packages("pacman")

# Instalamos y Cargamos los paquetes necesarios
pacman::p_load( tidyverse, NlcOptim)


# 1) Carga de datos ---------------------------------------
data <- read.csv("raw_data/retornos.csv")[,-1]

#Lectura de su estructura
class(data)
str(data)


# 2) Construcción de insumos ------------------------------ 


# vector fila de ceros: limite inferior que puede adoptar las tenencias del portaflio
v0 <- rep(0,dim(data)[2])
# class(v0)
# dim(v0)
# length(v0)


# vector fila de unos: limite superior que puede adoptar las tenencias del portaflio
v1 <- rep(1, dim(data)[2])
# class(v1)
# dim(v1)
# length(v1)


# vector fila de Condicion Inicial de tenencias
v2 <- t(c(1, 0, 0, 0))
# class(v2)
# dim(v2)
# length(v2)


# vector fila de unos de la misma cantidad que tenemos datos
f <- t(rep(1, dim(data)[1]))
# class(f)
# dim(f)
# length(f)


# 3) funcion de utilidad ----------------------------------------

# Tipo exponencial negativa sobre los retornos del portafolio

# Corso(2015, pg. 96): " [...] se supone que el agente forma expectativas en base a la
# información pasada de los retornos reales de los cuatro activos en
# cuestión. Específicamente, se supone que el agente toma en cuenta
# todas las realizaciones conjuntas pasadas del período considerado
# (septiembre de 1977-diciembre de 2012), y a cada una de estas
# realizaciones le asigna la misma probabilidad de ocurrencia en el
# período t +1. Asumir que el agente utiliza la distribución empírica
# correspondiente a todo el período muestral equivale a suponer que todos
# los eventos pasados tienen el mismo peso 1/n en la memoria del agente,
# donde n es el número de realizaciones pasadas."

# Tiene un -1 adelante porque la funcion "solnl" MINIMIZA, y  nosotros estamos buscando un maximo.

eval_f <- function(x) {
  
  return( 
    
    # x <- c(0, 0.639427, 0.115038, 0.245536) # para chequear que la funcion de un numero como resultado.
    
    -1 * ( (1/n) * (f %*% t(   -exp (-j*( f + x %*% t(data)   ))
                            )
                    )
          )
  )
}





#
# 4) Ejercicio de Optimizacion -----------------------------------

# La funcion solnl realiza lo siguiente:
#            min f(x)
#                s.t. ceq(x) = 0
#                       c(x) ≤ 0
#                         Ax ≤ B
#                      Aeq x ≤ Beq
#                      lb≤ x ≤ ub
#
# Leer la ayuda de la funcion para seguir el procedimiento.

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


# El grafico lo hacemos paso a paso:

# Primer paso: definir los datos y la linea:
g <- ggplot(demandas_aux,
       aes(x = aversion_riesgo, y = tenencia, color=activo))+
  geom_line()
plot(g)

# Segundo, agregamos puntos:
g <- g + geom_point()
plot(g)

# tercero, agregamos titulo, subtitulo y fuente:
g <- g + labs(title = "Optimización de cartera",
              subtitle = "Tenencias óptimas de activos para distintos niveles de averisón al riesgo",
              caption = "Fuente: Tesis Doctoral de Eduardo Ariel Corso")
plot(g)

# Cuarto, le sacamos el "estilo" por default: 
g <- g + theme_bw()
  
plot(g)

# Quinto y ultimo, le damos nuestro estilo:
g <- g + theme(legend.position="top",
               panel.border = element_blank(),
               plot.title = element_text(face="bold",hjust =0.5),
               plot.subtitle = element_text(hjust =0.5),
               plot.caption = element_text(size=8, colour = "gray40", hjust =1),
               axis.title = element_blank(),
               panel.grid.minor = element_blank())
plot(g)  


# se puede hacer lo mismo, pero todo junto!
ggplot(demandas_aux,
            aes(x = aversion_riesgo, y = tenencia, color=activo))+
  geom_line() +geom_point() +
  labs(title = "Optimización de cartera: Maximización directa de la utilidad esperada",
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

