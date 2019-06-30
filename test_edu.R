library(tidyverse)
library(nloptr)
library(pracma)
library(NlcOptim)


data <- read.csv("retornos.csv")[,-1]
# str(data)


# vector de ceros
v0 <- t(rep(0,dim(data)[2]))
class(v0)
dim(v0)
length(v0)


# vector de unos
v1 <- t(rep(1, dim(data)[2]))
class(v1)
dim(v1)
length(v1)


# vector de Condicion Inicial de tenencias
v2 <- t(c(1, 0, 0, 0))
class(v2)
dim(v2)
length(v2)


# Vector de costos de transaccion
k <- t(t(c(0, 0.1, 0.042, 0.05)))
class(k)
dim(k)
length(k)


# vector de unos
f <- t(rep(1, dim(data)[1]))
class(f)
dim(f)
length(f)


# Objective function
eval_f <- function(x) {
  
  return( 
    
    # x <- c(0.25, 0.25, 0.25, 0.25)
    
    -1 * ( (1/n) * (f %*% t(   -exp (-j*( f + x %*% t(data) -  as.numeric(abs(x-v2) %*% k)  ))
    )
    )
    )
  )
}


# constraint function
eval_g <- function(x) {
  
  return( v2 * x -1 )
  
}

j <- 1
n <- dim(data)[1]


# Para probar el pracma
t1 <- fmincon( x0 = v2,
               fn = eval_f,
               Aeq = v1,
               beq = 1,
               lb = v0,
               ub = v1)




# Para probar el nloptr
t1 <- nloptr( eval_f = eval_f,
              x0 = v2,
              eval_g_eq = eval_g,
              lb = v0,
              ub = v1,
              opts = 'NLOPT_LD_LBFGS')
