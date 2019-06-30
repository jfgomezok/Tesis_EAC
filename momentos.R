


# vector de medias -------------------------------------------------------

M1 <- data.frame()

for (i in 1:dim(data)[2]){
  # i <- 1
  M1[i,1] <- mean(data[,i])
}






# matriz de varianzas y covarianzas ---------------------------------------

M2 <- data.frame()

for (i in 1:dim(data)[2]){
  # i <- 1
  
  for (j in 1:dim(data)[2]){
    # j <- 1
    u <- 0
    for (t in 1:dim(data)[1]){
      # t <- 1
      u <- u + (( data[t,i] - mean(data[,i])) * ( data[t,j]-mean(data[,j])))  #es la suma del producto de cada valor menor la media de la columna
    }
    
    M2[i,j] <- u / (dim(data)[1]-1)
  }
}



# matriz de sesgos -------------------------------------------------------------

M3 <- data.frame()

for (i in 1:dim(data)[2]){
  s <- data.frame()
  # i <- 1
  for (j in 1:dim(data)[2]){
    # j<-1
    for (k in 1:dim(data)[2]){
      # k <- 2
      u <- 0
      for (t in 1:dim(data)[1]){
        # t<-1
        u <- u + ((data[t,i] - mean(data[,i])) * (data[t,j] - mean(data[,j])) *
                    (data[t,k] - mean(data[,k])))
        
      }
      s[j,k] <- u / dim(data)[1]
    }
    
  }
  M3 <- rbind(M3, s)
}


# matriz de kurtosis ----------------------------------------------------------------

M4 <- data.frame()

for (i in 1:dim(data)[2]){
  # i <- 1
  for (j in 1:dim(data)[2]){
    s <- data.frame()
    # j<-1
    for (k in 1:dim(data)[2]){
      # k <- 2
      for (l in 1:dim(data)[2]){
        # l<-1
        u <- 0
        for (t in 1:dim(data)[1]){
          # t<-1
          u <- u + ( (data[t,i] - mean(data[,i])) * (data[t,j] - mean(data[,j])) *
                       (data[t,k] - mean(data[,k])) * ( data[t,l] - mean(data[,l])) )
          
        }
        s[k,l] <- u / dim(data)[1]
      }
    }
    M4 <- rbind(M4, s)
  }
}


rm(s, i, j, k, l , t, u)
