library(tidyverse)



data <- read.csv("raw_data/retornos.csv")
data$fecha <- as.Date(data$fecha, "%d/%m/%Y")
str(data)


summary(data[,-1])


max_act_local <- max(data["act_local"])
min_act_local <- min(data["act_local"])

max_act_ext <- max(data["act_ext"])
min_act_ext <- min(data["act_ext"])

max_inmueble <- max(data["inmueble"])
min_inmueble <- min(data["inmueble"])


max_acciones <- max(data["acciones"])
min_acciones <- min(data["acciones"])

x1 <- seq(from = min_act_local, by = 0.001,to = max_act_local)
x2 <- seq(from = min_act_ext, by = 0.001,to = max_act_ext)
x3 <- seq(from = min_inmueble, by = 0.001,to = max_inmueble)
x4 <- seq(from = min_acciones, by = 0.001,to = max_inmueble)


f1 <- density(data[,2])
f2 <- density(data[,3])
f3 <- density(data[,4])
f4 <- density(data[,5])


plot(f1)
plot(f2)
plot(f3)
plot(f4)

