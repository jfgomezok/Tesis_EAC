
# Si no está instalado, instalamos el paquete "pacman"
if ( !require("pacman") ) install.packages("pacman")

# Instalamos y Cargamos los paquetes necesarios
pacman::p_load( tidyverse, stargazer)



# Carga de datos
data <- read.csv("raw_data/retornos.csv")

# Lectura de la estructura
class(data)
str(data)
stargazer(data, type =  "text")

# Vemos que la fecha es de tipo "Factor". Se la cambiamos a tipo "date":
data$fecha <- as.Date(data$fecha, "%d/%m/%Y")

# Verificamos que este todo ok
str(data)


# Visualizamos serie temporal del activo local:
ggplot(data, aes(x=fecha))+
  geom_line(aes(y= act_local))

# Visualizamos serie temporal del activo externo:
ggplot(data, aes(x=fecha))+
  geom_line(aes(y= act_ext))


# Las dos juntas!
ggplot(data, aes(x=fecha))+
  geom_line(aes(y= act_local))+
  geom_line(aes(y= act_ext))


# Para resolver el problema de tener que sumar una por una, hay que mejorar la fuente de los datos:
data %>%
  gather(key = 'activo', 'retorno', -1) %>%
  ggplot(aes(x = fecha, y = retorno, colour= activo))+
  geom_line()


# podemos filtrar un periodo en particular:
data %>%
  filter(between(fecha, as.Date('1980-01-01'), as.Date('1993-01-01'))) %>%
  gather(key = 'activo', 'retorno', -1) %>%
  ggplot(aes(x = fecha, y = retorno, colour= activo))+
    geom_line()+
    theme_minimal()
  
# podemos filtrar un periodo y activos en particular:
data %>%
  filter(between(fecha, as.Date('1980-01-01'), as.Date('1993-01-01'))) %>%
  gather(key = 'activo', 'retorno', -1) %>%
  filter(activo %in% c('act_local', 'act_ext')) %>%
  ggplot(aes(x = fecha, y = retorno, colour= activo))+
  geom_line()+
  theme_minimal()


# Exceso de retorno del activo externo sobre el activo local
data %>%
  select(fecha, act_local, act_ext) %>%
  mutate( erae = act_ext - act_local) %>%
  ggplot(aes(x = fecha, y = erae))+
    geom_line(color='red')+
    geom_hline(yintercept = 0, linetype = 'dashed') +
    theme_minimal()+
    ggtitle("Exceso de Retorno del Activo Externo sobre el Activo Local", 
            subtitle = "En términos reales, para tenencia igual a un año") +
    labs(caption = "Fuente: Tesis Doctoral de Eduardo Ariel Corso") 



# Histogramas
data %>%
  select(-fecha) %>%
  gather(key = 'activo', 'retorno') %>%
  ggplot(aes(x= retorno))+
    geom_histogram(bins = 50)+
    facet_wrap(~ activo)


# Funciones de densidad
data %>%
  select(-fecha) %>%
  gather(key = 'activo', 'retorno') %>%
  ggplot(aes(x= retorno, fill = activo))+
    geom_density(alpha=0.3)+
    facet_wrap(~ activo)


# Veamos un BoxPlot
data %>%
  select(-fecha) %>%
  gather(key = 'activo', 'retorno') %>%
  ggplot(aes(x=activo, y = retorno))+
    geom_boxplot(outlier.color = 'red')



# Calculamos una matriz, de identicas dimensiones, con activos que simulen la misma media
# y varianza, pero con distribución normal.
x <- data.frame(norm_act_local = rnorm(n = 424, mean = mean(data$act_local), sd = sd(data$act_local)),
                norm_act_ext = rnorm(n = 424,  mean = mean(data$act_ext), sd = sd(data$act_ext)),
                norm_inmueble = rnorm(n = 424, mean = mean(data$inmueble), sd = sd(data$inmueble)),
                norm_acciones = rnorm(n=424, mean = mean(data$acciones), sd = sd(data$acciones)))

# Visualicemos la nueva matriz
x %>%
  gather(key = 'activo', 'retorno') %>%
  ggplot(aes(x=activo, y = retorno))+
  geom_boxplot(outlier.color = 'red')

x %>%
  gather(key = 'activo', 'retorno') %>%
  ggplot(aes(x=retorno))+
  geom_density()+
  facet_wrap(~activo)


# Juntamos los datoss de la distribucion normal, con los datos muestrales
data2 <- cbind(data, x)

# Calculamos una matriz de medias, para graficar las lineas verticales
medias <- as.data.frame(colMeans(data[-1])) %>%
  mutate(activo = row.names(.))
colnames(medias) <- c('mean', 'clase')


# Armamos los histogramas con las funciones normales arriba
data2 %>%
  select(-fecha) %>%
  gather(key = 'activo', value = 'retorno') %>%
  mutate(clase = activo,
         clase = gsub('norm_', '', clase)) %>%
  ggplot(aes(x=retorno, fill=activo))+
    geom_density(alpha=0.4)+
    geom_vline(data = medias, aes(xintercept=mean), linetype="dashed")+
    facet_wrap(.~clase)+
    ggtitle("Funciones de Distribución de los Retornos de las Reservas de Valor en Argentina", 
            subtitle = "En términos reales, para tenencia igual a un año") +
    labs(caption = "Fuente: Tesis Doctoral de Eduardo Ariel Corso") 
  


