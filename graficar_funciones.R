

p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))

fun.1 <- function(x) -exp(-2*(1+x))

p + stat_function(fun = fun.1) + xlim(0,2)
