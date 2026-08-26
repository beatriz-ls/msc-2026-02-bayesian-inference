# questão 3

# Gamma

## shape variando e rate constante
x <- seq(0, 25, length.out = 1000)

plot(x, dgamma(x, shape = 1, scale = 2),
     type = "l",
     lwd = 2,
     col = "red",
     xlab = "x",
     ylab = "Densidade",
     main = expression("Distribuição Gama" ~ beta == 2))

lines(x, dgamma(x, shape = 2, scale = 2),
      col = "blue", lwd = 2)

lines(x, dgamma(x, shape = 3, scale = 2),
      col = "green", lwd = 2)

lines(x, dgamma(x, shape = 5, scale = 2),
      col = "purple", lwd = 2)

legend("topright",
       legend = c("α = 1", "α = 2", "α = 3", "α = 5"),
       col = c("red", "blue", "green", "purple"),
       lwd = 2)

dev.copy(png, "plot/grafico_gamma_rate_fixo.png",
         width = 800, height = 600, res = 100)

dev.off()

## rate variando e shape constante

plot(x, dgamma(x, shape = 2, scale = 2),
     type = "l",
     lwd = 2,
     col = "red",
     xlab = "x",
     ylab = "Densidade",
     main = expression("Distribuição Gama" ~ alpha == 2))

lines(x, dgamma(x, shape = 2, scale = 3),
      col = "blue", lwd = 2)

lines(x, dgamma(x, shape = 2, scale = 4),
      col = "green", lwd = 2)

lines(x, dgamma(x, shape = 2, scale = 5),
      col = "purple", lwd = 2)

legend("topright",
       legend = c(
         expression(beta == 2),
         expression(beta == 3),
         expression(beta == 4),
         expression(beta == 5)
       ),
       col = c("red", "blue", "green", "purple"),
       lwd = 2)

dev.copy(png, "plot/grafico_gamma_shape_fixo.png",
         width = 800, height = 600, res = 100)

dev.off()

# Gamma Inversa

## shape variando e rate constante
plot(x, dgamma(1/x, shape = 1, scale = 1)/x^2,
     type = "l",
     lwd = 2,
     col = "red",
     xlab = "x",
     ylab = "Densidade",
     main = expression("Distribuição Gama Invertida" ~ beta == 1))

lines(x, dgamma(1/x, shape = .8, scale = 1)/x^2,
      col = "blue", lwd = 2)

lines(x, dgamma(1/x, shape = .6, scale = 1)/x^2,
      col = "green", lwd = 2)

lines(x, dgamma(1/x, shape = .4, scale = 1)/x^2,
      col = "purple", lwd = 2)

legend("topright",
       legend = c("α = 1", "α = .8", "α = .6", "α = .4"),
       col = c("red", "blue", "green", "purple"),
       lwd = 2)

dev.copy(png, "plot/grafico_gamma_inv_rate_fixo.png",
         width = 800, height = 600, res = 100)

dev.off()

## rate variando e shape constante
plot(x, dgamma(1/x, shape = 1, scale = 1)/x^2,
     type = "l",
     lwd = 2,
     col = "red",
     xlab = "x",
     ylab = "Densidade",
     main = expression("Distribuição Gama Invertida" ~ alpha == 1))

lines(x, dgamma(1/x, shape = 1, scale = .8)/x^2,
      col = "blue", lwd = 2)

lines(x, dgamma(1/x, shape = 1, scale = .6)/x^2,
      col = "green", lwd = 2)

lines(x, dgamma(1/x, shape = 1, scale = .4)/x^2,
      col = "purple", lwd = 2)

legend("topright",
       legend = c(
         expression(beta == 1),
         expression(beta == .8),
         expression(beta == .6),
         expression(beta == .4)
       ),
       col = c("red", "blue", "green", "purple"),
       lwd = 2)

dev.copy(png, "plot/grafico_gamma_inv_shape_fixo.png",
         width = 800, height = 600, res = 100)

dev.off()

## Grafico de \beta = \alpha = (1, 10,0.001)

plot(x, dgamma(1/x, shape = 1, scale = 1)/x^2,
     type = "l",
     lwd = 2,
     col = "red",
     xlab = "x",
     ylab = "Densidade",
     main = expression("Distribuição Gama Invertida" ~ alpha == beta))

lines(x, dgamma(1/x, shape = 10, scale = 10)/x^2,
      col = "blue", lwd = 2)

lines(x, dgamma(1/x, shape = 0.001, scale = 0.001)/x^2,
      col = "green", lwd = 2)

## Gerando amostra de uma gamma inversa a paretir de uma exponencial

n <- 1000
alpha <- 3
beta <- 2

Y <- numeric(n)

for (j in 1:n) {
  
  soma <- 0
  
  for (i in 1:alpha) {
    
    E <- rexp(1, rate = 1/beta)
    
    soma <- soma + E
  }
  
  Y[j] <- 1 / soma
}
Y

hist(Y,
     probability = TRUE,
     breaks = 40,
     col = "lightblue",
     main = "Gama Inversa",
     xlab = "Y")
