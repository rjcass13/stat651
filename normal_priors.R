x <- c(7, 5, 5, 5, 6, 5, 10, 6, 4.5, 5, 5, 7, 7, 4.5, 4.5)
hist(x, breaks = 10)

n <- length(x)
xbar <- mean(x)
s2 <- var(x)

rinvgamma <- function(n, shape, scale) {
  1.0 / rgamma(n, shape = shape, rate = scale)
}

dinvgamma <- function(x, shape, scale) {
  dgamma(1.0 / x, shape = shape, rate = scale) / x^2
}

a1 <- (n-1)/2
b1 <- (n-1)*s2/2


nsim <- 10000

sig2 <- rinvgamma(nsim, a1, b1)
mu <- rnorm(nsim, xbar, sqrt(sig2/n))

cbind(mu, sig2)
plot(mu, sig2, pch = '.')

mean(mu)

# Posterior prob that the avg. time spent on HW1 > 7
mean(mu>7)
