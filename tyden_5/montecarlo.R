#########################################################################
# *Monte Carlo* integrování


## Úkoly


### 1. 

MC1 <- function(n, r) { 
  U <- runif(n)       # generujeme U ~ Rs(0; 1)
  X <- r(U)           # transformace X = r(U)
  int <- mean(X)      # bodovy odhad integralu = vyberovy prumer (X)
  sigma2 <- var(X)    # odhad sigma^2 = vyberovy rozptyl (X)
  CI <- int + c(-1, 1) * qnorm(0.995) * sqrt(sigma2 / n) # 99% IS
  return(c(int = int, CI = CI))
}
I <- MC1(1e4, function (x) {exp(-x^2 /2) / sqrt(2 * pi)}) 
print(c(I, exact = pnorm(1) - pnorm(0)))

### 2.

MC2 <- function(n, r, a, b) {
  U <- runif(n, a, b)      # generujeme U ~ Rs(a; b)
  X <- r(U)                # transformace X = r(U)
  int <- (b - a) * mean(X) # bodovy odhad integralu = vyberovy prumer (X)
  sigma2 <- var(X)         # odhad sigma^2 = vyberovy rozptyl (X)
  CI <- int + c(-1, 1) * (b - a) * qnorm(0.995) * sqrt(sigma2 / n) # 99% IS
  return(c(int = int, CI = CI))
}
I <- MC2(1e4, function (x) {exp(-x^2 /2) / sqrt(2 * pi)}, -1.96, 1.96) # MC
print(c(I, exact = pnorm(1.96) - pnorm(-1.96)))

### 3.

I <- MC2(1e4, function (x) {x}, 0, 1)
print(c(I, exact = 1/2))

### 4.

I <- MC2(1e4, function (x) {exp(-x)}, 0, 5)
print(c(I, exact = exp(-0) - exp(-5)))

### 5.

MC3 <- function(n, r, theta) {
  U <- runif(n)             # U ~ Rs(0; 1)
  X.star <- -log(U) / theta # X.star ~ Ex(theta*)
  X <- r(X.star)
  int <- mean(X)
  sigma2 <- var(X)
  CI <- int + c(-1, 1) * qnorm(0.995) * sqrt(sigma2 / n) # 99% IS
  return(c(int = int, CI = CI))
}
I <- MC3(1e4, function (x) {ifelse(x <= 5, 1, 0)}, 1)
print(c(I, exact = exp(-0) - exp(-5)))

### 6.

I <- MC3(1e4, function (x) {x^2}, 1)
print(c(I, exact = gamma(3)))

### 7.

MC4 <- function(n, r, theta, theta.star) {
  U <- runif(n)                  # U ~ Rs(0; 1)
  X.star <- -log(U) / theta.star # X.star ~ Ex(theta)
  X <- r(X.star) * dexp(X.star, theta) / dexp(X.star, theta.star)
  int <- mean(X)
  sigma2 <- var(X)
  CI <- int + c(-1, 1) * qnorm(0.995) * sqrt(sigma2 / n) # 99% IS
  return(c(int = int, CI = CI))
}
I <- MC4(1e4, function (x) {x^2}, 1, 3)
print(c(I, exact = gamma(3)))

### 8.

I <- MC4(1e4, function (x) {x^2}, 1, 1/2)
print(c(I, exact = gamma(3)))

### 9.

I <- MC3(1e4, function (x) {ifelse(x <= 5, x^2, 0)}, 1)
print(c(I, exact = 2 - 37 * exp(-5)))

### 10.

MC5 <- function(n, r, a, b, c) {
  X <- runif(n, a, b) # X ~ Rs(a; b)
  Y <- runif(n, 0, c) # Y ~ Rs(0; c)
  df <- data.frame(X, Y)
  df$test <- Y <= r(X)
  int <- (b - a) * c * sum(df$test) / n
  df$test <- factor(df$test, labels = c("reject", "accept"))
  return(list(int = int, df = df))
}
I <- MC5(1e4, function (x) {x^2 * exp(-x)}, 0, 5, 0.6)
I$int
ggplot(I$df, aes(x = X, y = Y, color = test)) + geom_point(size = 0.8)

### 11.

MC5 <- function(n, r, a, b, c) {
  X <- runif(n, a, b) # X ~ Rs(a; b)
  Y <- runif(n, 0, c) # Y ~ Rs(0; c)
  df <- data.frame(X, Y)
  df$test <- Y <= r(X)
  int <- (b - a) * c * sum(df$test) / n
  df$test <- factor(df$test, labels = c("reject", "accept"))
  return(list(int = int, df = df))
}
I <- MC5(1e4, function (x) {x^2 * abs(sin(x)) * exp(-x)}, 0, 2*pi, 0.6)
I$int
ggplot(I$df, aes(x = X, y = Y, color = test)) + geom_point(size = 0.8)
