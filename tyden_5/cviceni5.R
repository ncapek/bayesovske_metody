library("dplyr")
library("ggplot2")

## Overení správnosti generátoru pseudonáhodných císel

N <- 100
a <- 1
b <- 2
X <- rgamma(N, shape = a, rate = b) # generator pseudonahodnych cisel
df.X <- data.frame(x = X)

x <- seq(min(X), max(X), by = 0.01)
f <- dgamma(x, shape = a, rate = b)	# teoreticka hustota 
F <- pgamma(x, shape = a, rate = b) # teoreticka distribucni funkce (CDF)
df.teor <- data.frame(x = x, f = f, F = F)

ggplot(df.X, aes(x = x, y = ..density..)) + 
  geom_histogram(colour = "black", fill = "#FFCC00", bins = ceiling(2*N^(1/3))) + # histogram
  geom_line(data = df.teor, mapping = aes(x = x, y = f), col = "red", size = 1.0) + # teoreticka hustota
  xlab("x") + ylab("hustota pravdepodobnosti")
ggplot(df.X, aes(x = x)) + 
  stat_ecdf(colour = "black", geom = "step") + # ECDF
  geom_line(data = df.teor, mapping = aes(x = x, y = F), col = "red", size = 1.0) + # teoreticka CDF
  xlab("x") + ylab("distribucni funkce")
ggplot(df.X, aes(sample = x)) + 
  geom_qq(colour = "black", distribution = qgamma, dparams = list(shape = a, rate = b)) + # Q-Q plot
  geom_qq_line(colour = "red", distribution = qgamma, dparams = list(shape = a, rate = b)) # teorie
# p-hodnota by mela byt vetsi nez 5 %, tzn. nezamitnuti nulove hypotezy o shode empirickeho a teoretickeho rozdeleni
ks.test(X, "pgamma", shape = a, rate = b) # funkce "ks.test" = Kolmogorovuv-Smirnovuv test
# hodnoty ACF pro lag>0 mimo vyznaceny interval ukazuji na vyznamnou korelaci a tudiz porusenou nezavislost
acf.df <- with(acf(X, lag.max = 50, plot = FALSE), data.frame(lag, acf)) # funkce "acf" = ACF
meze <- qnorm(0.05 / 2) / sqrt(N) # meze pro test nekorelovanosti
ggplot(acf.df, aes(x = lag, y = acf)) + 
  geom_bar(stat = "identity", colour = "black", size = 0.2, fill = "#FFCC00", width = 1.0) + # ACF
  geom_hline(yintercept = meze, color = "red") + geom_hline(yintercept = -meze, color = "red")

## Metoda inverzní transformace


### Exponenciální rozdelení

N <- 1e4
theta <- 3
X <- runif(N)
Y <- -log(X) / theta
df.Y <- data.frame(Y)
ggplot(df.Y, aes(x = Y, y = ..density..)) + 
  geom_histogram(colour = "black", fill = "#FFCC00", bins = ceiling(2*N^(1/3))) +
  xlab("y") + ylab("hustota pravdepodobnosti")
ks.test(Y, "pexp", rate = theta)
```

### Chí-kvadrát rozdelení se sudými stupni volnosti



### Gama rozdelení s celocíselným $a$



### Beta rozdelení s celocíselnými $a$ a $b$



### Standardizované normální rozdelení -- Boxova-M\"ullerova transformace



## Diskrétní rozdelení pravdepodobnosti

y <- c(0, 1, 9) # generujeme hodnoty 0, 1 a 9
p.y <- c(1/2, 1/4, 1/4) # s pravdepodobnostmi 1/2, 1/4 a 1/4
N <- 1000
Y <- sample(y, N, replace = TRUE, prob = p.y)
table(Y) # frekvencni tabulka

### Vlastní generátor

N <- 1e4
y <- c(10, 20, 30, 40, 50)
p.y <- c(0.1, 0.2, 0.3, 0.3, 0.1)
razeni <- order(p.y, decreasing = TRUE)
(pp.y <- p.y[razeni])
(yy <- y[razeni])
(c <- cumsum(pp.y))
X <- runif(N)
Y <- sapply(X, function(x) {yy[sum(x > c) + 1]} ) # "cyklus" prirazeni Y podle hodnoty X
table(Y)
df <- data.frame(Y, generator = "vlastni")
df$Y <- factor(df$Y, levels = y, ordered = TRUE)
ggplot(df, aes(x = Y, y = (..count..)/sum(..count..), fill = generator)) + 
  geom_bar(colour = "black", position = "dodge") + 
  labs(y = "pravdepodobnost") + theme(legend.position = "bottom")

### Alternativní rozdelení

N <- 1e4
n <- 10
theta <- 0.3
y <- seq(1, 0)
p.y <- c(theta, 1 - theta)
razeni <- order(p.y, decreasing = TRUE)
pp.y <- p.y[razeni]
yy <- y[razeni]
c <- cumsum(pp.y)
X <- runif(N)
Y <- sapply(X, function(x) {yy[sum(x > c) + 1]} )
Z <- rbinom(N, 1, theta) # generator v R
df <- rbind(data.frame(Y, generator = "vlastni"), data.frame(Y = Z, generator = "R"))
df$Y <- factor(df$Y, levels = y, ordered = TRUE)
ggplot(df, aes(x = Y, y = (..count..)/sum(..count..), fill = generator)) + 
  geom_bar(colour = "black", position = "dodge") + 
  labs(y = "pravdepodobnost") + scale_x_discrete(drop = FALSE) + theme(legend.position = "bottom")

### Binomické rozdelení

N <- 1e4
n <- 10
theta <- 0.3
y <- seq(0, n, by = 1)
p.y <- dbinom(y, n, theta)
razeni <- order(p.y, decreasing = TRUE)
pp.y <- p.y[razeni]
yy <- y[razeni]
c <- cumsum(pp.y)
X <- runif(N)
Y <- sapply(X, function(x) {yy[sum(x > c) + 1]} )
Z <- rbinom(N, n, theta) # generator v R
df <- rbind(data.frame(Y, generator = "vlastni"), data.frame(Y = Z, generator = "R"))
df$Y <- factor(df$Y, levels = y, ordered = TRUE)
ggplot(df, aes(x = Y, y = (..count..)/sum(..count..), fill = generator)) + 
  geom_bar(colour = "black", position = "dodge") + 
  labs(y = "pravdepodobnost") + 
  scale_x_discrete(drop = FALSE) + theme(legend.position = "bottom")

### Binomické rozdelení -- jako soucet alternativních

N <- 1e4
n <- 10
theta <- 0.3
y <- seq(0, 1)
p.y <- c(1 - theta, theta)
razeni <- order(p.y, decreasing = TRUE)
pp.y <- p.y[razeni]
yy <- y[razeni]
c <- cumsum(pp.y)
X <- runif(n * N)
Y.alternativni <- matrix(sapply(X, function(x) {yy[sum(x > c) + 1]} ), ncol = n)
Y <- apply(Y.alternativni, 1, sum) # soucet v kazdem radku, obdrzime binomickou n. v.
Z <- rbinom(N, n, theta) # generator v R
df <- rbind(data.frame(Y, generator = "vlastni"), data.frame(Y = Z, generator = "R"))
df$Y <- factor(df$Y, levels = seq(0, n, by = 1), ordered = TRUE)
ggplot(df, aes(x = Y, y = (..count..)/sum(..count..), fill = generator)) + 
  geom_bar(colour = "black", position = "dodge") + 
  labs(y = "pravdepodobnost") + 
  scale_x_discrete(drop = FALSE) + theme(legend.position = "bottom")

### Poissonovo rozdelení

N <- 1e4
theta <- 3
y <- round(seq(max(0, theta - 3*sqrt(theta)), theta + 3*sqrt(theta), by = 1))
p.y <- dpois(y, theta)
sum(p.y) # p.y v nasi aproximaci neni rozdeleni pravdepodobnosti
p.y <- p.y / sum(p.y) # nutno normalizovat na soucet = 1
razeni <- order(p.y, decreasing = TRUE)
pp.y <- p.y[razeni]
yy <- y[razeni]
c <- cumsum(pp.y)
X <- runif(N)
Y <- sapply(X, function(x) {yy[sum(x > c) + 1]} )
Z <- rpois(N, theta) # generator v R
df <- rbind(data.frame(Y, generator = "vlastni"), data.frame(Y = Z, generator = "R"))
df$Y <- factor(df$Y, levels = y, ordered = TRUE)
ggplot(df, aes(x = Y, y = (..count..)/sum(..count..), fill = generator)) + 
  geom_bar(colour = "black", position = "dodge") + 
  labs(y = "pravdepodobnost") + 
  scale_x_discrete(drop = FALSE) + theme(legend.position = "bottom")

### Poissonovo rozdelení -- pomocí exponenciálního

N <- 1e4
theta <- 3
Y <- sapply(1:N, function(i) {
  n <- 0
  T <- 0
  while (T <= 1) {
    U <- runif(1)
    T.i <- -log(U) / theta
    T <- T + T.i
    n <- n + 1
  }
  return(n-1)
})
Z <- rpois(N, theta)
df <- rbind(data.frame(Y, generator = "vlastni"), data.frame(Y = Z, generator = "R"))
df$Y <- factor(df$Y)
ggplot(df, aes(x = Y, y = (..count..)/sum(..count..), fill = generator)) + 
  geom_bar(colour = "black", position = "dodge") + 
  labs(y = "pravdepodobnost") + theme(legend.position = "bottom")

## Zamítací metoda (*rejection sampling*, *acceptance-rejection method*)

1. Generujeme $X$ z rozdelení pravdepodobnosti s hustotou $f_X$. 
2. Nezávisle na $X$ generujeme $U \sim \mathsf{Rs}(0; 1)$.
3. Pokud $$ U \leq \frac{f_Y(X)}{c \, f_X(X)}, $$ potom $Y = X$. Jinak se vracíme na krok 1.

### Cílové: beta, výchozí: rovnomerné

N <- 1e4
a <- 2.7
b <- 6.3
c <- 2.669
X <- runif(N)
U <- runif(N)
test <- ifelse(U < dbeta(X, a, b) / (c * 1), "prijato", "zamitnuto") # zde je hustota f_X = 1
Y <- ifelse(test == "prijato", X, NA)
df <- data.frame(X, U, test, Y)
p <- ggplot(df, aes(x = Y, y = ..density..)) + 
  geom_histogram(colour = "black", fill = "#FFCC00", bins = ceiling(2*N^(1/3)))
y <- seq(0, 1, by = 0.01)
f.Y <- dbeta(y, a, b)
df.teoret <- data.frame(y, f.Y)
c(sum(!is.na(Y)) / N, 1/c, c) # pravdepodobnost prijeti a stredni pocet pruchodu cyklem
p + geom_line(data = df.teoret, aes(x = y, y = f.Y), colour = "red", size = 1.2) # teoreticka hustota
ggplot(df, aes(x = X, y = U, color = test)) + geom_point(size = 0.8) # Rozdeleni (X, U)
sum(!is.na(Y)) / N # pravdepodobnost prijeti

### Cílové: normální, výchozí: exponenciální



### Cílové: gama, výchozí: exponenciální



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
