library("dplyr")
library("ggplot2")

# Studie: Aposteriorní hustota pri zmene v binomickém experimentu

### Pevný pocet opakování, meníme pocty úspechu

n <- 10
y <- c(0, 1, 3, 5, 7, 9, 10)
theta <- seq(0, 1, by = 0.001)
df <- expand.grid(
  y = y, 
  theta = theta, 
  KEEP.OUT.ATTRS = FALSE
)
df1 <- df %>% mutate(
  n = n,
  h = dbeta(theta, 1 + y, 1 + n - y), 
  y = factor(y), 
  n = factor(n)
)
ggplot(df1, aes(x = theta, y = h, colour = y:n)) + 
  geom_line(size = 1) + 
  xlab("theta") + ylab("aposteriorni hustota")

### Meníme pocty opakování, ale pozorujeme jen samé neúspechy
n <- c(0, 1, 2, 3, 5)
y <- 0
theta <- seq(0, 1, by = 0.001)
df <- expand.grid(
  n = n, 
  theta = theta, 
  KEEP.OUT.ATTRS = FALSE
)
df1 <- df %>% mutate(
  n = n,
  h = dbeta(theta, 1 + y, 1 + n - y), 
  y = factor(y), 
  n = factor(n)
)
ggplot(df1, aes(x = theta, y = h, , colour = y:n)) + 
  geom_line(size = 1) + 
  xlab("theta") + ylab("aposteriorni hustota")

### Meníme pocty opakování, ale pozorujeme jen jeden úspech
n <- c(1, 2, 3, 5, 10)
y <- 1
theta <- seq(0, 1, by = 0.001)
df <- expand.grid(
  n = n, 
  theta = theta, 
  KEEP.OUT.ATTRS = FALSE
)
df1 <- df %>% mutate(
  n = n,
  h = dbeta(theta, 1 + y, 1 + n - y), 
  y = factor(y), 
  n = factor(n)
)
ggplot(df1, aes(x = theta, y = h, colour = y:n)) + 
  geom_line(size = 1) + 
  xlab("theta") + ylab("aposteriorni hustota")

### Meníme pocty opakování (sudé), pozorujeme vzdy polovinu úspechu
n <- c(0, 2, 4, 6, 10)
theta <- seq(0, 1, by = 0.001)
df <- expand.grid(
  n = n, 
  theta = theta, 
  KEEP.OUT.ATTRS = FALSE
)
df1 <- df %>% mutate(
  n = n,
  h = dbeta(theta, 1 + n/2, 1 + n - n/2), 
  y = factor(n/2), 
  n = factor(n)
)
ggplot(df1, aes(x = theta, y = h, colour = y:n)) + 
  geom_line(size = 1) + 
  xlab("theta") + ylab("aposteriorni hustota")

# Studie: Aposteriorní císelné charakteristiky


### V závislosti na poctu úspechu $y$ pri pevném poctu opakování (napr. $n = 20$)

df <- data.frame(n = 20, y = seq(0, 20, by = 1))
df1 <- df %>% mutate(
  a = 1 + y, 
  b = 1 + n - y, 
  E = a / (a + b), 
  Var = a * b / ((a + b)^2 * (a + b + 1)), 
  Mode = (a - 1) / (a + b - 2), 
  y = factor(y), 
  n = factor(n)
)
ggplot(df1, aes(x = y, y = E, group = 1)) + 
  geom_line(size = 0.2, linetype = "dashed", colour = "red") + 
  geom_point(size = 1.5, colour = "red") + 
  xlab("y") + ylab("aposteriorni stredni hodnota") + ylim(c(0, 1))

ggplot(df1, aes(x = y, y = Var, group = 1)) + 
  geom_line(size = 0.2, linetype = "dashed", colour = "darkgreen") + 
  geom_point(size = 1.5, colour = "darkgreen") + 
  xlab("y") + ylab("aposteriorni rozptyl") + ylim(c(0, 0.015))

ggplot(df1, aes(x = y, y = Mode, group = 1)) + 
  geom_line(size = 0.2, linetype = "dashed", colour = "blue") + 
  geom_point(size = 1.5, colour = "blue") + 
  xlab("y") + ylab("aposteriorni modus") + ylim(c(0, 1))

### V závislosti na poctu opakování $n$, kdyz pozorujeme jen samé neúspechy
df <- data.frame(n = seq(0, 20, by = 1), y = 0)
df1 <- df %>% mutate(
  a = 1 + y, 
  b = 1 + n - y, 
  E = a / (a + b), 
  Var = a * b / ((a + b)^2 * (a + b + 1)), 
  Mode = (a - 1) / (a + b - 2), 
  y = factor(y), 
  n = factor(n)
)

ggplot(df1, aes(x = n, y = E, group = 1)) + 
  geom_line(size = 0.2, linetype = "dashed", colour = "red") + 
  geom_point(size = 1.5, colour = "red") + 
  xlab("n") + ylab("aposteriorni stredni hodnota") + ylim(c(0, 1))

ggplot(df1, aes(x = n, y = Var, group = 1)) + 
  geom_line(size = 0.2, linetype = "dashed", colour = "darkgreen") + 
  geom_point(size = 1.5, colour = "darkgreen") + 
  xlab("y") + ylab("aposteriorni rozptyl") + ylim(c(0, 0.1))

ggplot(df1, aes(x = n, y = Mode, group = 1)) + 
  geom_line(size = 0.2, linetype = "dashed", colour = "blue") + 
  geom_point(size = 1.5, colour = "blue") + 
  xlab("y") + ylab("aposteriorni modus") + ylim(c(0, 1))

### V závislosti na poctu opakování $n$, kdyz pozorujeme jen jeden úspech

df <- data.frame(n = seq(1, 20, by = 1), y = c(1))
df1 <- df %>% mutate(
  a = 1 + y, 
  b = 1 + n - y, 
  E = a / (a + b), 
  Var = a * b / ((a + b)^2 * (a + b + 1)), 
  Mode = (a - 1) / (a + b - 2), 
  y = factor(y), 
  n = factor(n)
)

ggplot(df1, aes(x = n, y = E, group = 1)) + 
  geom_line(size = 0.2, linetype = "dashed", colour = "red") + 
  geom_point(size = 1.5, colour = "red") + 
  xlab("n") + ylab("aposteriorni stredni hodnota") + ylim(c(0, 1))

ggplot(df1, aes(x = n, y = Var, group = 1)) + 
  geom_line(size = 0.2, linetype = "dashed", colour = "darkgreen") + 
  geom_point(size = 1.5, colour = "darkgreen") + 
  xlab("y") + ylab("aposteriorni rozptyl") + ylim(c(0, 0.1))

ggplot(df1, aes(x = n, y = Mode, group = 1)) + 
  geom_line(size = 0.2, linetype = "dashed", colour = "blue") + 
  geom_point(size = 1.5, colour = "blue") + 
  xlab("y") + ylab("aposteriorni modus") + ylim(c(0, 1))


### V závislosti na poctu opakování $n$, kdyz pozorujeme vzdy polovinu úspechu
n = seq(0, 20, by = 1)
y = n/2
df <- data.frame(n = n, y = y)
df1 <- df %>% mutate(
  a = 1 + y, 
  b = 1 + n - y, 
  E = a / (a + b), 
  Var = a * b / ((a + b)^2 * (a + b + 1)), 
  Mode = (a - 1) / (a + b - 2), 
  y = factor(y), 
  n = factor(n)
)

ggplot(df1, aes(x = n, y = E, group = 1)) + 
  geom_line(size = 0.2, linetype = "dashed", colour = "red") + 
  geom_point(size = 1.5, colour = "red") + 
  xlab("n") + ylab("aposteriorni stredni hodnota") + ylim(c(0, 1))

ggplot(df1, aes(x = n, y = Var, group = 1)) + 
  geom_line(size = 0.2, linetype = "dashed", colour = "darkgreen") + 
  geom_point(size = 1.5, colour = "darkgreen") + 
  xlab("y") + ylab("aposteriorni rozptyl") + ylim(c(0, 0.1))

ggplot(df1, aes(x = n, y = Mode, group = 1)) + 
  geom_line(size = 0.2, linetype = "dashed", colour = "blue") + 
  geom_point(size = 1.5, colour = "blue") + 
  xlab("y") + ylab("aposteriorni modus") + ylim(c(0, 1))

# Príklad: Pocet pocet infikovaných osob

n <- 20
n * c(0.05, 0.10, 0.20)
c(dbinom(0, n, prob = 0.05), dbinom(0, n, prob = 0.10), dbinom(0, n, prob = 0.20))


## Volba 1: Rovnomerné apriorní rozdelení 

c(E = (0 + 1) / 2, Var = (1 - 0)^2 / 12)
qunif(c(0.025, 0.975), 0, 1)
c(punif(0.10, 0, 1), punif(0.20, 0, 1) - punif(0.05, 0, 1))

### Aposteriorní rozdelení

df.theta <- data.frame(theta = seq(0, 1, by = 0.001))
n <- 20
y <- 0
posterior <- list(a = 1 + y, b = 1 + n - y)
df.prior <- df.theta %>%  mutate(
  a = NA, 
  b = NA, 
  h = 1,
  model = "prior"
)
df.posterior <- df.theta %>% mutate(
  a = posterior$a, 
  b = posterior$b, 
  h = dbeta(theta, a, b),
  model = "posterior"
)
df1 <- rbind(df.prior, df.posterior)
ggplot(df1, aes(x = theta, y = h, color = model, fill = model)) + 
  geom_area(position = "identity", alpha = 0.2, size = 0) + 
  geom_line(size = 1) + 
  xlab("theta") + 
  ylab("hustota")

a = posterior$a
b = posterior$b


c(E = a/(a + b), Var = a*b/((a+b)**2 * (a+b+1))) #aposteriorní strední hodnota
qbeta(c(0.025, 0.975), a, b) #aposteriorní interval spolehlivosti
c(pbeta(0.10, a, b), pbeta(0.20, a, b) - pbeta(0.05, a, b)) #P(0.05<Theta<0.2)

## Volba 2: Beta apriorní rozdelení 
a = 2
b = 8
prior2 <- list(a = 2, b = 8)
c(E = a/(a + b), Var = a*b/((a+b)**2 * (a+b+1))) #aposteriorní strední hodnota
qbeta(c(0.025, 0.975), a, b) #aposteriorní interval spolehlivosti
c(pbeta(0.10, a, b), pbeta(0.20, a, b) - pbeta(0.05, a, b)) #P(0.05<Theta<0.2)

### Aposteriorní rozdelení

posterior2 <- list(a = prior2$a + y, b = prior2$b + n - y)
n <- 20
y <- 0
df.theta <- data.frame(theta = seq(0, 1, by = 0.001))

df.prior2 <- df.theta %>%  mutate(
  a = a, 
  b = b, 
  h = dbeta(theta, a, b),
  model = "prior2"
)

df.posterior2 <- df.theta %>% mutate(
  a = posterior2$a, 
  b = posterior2$b, 
  h = dbeta(theta, a, b),
  model = "posterior2"
)

df2 <- rbind(df.prior2, df.posterior2)
ggplot(df2, aes(x = theta, y = h, color = model, fill = model)) + 
  geom_area(position = "identity", alpha = 0.2, size = 0) + 
  geom_line(size = 1) + 
  xlab("theta") + 
  ylab("hustota")

## Porovnání 
df <- rbind(df1, df2)
ggplot(df, aes(x = theta, y = h, color = model, fill = model)) + 
  geom_line(size = 1) + 
  xlab("theta") + 
  ylab("hustota")



## Prediktivní rozdelení

df.z <- data.frame(z = c(0, 1))
df.pred.prior <- df.z %>% mutate(
  f = dbinom(z, 1, with(prior2, a / (a + b))), 
  model = "prior"
)
df.pred.posterior <- df.z %>% mutate(
  f = dbinom(z, 1, with(posterior2, a / (a + b))), 
  model = "posterior"
)
df.pred <- rbind(df.pred.prior, df.pred.posterior) %>% mutate(
  z = factor(z))
ggplot(df.pred, aes(x = z, y = f, group = model, color = model)) + 
  geom_line(size = 0.2, lty = 2) + 
  geom_point(size = 2, pch = 19) +
  xlab("predikovany vysledek") +
  ylab("prediktivni pravdepodobnost") + 
  ylim(c(0, 1))
