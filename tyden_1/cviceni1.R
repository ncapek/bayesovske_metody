library("dplyr")
library("ggplot2")

Y <- c(0, 0, 1, 0, 0, 1, 0, 1, 0, 0)
n <- length(Y)

df <- data.frame(Y = Y)                          # vytvoreni tabulky
df$Y <- factor(df$Y, levels = c(0, 1))           # faktor
df <- df %>% mutate(                             # nebo pomoci dplyr
  Y = factor(Y, levels = c(0, 1))
)

df %>% str
df %>% summary
df %>% names
df %>% head
df %>% as_tibble
df %>% View

ggplot(df, aes(x = Y)) +                         # zakladni prikaz ggplot2 grafiky
  geom_bar(fill = "orange", color = "black") +   # sloupcovy graf cetnosti
  scale_x_discrete(drop = FALSE) +               # tato volba zobrazi i urovne s nulovymi cetnostmi
  xlab("vysledky pokusu") +                      # popisek vodorovne osy
  ylab("pocty")                                  # popisek svisle osy

median(Y)
mean(Y)
var(Y)
sd(Y)

dbinom(3, 10, 0.5)


n <- 10
y <- seq(0, n, by = 1)
theta <- 0.5
df <- data.frame(y = y, n = n, theta = theta)
df

df2 <- df %>% mutate(                    # dopocitame pravdepodobnostni funkci
  f = dbinom(y, n, theta)
)
sum(df2$f)


ggplot(df2, aes(x = y, y = f, group = 1)) + 
  geom_line(linetype = "dashed", size = 0.2) + # kresleni car, krivek
  geom_point(size = 1.5) +										 # kresleni bodu
  xlab("pocet licu") + ylab("pravdepodobnost")

# Ukol
# Spo????tejte klasick?? bodov?? odhad parametru ?? pro n???? vektor pozorov??n?? Y a vykreslete graf odpov??daj??c?? pravd??podobnostn??
# funkce.
bodovy_odhad = mean(Y)
n <- 10
y <- seq(0, n, by = 1)
df <- data.frame(y = y, n = n, theta = bodovy_odhad)
df

df2 <- df %>% mutate(                    # dopocitame pravdepodobnostni funkci
  f = dbinom(y, n, bodovy_odhad)
)
sum(df2$f)

ggplot(df2, aes(x = y, y = f, group = 1)) + 
  geom_line(linetype = "dashed", size = 0.2) + # kresleni car, krivek
  geom_point(size = 1.5) +										 # kresleni bodu
  xlab("pocet licu") + ylab("pravdepodobnost")



n <- 10
df <- expand.grid(
  n = n, 
  y = seq(0, n, by = 1), 
  theta = c(0.05, 0.25, 0.5, 0.75, 0.95), 
  KEEP.OUT.ATTRS = FALSE
)

df2 <- df %>% mutate(
  f = dbinom(y, n, theta), 
  theta = factor(theta)
)

df2

ggplot(df2, aes(x = y, y = f, colour = theta)) + 
  geom_line(linetype = "dashed", size = 0.2) + 
  geom_point(size = 1.5) + 
  xlab("pocet licu") + ylab("pravdepodobnost")

ggplot(df2, aes(x = y, y = f, colour = theta)) + 
  geom_line(linetype = "dashed", size = 0.2) + 
  geom_point(size = 1.5) + 
  facet_wrap(theta ~ ., ncol = 2, scales = "free_y") + 
  xlab("pocet licu") + ylab("pravdepodobnost")

## Bayesovsk?? pohled

### Apriorn?? rozd??len??

theta <- seq(0, 1, by = 0.001)
df <- data.frame(theta = theta)
df2 <- df %>% mutate(
  h = dunif(theta, 0, 1)
)
ggplot(df2, aes(x = theta, y = h, group = 1)) + 
  geom_area(position = "identity", fill = "black", alpha = 0.2, size = 0) + # stinovani plochy pod krivkou
  geom_line(size = 1, color = "black") + 
  xlab("theta") + ylab("apriorni hustota") + 
  ylim(0, 1.5)

### Aposteriorn?? rozd??len??

Y <- c(0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1)
n <- length(Y)
y <- sum(Y)
theta <- seq(0, 1, by = 0.001)
df <- data.frame(n = n, y = y, theta = theta)
df3 <- df %>% mutate(
  h = dbeta(theta, 1 + y, 1 + n - y)
)
ggplot(df3, aes(x = theta, y = h, group = 1)) + 
  geom_area(position = "identity", fill = "red", alpha = 0.2, size = 0) + 
  geom_line(size = 1, color = "red") + 
  xlab("theta") + ylab("aposteriorni hustota")

df2 <- df2 %>% mutate(
  n = NA, 
  y = "prior"
)

df23 <- rbind(df2, df3) %>% mutate(
  y = factor(y)
)

ggplot(df23, aes(x = theta, y = h, color = y, fill = y)) + 
  geom_area(position = "identity", alpha = 0.2, size = 0) + 
  geom_line(size = 1) + 
  xlab("theta") + 
  ylab("aposteriorni (apriorni) hustota")

### ??kol

Y1 <- c(1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0)
Y2 <- c(0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1)

theta <- seq(0, 1, by = 0.001)
df <- data.frame(theta = theta)
df2 <- df %>% mutate(
  h = dunif(theta, 0, 1)
)
ggplot(df2, aes(x = theta, y = h, group = 1)) + 
  geom_area(position = "identity", fill = "black", alpha = 0.2, size = 0) + # stinovani plochy pod krivkou
  geom_line(size = 1, color = "black") + 
  xlab("theta") + ylab("apriorni hustota") + 
  ylim(0, 1.5)

### Aposteriorn?? rozd??len??

n1 <- length(Y1)
y1 <- sum(Y1)
theta <- seq(0, 1, by = 0.001)
df <- data.frame(n = n1, y = y1, theta = theta)
df1 <- df %>% mutate(
  h = dbeta(theta, 1 + y1, 1 + n - y1)
)
ggplot(df1, aes(x = theta, y = h, group = 1)) + 
  geom_area(position = "identity", fill = "red", alpha = 0.2, size = 0) + 
  geom_line(size = 1, color = "red") + 
  xlab("theta") + ylab("aposteriorni hustota")

n2 <- length(Y2)
y2 <- sum(Y2)
theta <- seq(0, 1, by = 0.001)
df <- data.frame(n = n2, y = y2, theta = theta)
df2 <- df %>% mutate(
  h = dbeta(theta, 1 + y2, 1 + n - y2)
)
ggplot(df2, aes(x = theta, y = h, group = 1)) + 
  geom_area(position = "identity", fill = "red", alpha = 0.2, size = 0) + 
  geom_line(size = 1, color = "red") + 
  xlab("theta") + ylab("aposteriorni hustota")

n3 <- 250
y3 <- 1
theta <- seq(0, 1, by = 0.001)
df <- data.frame(n = n3, y = y3, theta = theta)
df3 <- df %>% mutate(
  h = dunif(theta, 0, 1)
)

df3

df123 <- rbind(df1, df2, df3) %>% mutate(
  y = factor(y)
)

ggplot(df123, aes(x = theta, y = h, color = y, fill = y)) + 
  geom_area(position = "identity", alpha = 0.2, size = 0) + 
  geom_line(size = 1) + 
  xlab("theta") + 
  ylab("aposteriorni (apriorni) hustota")
