library("dplyr")
library("ggplot2")

# "studentdata" z knihovny "LearnBayes". Jim Albert. Bayesian Computation with R. 2007.
load("data/studentdata.RData", verbose = TRUE)

X <- studentdata$WakeUp - studentdata$ToSleep # delka spanku
df.X <- data.frame(X = X[!is.na(X)]) # odfiltrujeme NA hodnoty
n <- nrow(df.X) # pocet pozorovani
y <- sum(df.X$X >= 8) # postacujici statistika
data.frame(y = y, n = n, podil = round(y/n, 3))
# Histogram: vhodne upravit pomoci parametru "bins" (pocet), anebo "binwidth" (sirka)
ggplot(df.X, aes(x = X, y = ..density..)) + 
  geom_histogram(color = "black", fill = "#FFCC00", size = 0.5, bins = ceiling(2*n^(1/3))) + 
  labs(x = "delka spanku [h]", y = "hustota pravdepodobnosti")

## 1. Diskrétní apriorní rozdelení


theta <- seq(0.05, 0.95, by = 0.1)
w <- c(2, 4, 8, 8, 4, 2, 1, 1, 1, 1) # vahy
# Apriorni rozdeleni
df.prior <- data.frame(theta = theta, w = w) %>% mutate(
  model = "prior",
  h = w / sum(w), # normalizace na apriorni pravdepodobnosti
  log.l = NA,
  g = NA,
  F = cumsum(h) # distribucni funkce
)
# Aposteriorni rozdeleni
df.posterior <- df.prior %>% mutate(
  model = "posterior", 
  log.l = y * log(theta) + (n - y) * log(1 - theta),  # logaritmicka verohodnost
  g = exp(log.l - max(log.l)) * w, # verohodnost * apriorni vahy
  h = g / sum(g), # normalizace na aposteriorni pravdepodobnosti
  F = cumsum(h) # distribucni funkce
)
df1 <- rbind(df.prior, df.posterior)
ggplot(df1, aes(x = theta, y = h, group = model, color = model)) + 
  geom_line(size = 0.2, lty = 2) + 
  geom_point(size = 2, pch = 19) + 
  labs(x = "theta = P(spanek >= 8 h)", y = "pravdepodobnost") + ylim(c(0, 1)) + 
  annotate("point", x = y/n, y = 0, size = 2, pch = 2)
# Ciselne charakteristiky a odhady
result <- df1 %>% 
  group_by(model) %>% 
  summarise(
    MAP = theta[which.max(h)], 
    E = sum(theta * h),
    SD = sqrt(sum(theta^2 * h) - sum(theta * h)^2), 
    Q2.5 = theta[min(which(F > 0.025))], 
    Q97.5 = theta[min(which(F > 0.975))]
  ) %>% 
  as.data.frame
print(result, digits = 3)
# Prediktivni rozdeleni
df.z <- data.frame(z = seq(0, 1, by = 1))
df.pred.prior <- with(df.prior, 
                      df.z %>% rowwise %>% mutate(
                        f = sum(theta^z * (1-theta)^(1-z) * h), # vypocet predikovane pravdepodobnosti
                        model = "prior"
                      )
)
df.pred.posterior <- with(df.posterior, 
                          df.z %>% rowwise %>% mutate(
                            f = sum(theta^z * (1-theta)^(1-z) * h), # vypocet predikovane pravdepodobnosti
                            model = "posterior"
                          )
)
df.pred <- rbind(df.pred.prior, df.pred.posterior)
ggplot(df.pred, aes(x = z, y = f, group = model, color = model)) + 
  geom_line(size = 0.2, lty = 2) + 
  geom_point(size = 2, pch = 19) +
  labs(x = "z = predikce", y = "pravdepodobnost") + ylim(c(0, 1))
print(df.pred %>% as.data.frame, digits = 3)

### Simulace náhodných výberu

N <- 1e5
sim.prior <- data.frame( # generovani apriornich theta
  theta = sample(df.prior$theta, N, prob = df.prior$h, replace = TRUE) ,
  model = "prior"
)
sim.posterior <- data.frame( # generovani aposteriornich theta
  theta = sample(df.posterior$theta, N, prob = df.posterior$h, replace = TRUE), 
  model = "posterior"
)
sim <- rbind(sim.prior, sim.posterior)
# Sloupcovy graf cetnosti
ggplot(sim, aes(x = factor(theta), fill = model)) + 
  geom_bar(color = "black") + 
  facet_grid(. ~ model) + 
  labs(x = "theta = P(spanek >= 8 h)", y = "pocet")
# Nebo jako bodovy graf relativnich cetnosti (y = ..prop..)
ggplot(sim, aes(x = factor(theta), group = model, color = model)) + 
  geom_line(aes(y = ..prop..), stat = "count", size = 0.3, linetype = 2) + 
  geom_point(aes(y = ..prop..), stat = "count", size = 2, pch = 19) + 
  labs(x = "theta = P(spanek >= 8 h)", y = "pravdepodobnost") + ylim(c(0, 1))
# Ciselne charakteristiky a odhady
result <- sim %>% 
  group_by(model) %>% 
  summarise(
    E = mean(theta), 
    SD = sd(theta), 
    Q2.5 = quantile(theta, probs = 0.025, names = FALSE), 
    Q97.5 = quantile(theta, probs = 0.975, names = FALSE)
  ) %>% as.data.frame
print(result, digits = 3)

sim <- sim %>% mutate(z = rbinom(theta, 1, theta)) # k vygenerovanym theta nahodne generujeme predikce
# Sloupcovy graf cetnosti
ggplot(sim, aes(x = factor(z), fill = model)) + 
  geom_bar(color = "black") + 
  facet_grid(. ~ model) + 
  labs(x = "z = predikce", y = "pocet")
# Nebo jako bodovy graf relativnich cetnosti (y = ..prop..)
ggplot(sim, aes(x = factor(z), group = model, color = model)) + 
  geom_line(aes(y = ..prop..), stat = "count", size = 0.3, linetype = 2) + 
  geom_point(aes(y = ..prop..), stat = "count", size = 2, pch = 19) + 
  labs(x = "z = predikce", y = "pravdepodobnost") + ylim(c(0, 1))
# Predikovane pravdepodobnosti uspechu odhadneme relativnimi cetnostmi
pred <- sim %>% 
  group_by(model) %>% 
  summarise("P(uspech)" = sum(z) / length(z)) %>% 
  as.data.frame
print(pred, digits = 3)

## 2. Apriorní rozdelení zadané histogramem

theta.k <- seq(0.0, 1.0, by = 0.1) # delici body
w <- c(2, 4, 8, 8, 4, 2, 1, 1, 1, 1) # vahy jednotlivych intervalu

d <- diff(theta.k) # delky intervalu
h <- w / sum(w) / d # apriorni hustota na jednotlivych intervalech
# Definice funkce pro vypocet apriorni hustoty
pdf.prior <- stepfun(theta.k, c(0, h, 0))

# Definice funkce pro vypocet aposteriorni  hustoty
pdf.posterior <- function (theta) {
  pdf.prior(theta) * dbeta(theta, y+1, n-y+1) / sum(diff(pbeta(theta.k, y+1, n-y+1)) * h)
}
# Dale pokracujeme klasicky...
df.theta <- data.frame(theta = seq(0, 1, by = 0.001))
df.prior <- df.theta %>% mutate(
  h = pdf.prior(theta),
  model = "prior"
)
df.posterior <- df.theta %>% mutate(
  h = pdf.posterior(theta),
  model = "posterior"
)
df1 <- rbind(df.prior, df.posterior)
ggplot(df1, aes(x = theta, y = h, color = model, fill = model)) + 
  geom_area(position = "identity", alpha = 0.2, size = 0) +
  geom_line(size = 0.5) + 
  labs(x = "theta = P(spanek >= 8 h)", y = "hustota")
### Simulace náhodných výberu


# Generovani nahodneho vyberu z apriorniho rozdeleni zamitaci metodou
N <- 1e5
c <- 2.5
X <- runif(N)
U <- runif(N)
Y <- ifelse(U < pdf.prior(X) / (c * 1), X, NA)
sim.prior <- data.frame( # generovani apriornich theta
  theta = Y[!is.na(Y)],
  model = "prior"
)
# Generovani nahodneho vyberu z aposteriorniho rozdeleni zamitaci metodou
N <- 1e6
c <- 27
X <- runif(N)
U <- runif(N)
Y <- ifelse(U < pdf.posterior(X) / (c * 1), X, NA)
sim.posterior <- data.frame( # generovani apriornich theta
  theta = Y[!is.na(Y)],
  model = "posterior"
)
sim <- rbind(sim.prior, sim.posterior)
# Histogramy obou vygenerovanych vyberu
ggplot(sim, aes(x = theta, y = ..density.., fill = model)) + 
  geom_histogram(color = "black",  size = 0.2, bins = 100) + 
  facet_grid(. ~ model, scales = "free_x") + 
  labs(x = "theta = P(spanek >= 8 h)", y = "hustota")

ggplot(sim, aes(x = theta, color = model, fill = model)) + 
  geom_density(size = 0.5, alpha = 0.2) + 
  labs(x = "theta = P(spanek >= 8 h)", y = "jadrovy odhad hustoty")

result <- sim %>% 
  group_by(model) %>% 
  summarise(
    MAP = theta[which.max(h)], 
    E = mean(theta), 
    SD = sd(theta), 
    Q2.5 = quantile(theta, probs = 0.025, names = FALSE), 
    Q97.5 = quantile(theta, probs = 0.975, names = FALSE)
  ) %>% as.data.frame
print(result, digits = 3)

sim <- sim %>% mutate(z = rbinom(theta, 1, theta)) # k vygenerovanym theta nahodne generujeme predikce
# Sloupcovy graf cetnosti
ggplot(sim, aes(x = factor(z), fill = model)) + 
  geom_bar(color = "black") + 
  facet_grid(. ~ model) + 
  labs(x = "z = predikce", y = "pocet")
# Nebo jako bodovy graf relativnich cetnosti (y = ..prop..)
ggplot(sim, aes(x = factor(z), group = model, color = model)) + 
  geom_line(aes(y = ..prop..), stat = "count", size = 0.3, linetype = 2) + 
  geom_point(aes(y = ..prop..), stat = "count", size = 2, pch = 19) + 
  labs(x = "z = predikce", y = "pravdepodobnost") + ylim(c(0, 1))
# Predikovane pravdepodobnosti uspechu odhadneme relativnimi cetnostmi
pred <- sim %>% 
  group_by(model) %>% 
  summarise("P(uspech)" = sum(z) / length(z)) %>% 
  as.data.frame
print(pred, digits = 3)

# 3. Apriorní rozdelení explicitne zadané apriorní hustotou

# Monte Carlo zamitaci metoda integrovani
MC5 <- function(n, r, a, b, c) {
  X <- runif(n, a, b)
  Y <- runif(n, 0, c)
  df <- data.frame(X, Y)
  df$test <- Y <= r(X)
  int <- (b - a) * c * sum(df$test) / n
  df$test <- factor(df$test, labels = c("reject", "accept"))
  return(list(int = int, df = df))
}
K <- MC5(1e6, function (theta) { sin(pi*theta) * exp(-theta) }, 0, 1, 0.65)
# Definice funkce pro vypocet apriorni hustoty
pdf.prior <- function (theta) {
  sin(pi*theta) * exp(-theta) / K$int
}

K1 <- MC5(1e6, function (theta) { pdf.prior(theta) * dbeta(theta, y+1, n-y+1) }, 0, 1, 35)
# Definice funkce pro vypocet aposteriorni hustoty
pdf.posterior <- function (theta) {
  pdf.prior(theta) * dbeta(theta, y+1, n-y+1) / K1$int
}

df.theta <- data.frame(theta = seq(0, 1, by = 0.001))
df.prior <- df.theta %>% mutate(
  h = pdf.prior(theta),
  model = "prior"
)
df.posterior <- df.theta %>% mutate(
  h = pdf.posterior(theta),
  model = "posterior"
)
df1 <- rbind(df.prior, df.posterior)
ggplot(df1, aes(x = theta, y = h, color = model, fill = model)) + 
  geom_area(position = "identity", alpha = 0.2, size = 0) +
  geom_line(size = 0.5) + 
  labs(x = "theta = P(spanek >= 8 h)", y = "hustota")

# Generovani nahodneho vyberu z apriorniho rozdeleni zamitaci metodou
N <- 1e5
c <- 2.5
X <- runif(N)
U <- runif(N)
Y <- ifelse(U < pdf.prior(X) / (c * 1), X, NA)
sim.prior <- data.frame( # generovani apriornich theta
  theta = Y[!is.na(Y)],
  model = "prior"
)
# Generovani nahodneho vyberu z aposteriorniho rozdeleni zamitaci metodou
N <- 1e6
c <- 23
X <- runif(N)
U <- runif(N)
Y <- ifelse(U < pdf.posterior(X) / (c * 1), X, NA)
sim.posterior <- data.frame( # generovani apriornich theta
  theta = Y[!is.na(Y)],
  model = "posterior"
)
sim <- rbind(sim.prior, sim.posterior)
# Histogramy obou vygenerovanych vyberu
ggplot(sim, aes(x = theta, y = ..density.., fill = model)) + 
  geom_histogram(color = "black",  size = 0.2, bins = 100) + 
  facet_grid(. ~ model, scales = "free_x") + 
  labs(x = "theta = P(spanek >= 8 h)", y = "hustota")
ggplot(sim, aes(x = theta, color = model, fill = model)) + 
  geom_density(size = 0.5, alpha = 0.2) + 
  labs(x = "theta = P(spanek >= 8 h)", y = "jadrovy odhad hustoty")

result <- sim %>% 
  group_by(model) %>% 
  summarise(
    MAP = theta[which.max(h)], 
    E = mean(theta), 
    SD = sd(theta), 
    Q2.5 = quantile(theta, probs = 0.025, names = FALSE), 
    Q97.5 = quantile(theta, probs = 0.975, names = FALSE)
  ) %>% as.data.frame
print(result, digits = 3)

sim <- sim %>% mutate(z = rbinom(theta, 1, theta)) # k vygenerovanym theta nahodne generujeme predikce
# Sloupcovy graf cetnosti
# ggplot(sim, aes(x = factor(z), fill = model)) + 
# 	geom_bar(color = "black") + 
# 	facet_grid(. ~ model) + 
# 	labs(x = "z = predikce", y = "pocet")
# Nebo jako bodovy graf relativnich cetnosti (y = ..prop..)
ggplot(sim, aes(x = factor(z), group = model, color = model)) + 
  geom_line(aes(y = ..prop..), stat = "count", size = 0.3, linetype = 2) + 
  geom_point(aes(y = ..prop..), stat = "count", size = 2, pch = 19) + 
  labs(x = "z = predikce", y = "pravdepodobnost") + ylim(c(0, 1))
# Predikovane pravdepodobnosti uspechu odhadneme relativnimi cetnostmi
pred <- sim %>% 
  group_by(model) %>% 
  summarise("P(uspech)" = sum(z) / length(z)) %>% 
  as.data.frame
print(pred, digits = 3)
