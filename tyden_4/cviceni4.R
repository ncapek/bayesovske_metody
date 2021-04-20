library("dplyr")
library("ggplot2")

load(file = "data/deti1.RData", verbose = TRUE)

str(Y)
Y1 <- Y[Y$FEMALE == 1 & Y$YEAR >= 1990 & Y$AGE == 40 & Y$DEG < 3, "CHILDS"]
Y2 <- Y[Y$FEMALE == 1 & Y$YEAR >= 1990 & Y$AGE == 40 & Y$DEG >= 3, "CHILDS"]
Y1 <- Y1[!is.na(Y1)]
Y2 <- Y2[!is.na(Y2)]

n1 <- length(Y1)
y1 <- sum(Y1)
n2 <- length(Y2)
y2 <- sum(Y2)
Tab1 <- as.data.frame(table(Y = Y1))
Tab2 <- as.data.frame(table(Y = Y2))
Tab1$vzdelani <- "<Bc"
Tab2$vzdelani <- "Bc+"
Tab <- rbind(Tab1, Tab2) %>% mutate(vzdelani = factor(vzdelani))
ggplot(Tab, aes(x = Y, y = Freq, fill = vzdelani)) +
  geom_col(color = "black", size = 0.2, position = position_dodge(preserve = "single")) +
  scale_x_discrete(drop = FALSE) +
  xlab("pocet deti") +
  ylab("cetnosti")

prior <- list(a = 2, b = 1)
posterior1 <- list(a = prior$a + y1, b = prior$b + n1)
posterior2 <- list(a = prior$a + y2, b = prior$b + n2)
df.theta <- data.frame(theta = seq(0, 4, by = 0.01))
df.prior <- df.theta %>% mutate(
  a = prior$a, 
  b = prior$b, 
  h = dgamma(theta, a, b),
  model = "prior"
)
df.posterior1 <- df.theta %>% mutate(
  a = posterior1$a, 
  b = posterior1$b, 
  h = dgamma(theta, a, b),
  model = "post. <Bc"
)
df.posterior2 <- df.theta %>% mutate(
  a = posterior2$a, 
  b = posterior2$b, 
  h = dgamma(theta, a, b),
  model = "post. Bc+"
)
df1 <- rbind(df.prior, df.posterior1, df.posterior2)
ggplot(df1, aes(x = theta, y = h, color = model, fill = model)) + 
  #  geom_area(position = "identity", alpha = 0.2, size = 0) + 
  geom_line(size = 1) + 
  xlab("theta") + ylab("hustota")
result <- with(prior, data.frame(
  model = "prior", 
  est = NA, 
  MAP = NA, 
  E = a / b,
  SD = sqrt(a) / b, 
  Q2.5 = qgamma(0.025, a, b), 
  Q97.5 = qgamma(0.975, a, b)
))
result1 <- with(posterior1, data.frame(
  model = "post. <Bc", 
  est = y1 / n1, 
  MAP = (a - 1) / b,
  E = a / b,
  SD = sqrt(a) / b, 
  Q2.5 = qgamma(0.025, a, b), 
  Q97.5 = qgamma(0.975, a, b)
))
result2 <- with(posterior2, data.frame(
  model = "post. Bc+", 
  est = y2 / n2, 
  MAP = (a - 1) / b,
  E = a / b,
  SD = sqrt(a) / b, 
  Q2.5 = qgamma(0.025, a, b), 
  Q97.5 = qgamma(0.975, a, b)
))
print(rbind(result, result1, result2))

df.z <- data.frame(z = seq(0, 10, by = 1))
df.pred.prior <- with(prior, df.z %>% mutate(
  f = dnbinom(z, a, b / (b + 1)), 
  model = "prior"
))
df.pred.posterior1 <- with(posterior1, df.z %>% mutate(
  f = dnbinom(z, a, b / (b + 1)), 
  model = "post. <Bc"
))
df.pred.posterior2 <- with(posterior2, df.z %>% mutate(
  f = dnbinom(z, a, b / (b + 1)), 
  model = "post. Bc+"
))
df.pred <- rbind(df.pred.prior, df.pred.posterior1, df.pred.posterior2) %>% mutate(
  z = factor(z)
)
ggplot(df.pred, aes(x = z, y = f, group = model, color = model)) + 
  geom_line(size = 0.2, lty = 2) + 
  geom_point(size = 2, pch = 19) +
  xlab("predikovany pocet deti") +
  ylab("pravdepodobnost")
pred <- with(prior, data.frame(
  model = "prior", 
  E = a / b,
  SD = sqrt(a * (b + 1)) / b, 
  Q2.5 = qnbinom(0.025, a, b / (b + 1)), 
  Q97.5 = qnbinom(0.975, a, b / (b + 1))
))
pred1 <- with(posterior1, data.frame(
  model = "post. <Bc", 
  E = a / b,
  SD = sqrt(a * (b + 1)) / b, 
  Q2.5 = qnbinom(0.025, a, b / (b + 1)), 
  Q97.5 = qnbinom(0.975, a, b / (b + 1))
))
pred2 <- with(posterior2, data.frame(
  model = "post. Bc+", 
  E = a / b,
  SD = sqrt(a * (b + 1)) / b, 
  Q2.5 = qnbinom(0.025, a, b / (b + 1)), 
  Q97.5 = qnbinom(0.975, a, b / (b + 1))
))
print(rbind(pred, pred1, pred2))