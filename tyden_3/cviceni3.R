## Analýza citlivosti aposterironí strední hodnoty

df <- expand.grid(
  mean.prior = seq(0, 1, by = 0.01), 
  belief = seq(0, 100, by = 0.1),
  y = y, 
  n = n,
  KEEP.OUT.ATTRS = FALSE
)
df1 <- df %>% mutate(
  mean.posterior = (belief * mean.prior + y) / (belief + n)
)
mean.prior <- with(prior2, a / (a + b))
belief <-  with(prior2, (a + b))
# vrstevnicovy graf s barevnymi vrstevnicemi
ggplot(df1, aes(x = belief, y = mean.prior, z = mean.posterior)) + 
  geom_contour(binwidth = 0.05, aes(colour = ..level..)) + 
  labs(color = "apost.\nstredni\nhodnota", x = "belief", y = "apriorni stredni hodnota") +
  ggtitle("Aposteriorni stredni hodnota") + 
  geom_point(x = belief, y = mean.prior, color = "red", show.legend = FALSE)
# vrstevnicovy graf s barevnoy vyplni
ggplot(df1, aes(x = belief, y = mean.prior, z = mean.posterior)) + 
  geom_contour_filled(binwidth = 0.10) + 
  geom_contour(binwidth = 0.05, colour = "black", size = 0.1) + 
  labs(fill = "apost.\nstredni\nhodnota", x = "belief", y = "apriorni stredni hodnota") +
  ggtitle("Aposteriorni stredni hodnota") + 
  geom_point(x = belief, y = mean.prior, color = "red", show.legend = FALSE)
