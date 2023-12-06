library(outbreaks)
library(incidence)
library(ggplot2)

linelist <- ebola_sim_clean$linelist ##### Do the same
i <- incidence(linelist$date_of_onset)

test <-  read_csv("~/Downloads/test_covid.csv")
i <- incidence(linelist$date_of_onset)
plot(i) +
  theme_bw() # full outbreak

plot(i[1:160]) +
  theme_bw() # first 160 days

library(distcrete)
library(epitrix)
mu <- 15.3
sigma <- 9.3
cv <- sigma / mu
params <- gamma_mucv2shapescale(mu, cv)
si <- distcrete("gamma", shape = params$shape,
                scale = params$scale,
                interval = 1, w = 0.5)
si
si_df <- data.frame(t = 1:50,
                    p = si$d(1:50))
ggplot(si_df, aes(x = t, y = p)) +
  theme_bw() +
  geom_col() +
  labs(title = "Serial interval",
       x = "Days after onset",
       y = "Probability")

library(projections)
set.seed(1)
pred_1 <- project(i[1:100], R = 1.5, si = si, n_days = 30, n_sim = 1000)
pred_1

