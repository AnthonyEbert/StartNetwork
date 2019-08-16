
library(StartNetwork)
library(ggplot2)

x <- sapply(seq(0.025, 0.975, by = 0.025), er_KL, pl = 0.3)

ggplot2::qplot(seq(0.025, 0.975, by = 0.025),x, xlab = "dyad connection probability", ylab = "KL divergence") + ggplot2::geom_vline(aes(xintercept = x), data = data.frame(x = 0.3), col = "red")

