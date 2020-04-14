
library(dplyr)
library(StartNetwork)
library(ggplot2)

results_df <- data.frame(
  label = c(
    rep("ergm package", length(ergm_test)),
    rep("proposed method", length(sn_test0))
  ),
  value = c(ergm_test, sn_test0)
)

vertex4a <- StartNetwork::vertex4 %>%
  rowwise() %>%
  mutate(statistic = gwdegree(degseqx, 2), lik = combinations * exp(0.1 * statistic)) %>%
  ungroup() %>%
  mutate(lik = lik/sum(lik))

ggplot(results_df, aes(x = value, col = label)) +
  geom_histogram(
    aes(y = 0.05 * stat(density)),
    position = "identity", stat = "bin", binwidth = 0.05, alpha = 0
  ) +
  geom_point(data = vertex4a, aes(x = statistic, col = "truth", y = lik)) +
  xlab("Summary statistic") +
  ylab("Probability")

