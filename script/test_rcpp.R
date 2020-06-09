
# test Rcpp

ergmx = ergm::simulate_formula(network::network(100, directed = FALSE) ~ edges, output = "stats", coef = -2, nsim = 10000)
SNx = StartNetwork:::ergm_simulator_cpp(rep(0, 100), 100000, -2)

hist(SNx$stats[seq(4000, 100000, by = 100)])
hist(ergmx[4000:10000])

