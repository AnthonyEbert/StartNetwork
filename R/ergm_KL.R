
#' @export
ergm_KL <- function(eta, n = 10, etap = 0.05, replicates = 20, include_entropy = TRUE, coef1 = 0, coef1p = 0, entropy_coef = 1, both = FALSE, ...){

  x <- t(sapply(1:replicates, ergm_ss, coef = c(coef1, eta), n = n))
  entropy <- entropy_calc_matrix(x)

  ll <- mean(apply(x, 1, ergm_lik, coef1 = coef1p, eta = etap, n = n))

  output <- -ll - entropy_coef * entropy
  if(both){
    output <- c(-ll, -entropy)
  }

  return(output)

}

#' @import ergm
#' @import network
ergm_ss <- function(dummyx, coef, n, ...){
  x <- simulate(network(n) ~ edges + triangle, coef = coef, directed = FALSE, ...)
  out <- as.numeric(summary(x ~ edges + triangle))
  return(out)
}

ergm_lik <- function(x, coef1, eta, n){
  log(dbinom(x[1], n * (n - 1), prob = 0.5)) +
    c(coef1, eta) %*% x
}
