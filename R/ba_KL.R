
#' @export
ba_KL <- function(m, nl = 10, ml = 3, replicates = 1000, entropy_cheat = FALSE, entropy_return = FALSE, ...){

  y <- t(sapply(rep(nl, replicates), FUN = degree_pa_sampler, m = m))

  #log_lik <- mean( apply(y, 1, ba_lik2, p = pl) )


  log_lik <- mean(log(apply(y, 1, ba_lik, m = ml)))

  entropy <- entropy_calc_matrix(y)

  return(-log_lik - entropy)
}

#' @export
degree_pa_sampler <- function(nl, m){

  degree(sample_pa(nl, m = m, zero.appeal = 0, directed = FALSE))
}

#' @export
ba_lik <- function(y, p){
  (p - 1) * gamma(p) * prod(gamma(y) / gamma(y + p))
}

#' @export
ba_lik2 <- function(y, p){
  sum(lgamma(y)) - sum(lgamma(y + p))
}
