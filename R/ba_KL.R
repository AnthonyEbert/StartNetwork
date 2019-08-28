
#' @export
ba_KL <- function(m, nl = 50, ml = 3, replicates = 1000, entropy_cheat = FALSE, entropy_return = FALSE, ...){

  g <- igraph::graph(edges = c(1, 2, 1, 3), directed = FALSE)

  y <- t(sapply(rep(nl, replicates), FUN = degree_pa_sampler, m = m, g = g))
  y <- t(apply(y, 1, sort))

  #log_lik <- mean( apply(y, 1, ba_lik2, p = pl) )


  log_lik <- mean(log(apply(y, 1, ba_lik, m = ml)))

  entropy <- entropy_calc_matrix(y)

  return(-log_lik - entropy)
}

#' @export
degree_pa_sampler <- function(nl, m, g){

  #output <- rep(0, nl)

  x <- degree(sample_pa(nl, m = m, zero.appeal = 0, directed = FALSE, start.graph = g))

  #output[1:length(x)] <- x

  return(x)


}

#' @export
ba_lik <- function(y, m){
  prod(4 / (y * (y + 1) * (y + 2)))
}

#' @export
ba_lik2 <- function(y, p){
  sum(lgamma(y)) - sum(lgamma(y + p))
}
