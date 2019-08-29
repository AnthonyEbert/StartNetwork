
#' @export
ba_KL <- function(m, nl = 10000, ml = 3, replicates = 1000, samples = 100, entropy_cheat = FALSE, entropy_return = FALSE, ...){

  g <- igraph::graph(edges = c(1, 2, 1, 3), directed = FALSE)

  y <- sample(degree_pa_sampler(nl, m, g), samples)
  log_lik <- log(ba_lik(y, m = ml))
  entropy <- entropy_calc(y)

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
  prod(2*m*(m+1) / (y * (y + 1) * (y + 2)))
}

#' @export
ba_lik2 <- function(y, p){
  sum(lgamma(y)) - sum(lgamma(y + p))
}
