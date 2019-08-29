
#' @export
poisson_KL <- function(lambda, lambdaq = 1, replicates = 50, both = FALSE){

  x <- rpois(replicates, lambda)

  output <- c(mean(dpois(x, lambdaq, log = TRUE)), entropy_calc(x))

  return(output)
}


#' @export
nbn_KL <- function(prob, probq = 0.5, replicates = 50, both = FALSE){

  x <- rnbinom(replicates, size = 12, prob)

  output <- c(mean(dnbinom(x, size = 12, probq, log = TRUE)), entropy_calc(x))

  return(output)
}
