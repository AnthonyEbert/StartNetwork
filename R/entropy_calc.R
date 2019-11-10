
#' @export
entropy_calc <- function(x, naive = FALSE, hash = FALSE){
  if(hash){
    x <- sapply(x, digest::digest)
  }

  C_hat <- 1 - sum(nonduplicated(x)) / length(x)
  if(naive){
    C_hat <- 1
  }
  p_hat <- as.numeric(prop.table(table(x)))
  p_tilde <- C_hat * p_hat
  entropy <- - sum( (p_tilde * log(p_tilde)) / (1 - (1 - p_tilde)^length(x)) )

  return(entropy)
}

#' @export
nonduplicated <- function(x){

  return(!(duplicated(x) | duplicated(x, fromLast = TRUE)))
}
