
#' @export
entropy_calc <- function(x){
  C_hat <- 1 - length(unique(x)) / length(x)
  p_hat <- as.numeric(prop.table(table(x)))
  p_tilde <- C_hat * p_hat
  entropy <- - sum( (p_tilde * log(p_tilde)) / (1 - (1 - p_tilde)^length(x)) )

  return(entropy)
}

#' @export
entropy_calc_matrix <- function(x){

  n_x <- dim(x)[1]
  unique_x <- dim(mgcv::uniquecombs(x))[1]
  p_hat <- aggregate(list(numdup=rep(1,nrow(x))), as.data.frame(x), length)$numdup / n_x

  C_hat <- 1 - unique_x / n_x
  p_tilde <- C_hat * p_hat
  entropy <- - sum( (p_tilde * log(p_tilde)) / (1 - (1 - p_tilde)^n_x) )

  return(entropy)
}
