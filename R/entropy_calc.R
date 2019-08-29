
#' @export
entropy_calc <- function(x){
  C_hat <- 1 - length(nonduplicated(x)) / length(x)
  p_hat <- as.numeric(prop.table(table(x)))
  p_tilde <- C_hat * p_hat
  entropy <- - sum( (p_tilde * log(p_tilde)) / (1 - (1 - p_tilde)^length(x)) )

  return(entropy)
}

nonduplicated <- function(x){
  output <- x[ave(x, x, FUN = length) == 1]
  return(output)
}

#' @export
entropy_calc_matrix <- function(x){

  n_x <- dim(x)[1]

  nk <- aggregate(list(numdup=rep(1,nrow(x))), as.data.frame(x), length)$numdup
  unique_x <- length(which(nk == 1))

  p_hat <- nk / n_x

  C_hat <- 1 - unique_x / n_x
  p_tilde <- C_hat * p_hat
  entropy <- - sum( (p_tilde * log(p_tilde)) / (1 - (1 - p_tilde)^n_x) )

  return(entropy)
}
