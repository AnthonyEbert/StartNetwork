
#' @export
er_KL <- function(p, nl = 10, pl = 0.1, replicates = 1000, entropy_cheat = FALSE, entropy_return = FALSE, ...){

  ne <- choose(nl,2)

  y <- replicate(replicates, er_edges(nl, p, ...))
  y1 <- mean(y)
  y2 <- mean(log(choose(ne, y)))
  if(entropy_cheat){
    entropy <- mean(-log(dbinom(y, size = ne, p)))
  } else {
    C_hat <- 1 - length(unique) / replicates
    p_hat <- as.numeric(prop.table(table(y)))
    p_tilde <- C_hat * p_hat
    entropy <- - sum( (p_tilde * log(p_tilde)) / (1 - (1 - p_tilde)^replicates))
  }

  KL_div <- KL_calc(nl, pl, y1, y2, ne, entropy)

  if(is.nan(KL_div)){print(y)}

  if(entropy_return){
    output <- entropy
  } else {
    output <- KL_div
  }

  return(output)
}

KL_calc <- function(nl, pl, y1, y2, ne, entropy){

  output <- - ne * log(1 - pl) - log(pl/(1-pl)) * y1 -
    y2 -
    entropy

  return(output)

}


