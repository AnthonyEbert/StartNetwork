
#' @export
er_KL <- function(p, nl = 10, pl = 0.1, replicates = 1000, entropy_sim = TRUE, ...){

  ne <- choose(nl,2)

  y <- replicate(replicates, er_edges(nl, p, ...))
  y1 <- mean(y)
  y2 <- mean(log(choose(ne, y)))
  if(entropy_sim){
    entropy <- mean(-log(dbinom(y, size = ne, p)))
  } else {
    entropy <- 0.5 * log2(2*pi*exp(1)*ne*p*(1-p))
  }

  KL_div <- KL_calc(nl, pl, y1, y2, ne, entropy)

  if(is.nan(KL_div)){print(y)}

  return(KL_div)
}

KL_calc <- function(nl, pl, y1, y2, ne, entropy){

  output <- - ne * log(1 - pl) - log(pl/(1-pl)) * y1 -
    y2 -
    entropy

  return(output)

}


