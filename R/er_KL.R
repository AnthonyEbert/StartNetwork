
#' @export
er_KL <- function(p, nl = 10, pl = 0.1, replicates = 1000, ...){

  ne <- choose(nl,2)

  y <- replicate(replicates, er_edges(nl, p, ...))
  y1 <- mean(y)
  y2 <- mean(log(choose(ne, y)))
  entropy <- mean(-log(dbinom(y, size = ne, p)))

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


