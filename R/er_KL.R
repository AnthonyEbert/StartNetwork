
#' @export
er_KL <- function(p, nl = 10, pl = 0.1){
  g <- sapply(rep(nl, 10), igraph::sample_gnp, p = p, directed = FALSE, loops = FALSE)

  y <- igraph::gsize(g)

  KL_div <- - nl - log(1 - pl) - log(pl/(1-pl)) * y -
    log(factorial(nl)/(factorial(y) * factorial(nl - y))) +
    0.5 * log2(2*pi*exp(1)*nl*p*(1-p))

  if(is.nan(KL_div)){print(y)}

  return(KL_div)
}

KL_calc <- function(nl, pl, y, p){

  output <- - nl - log(1 - pl) - log(pl/(1-pl)) * y -
    log(factorial(nl)/(factorial(y) * factorial(nl - y))) +
    0.5 * log2(2*pi*exp(1)*nl*p*(1-p))

  return(output)

}


