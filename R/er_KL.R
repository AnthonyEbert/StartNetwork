
#' @export
er_KL <- function(p, nl = 10, pl = 0.1){

  y <- mean(replicate(50, er_edges(nl, p)))

  KL_div <- KL_calc(nl, pl, y, p)

  if(is.nan(KL_div)){print(y)}

  return(KL_div)
}

KL_calc <- function(nl, pl, y, p){

  ne <- factorial(nl) / (2 * factorial(nl - 2))

  output <- - ne * log(1 - pl) - log(pl/(1-pl)) * y -
    log(factorial(ne)/(factorial(y) * factorial(ne - y))) +
    0.5 * log(2*pi*exp(1)*nl*p*(1-p))

  return(output)

}


