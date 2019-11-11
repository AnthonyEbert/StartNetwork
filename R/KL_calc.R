
KL_calc <- function(nl, pl, y1, y2, ne, entropy){

  output <- - ne * log(1 - pl) - log(pl/(1-pl)) * y1 -
    y2

  return(output)

}

KL_calc2 <- function(pl, y1, y2, entropy){

  output <- - sum(pl * y1) -
    y2 - entropy

  return(output)

}


