
#' @export
degree_dist_standardised <- function(g, maxk = 1000){
  output <- rep(0, maxk)
  x <- igraph::degree_distribution(g)
  output[1:length(x)] <- x
  return(output)
}

#' @export
double_factorial <- function(n){

  even <- n %% 2 == 0

  stopifnot(is.logical(even))

  if(even){
    k <- n/2
    output <- (2^k) * factorial(k)
  } else {
    k <- (n+1)/2
    output <- factorial(2*k) / (2^k * factorial(k))
  }

  return(output)
}

#' @export
number_of_graphs_dd <- function(x, sorted = TRUE, bigz = TRUE){

  if(all(x == 0)){
    return(1)
  }

  L <- sum(x)/2

  output <- double_factorial(2*L - 1) *
    exp(-0.25 * (mean(x^2)/mean(x))^2) /
    prod(factorial(x))

  if(sorted){
    size_factor <- as.numeric(arrangements::npermutations(k = length(x), freq = as.numeric(table(x)), bigz = bigz))
    output <- output * size_factor
  }


  return(output)
}





