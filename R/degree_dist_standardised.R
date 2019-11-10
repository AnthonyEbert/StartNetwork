
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
number_of_graphs_dd <- function(x, sorted = TRUE, bigz = TRUE, type = 0){

  if(!igraph::is_graphical(x)){
    return(0)
  }

  if(all(x == 0)){
    return(1)
  }



  M <- sum(x)
  L <- M/2
  n <- length(x)

  if(all(x == (n-1))){
    return(1)
  }

  if(type == 0){
    output <- double_factorial(2*L - 1) *
      exp(-0.25 * (mean(x^2)/mean(x))^2) /
      prod(factorial(x))
  } else if(type == "log"){
    output <- log(double_factorial(2*L - 1)) +
      (-0.25 * (mean(x^2)/mean(x))^2) -
      sum(lfactorial(x))
  } else if(type == 1){
    output <- double_factorial(mean(x)*n) *
      exp(-0.25 * (mean(x^2)/mean(x))^2) /
      prod(factorial(x))
  } else if(type == 2){
    kbar <- mean(x)
    sigma2 <- mean((x/(kbar - 1))^2)
    sigma3 <- mean((x/(kbar - 1))^3)
    v2 <- 1 + sigma2
    v3 <- 1 + 3 * sigma2 + sigma3

    output <-
      factorial(M) / (factorial(M/2) * 2^(M/2) * prod(factorial(x))) *
        exp(- (kbar^2 * v2^2 - 1)/4 - (kbar^3 * (5 + 2 * v3 + 6 * v2^2 - 12 * v2))/(12*n))
  } else if(type == 3){
    kbar <- mean(x)
    v2 <- mean(x^2) / (kbar^2 * n)

    lambda <- kbar / (n - 1)
    gamma2 <- n * kbar^2 * (v2 - 1) / ((n-1)^2)

    output <- sqrt(2) * (lambda^lambda * (1 - lambda)^(1 - lambda))^(choose(n,2)) *
      prod(choose(n-1,x) * exp(1/4 - gamma2^2 / (4*lambda^2 * (1-lambda)^2)))
  } else if(type == 4){
    kbar <- mean(x)

    lambda <- kbar / (n - 1)
    gamma2 <- (n - 1)^(-2) * sum((x - kbar)^2)

    GtildeND <- (lambda^lambda * (1 - lambda)^(1 - lambda))^(choose(n,2)) *
      prod(choose(n-1,x))

    output <- sqrt(2) * exp(1/4 - gamma2^2 / (4*lambda^2 * (1-lambda)^2)) * GtildeND
  }

  if(sorted){
    size_factor <- as.numeric(arrangements::npermutations(k = length(x), freq = as.numeric(table(x)), bigz = bigz))
    if(type != "log"){
      output <- output * size_factor
    } else {
      output <- output + log(size_factor)
    }
  }


  return(output)
}

#' @export
number_triangle_graphs <- function(x,n){

  stopifnot(x >= 0)
  stopifnot(x %% 1 == 0)

  xvec <- 0:I(x-1)

  output <- prod(choose(n,3) - xvec)/factorial(x)

  return(output)
}

#' @export
lnumber_triangle_graphs <- function(x,n){

  stopifnot(x >= 0)
  stopifnot(x %% 1 == 0)

  xvec <- 0:I(x-1)

  z <- polyroot(z = c(-x*factorial(3), 2, -3, 1))
  z1 <- which.min(abs(Im(z)))
  z <- max(3, n - ceiling(Re(z[z1])))

  output <- x*lchoose(n,3) - lfactorial(x) + sum(lchoose(choose(z,2), 0:choose(z,2)))

  return(output)
}

#' @export
lnumber_triangle_graphsB <- function(x, n){

  stopifnot(x >= 0)
  stopifnot(x %% 1 == 0)

  xvec <- 0:I(x-1)

  output <- sum(lchoose(n - 2*xvec,3) )

  return(output)

}

