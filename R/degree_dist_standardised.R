
#' Degree distribution standardised
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
ldouble_factorial <- function(n){

  even <- n %% 2 == 0

  stopifnot(is.logical(even))

  if(even){
    k <- n/2
    output <- k * log(2) + lfactorial(k)
  } else {
    k <- (n+1)/2
    output <- lfactorial(2*k) - k * log(2) - lfactorial(k)
  }

  return(output)
}

#' @export
number_of_graphs_dd <- function(x, sorted = TRUE, bigz = TRUE, mirror = FALSE, type = "Bianconi"){

  if(!igraph::is_graphical(x)){
    return(rep(-Inf, length(type)))
  }

  if(all(x == 0)){
    return(rep(-Inf, length(type)))
  }

  M <- sum(x)
  L <- M/2
  n <- length(x)

  if(all(x == (n-1))){
    return(rep(0, length(type)))
  }

  if(mirror){
    if(mean(x) >= (n-1)/2){
      x = n - x - 1
    }
  }

  output <- NULL

  if("Bianconi" %in% type){
    output <- ldouble_factorial(mean(x)*n) +
      (-0.25 * (mean(x ^ 2)/mean(x))^2) -
      sum(lfactorial(x))
  }

  if("Liebenau" %in% type){
    gamma2 <- var(x)/(n - 1)
    mud = mean(x)/(n - 1)
    output2 <- 0.5 * log(2) + 1/4 - gamma2^2 / (4 * mud^2 * (1 - mud)^2) + (n * (n - 1)/2) *( mud * log(mud) + (1 - mud)*log(1 - mud)) + sum(lchoose(n - 1, x))
    output <- c(output, output2)
  }

  if("Exact" %in% type){
    stopifnot(length(x) == 4)
    sort_x <- sort(x)
    output <- StartNetwork::vertex4 %>% dplyr::filter(digest == digest::digest(sort_x)) %>% dplyr::pull(combinations)
    sorted <- FALSE
    if(length(output) == 0){output = 0}
    output <- log(output)
  }

  if(sorted){
    size_factor <- arrangements::npermutations(k = length(x), freq = as.numeric(table(x)), bigz = bigz)
    output <- output + gmp::log.bigz(size_factor)
  }


  return(output)
}
