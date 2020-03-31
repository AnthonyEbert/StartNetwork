
#' @export
kstar <- function(k, dstat){
  n <- length(dstat) - 1
  output <- sum(choose(k:n, k) * dstat[k:n])
  # output <- sapply(k:n, function(i,k){choose(i,k)*dstat[i]}, k = k)
  output <- sum(output)
}

#' @export
altkstars_sn <- function(x, lambda){
  n <- max(x)
  dstat <- as.numeric(table(factor(x, levels = 0:n)))
  dstat <- dstat / sum(dstat)
  lambda_vec <- (-1/lambda)^(c(0:I(n-3)))
  output <- sapply(2:I(n-1), kstar, dstat = dstat)
  output <- output * lambda_vec

  return(sum(output))
}

#' @export
gwdegree <- function(x, alpha){
  n <- max(x)
  dstat <- tabulate(x + 1)

  output <- 0

  for(i in 1:I(n-1)){
    output <- output + (1 - (1 - exp(-alpha))^i) * dstat[i]
  }

  output <- output * exp(alpha)

  return(output)
}




