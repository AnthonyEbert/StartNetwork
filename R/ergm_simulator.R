
#' @export
ergm_simulator <- function(init, sum_stat, loops, theta, prob = 0.2, ...){

  stopifnot(class(sum_stat) == "function")
  stopifnot(loops > 0)

  x = init
  accepted = matrix(NA, nrow = loops, ncol = length(x))
  accepted_ss <- rep(NA, loops)

  for(i in 1:loops){
    s_init <- sum_stat(x, ...)
    x_star <- qERGM(x, prob)
    s_star <- sum_stat(x_star, ...)

    acceptance_prob <- number_of_graphs_cdd(x_star) - number_of_graphs_cdd(x) + s_star %*% theta - s_init %*% theta
    if(rbinom(1, size = 1, prob = min(1,exp(acceptance_prob))) == 1){
      x = x_star
      s_init = s_star
    }
    accepted[i,] = x
    accepted_ss[i] = s_init
  }

  output <- list(accepted = accepted, accepted_ss = accepted_ss)

  return(output)
}

#' @export
qERGM <- function(x, prob){
  n <- length(x)
  ind <- sample.int(n, 2)

  if(rbinom(1, 1, 0.5) == 1){
    x[ind[1]] = x[ind[1]] + 1
    x[ind[2]] = x[ind[2]] + 1
  } else {
    x[ind[1]] = x[ind[1]] - 1
    x[ind[2]] = x[ind[2]] - 1
  }

  if(rbinom(1, 1, prob) == 1){
    x = qERGM(x, prob)
  }

  return(sort(x))
}


