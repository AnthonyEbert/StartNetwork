
#' @export
ergm_simulator <- function(init, sum_stat, loops, theta, type = "Liebenau", ...){

  stopifnot(class(sum_stat) == "function")
  stopifnot(loops > 0)

  x = init

  accepted = matrix(NA, nrow = loops, ncol = length(x))
  accepted_ss <- rep(NA, loops)

  for(i in 1:loops){
    s_init <- sum_stat(x, ...)
    x_star <- qERGM(x)
    s_star <- sum_stat(x_star, ...)

    acceptance_prob <- number_of_graphs_dd(x_star, type = type) - number_of_graphs_dd(x, type = type) + s_star %*% theta - s_init %*% theta
    if(rbinom(1, size = 1, prob = min(1,exp(acceptance_prob))) == 1){
      x = x_star
      s_init = s_star
    }
    accepted[i,] = x
    accepted_ss[i] = s_init
  }

  return(accepted_ss)
}

#' @export
qERGM <- function(x){
  n <- length(x)
  ind <- sample.int(n, 2)
  alt01 <- sample.int(3, 1, prob = c(1/3, 1/3, 1/3))

  if(alt01 == 1){
    x[ind[1]] = x[ind[1]] + 1
    x[ind[2]] = x[ind[2]] + 1
  } else if(alt01 == 2){
    x[ind[1]] = x[ind[1]] - 1
    x[ind[2]] = x[ind[2]] - 1
  } else {
    x[ind[1]] = x[ind[1]] - 1
    x[ind[2]] = x[ind[2]] + 1
  }

  if(rbinom(1, 1, 0.4) == 1){
    x = qERGM(x)
  }

  return(sort(x))
}


