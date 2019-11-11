
#' @export
aapply <- function(X, FUN, ...){
  x <- sapply(X, FUN, ...)
  if(class(x) != "matrix"){
    x <- t(x)
  }
  return(x)
}


func <- function(x, mech_net, lstat, ...){
  y <- mech_net(x, ...)
  return(list(degree = igraph::degree(y), stat = lstat(y)))
}
