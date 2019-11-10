
#' @export
aapply <- function(X, FUN, ...){
  x <- sapply(X, FUN, ...)
  if(class(x) != "matrix"){
    x <- t(x)
  }
  return(x)
}
