
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
  if(class(y) == "igraph"){
    return(list(degree = igraph::degree(y), stat = lstat(y)))
  } else if(class(y) == "network"){
    adj <- network::as.matrix.network.adjacency(y) %>% graph.adjacency()
    return(list(degree = igraph::degree(adj) %>% as.numeric / 2, stat = lstat(adj)))
  }
}
