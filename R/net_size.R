#' Size of graph space for n nodes.
#' @export
net_size <- function(n){
  possible_edges <- choose(n,2)
  output <- sum(choose(possible_edges,1:possible_edges))
  return(output)
}
