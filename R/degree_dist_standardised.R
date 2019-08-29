
#' @export
degree_dist_standardised <- function(g, maxk = 1000){
  output <- rep(0, maxk)
  x <- igraph::degree_distribution(g)
  output[1:length(x)] <- x
  return(output)
}
