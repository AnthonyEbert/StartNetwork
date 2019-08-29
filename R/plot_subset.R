
#' @export
plot_subset <- function(g, ...){
  g <- igraph::subgraph.edges(g, 1:ecount(g), delete.vertices = TRUE)
  plot(g, ...)
}
