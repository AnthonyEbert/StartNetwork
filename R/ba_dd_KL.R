#' @export
ba_dd_KL <- function(p, nl = 10, pl = 0.2, replicates = 1000, sorted = TRUE, ...){

  ne <- choose(nl,2)

  g <- igraph::graph(edges = c(1, 2, 1, 3), directed = FALSE)

  g <- lapply(rep(nl, replicates), igraph::sample_pa, power = p, directed = FALSE, start.graph = g, ...)
  y <- sapply(g, igraph::gsize)
  g <- lapply(g, igraph::degree)
  if(sorted){
    g <- lapply(g, sort)
  }

  y1 <- mean(sapply(g, max))
  y2 <- mean(log(sapply(g, number_of_graphs_dd, sorted = sorted)))

  entropy <- entropy_calc(g, hash = TRUE)

  KL_div <- KL_calc2(nl, pl, y1, y2, ne, entropy)

  if(is.nan(KL_div)){print(y)}

  output <- KL_div - entropy

  return(output)
}
