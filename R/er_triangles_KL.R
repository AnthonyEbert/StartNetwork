#' @export
er_triangles_KL <- function(p, nl = 10, pl = 0.1, replicates = 1000, sorted = TRUE, ...){

  ne <- choose(nl,2)

  input_fun <- function(nl, p){
    g <- simulate(network::network(nl, directed = FALSE) ~ triangles, coef = p)
    output <- igraph::graph_from_adjacency_matrix(network::as.matrix.network.adjacency(g), mode = "undirected")
    return(output)
  }

  g <- lapply(rep(nl, replicates), input_fun, p = p)
  y <- sapply(g, igraph::gsize)
  g <- lapply(g, function(x){length(igraph::triangles(x))/3})
  if(sorted){
    g <- lapply(g, sort)
  }

  y1 <- mean(y)
  y2 <- mean(log(sapply(g, number_triangle_graphs, n = nl)))

  entropy <- entropy_calc(g, hash = TRUE)

  KL_div <- KL_calc(nl, pl, y1, y2, ne, entropy)

  if(is.nan(KL_div)){print(y)}

  output <- KL_div - entropy

  return(output)
}
