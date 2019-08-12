#' network summary statistics
#' @export
#' @import igraph
#'
netw_ss <- function(g){

  degree_dist <- degree_distribution(g)

  n_nodes <- gorder(g)
  n_edges <- edge_density(g)
  n_twostar <- degree_dist[2]
  n_threestar <- degree_dist[3]
  n_triangles <- length(triangles(g))/3 / n_nodes

  output <- c(n_edges, n_twostar, n_threestar, n_triangles)

  return(output)
}

#' @export
netw_ss_sim <- function(n, param, type = "gnp"){

  if(type == "gnp"){
    graph <- igraph::sample_gnp(n = n, p = param, directed = FALSE, loops = FALSE)
  }

  if(type == "pa"){
    graph <- igraph::sample_pa(n = n, power = param, directed = FALSE)
  }

  if(type == "sbm"){
    pref.matrix <- cbind( param, rev(param))
    graph <- sample_sbm(n = n, pref.matrix = pref.matrix, block.sizes = c(n/2, n/2))
  }

  output <- netw_ss(graph)

  return(output)
}
