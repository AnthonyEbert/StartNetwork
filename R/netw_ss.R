#' network summary statistics
#' @export
#' @import igraph
#'
netw_ss <- function(g){

  degree_dist <- degree_distribution(g)

  n_nodes <- gorder(g)
  n_edges <- edge_density(g)
  n_twostar <- degree_dist[3]
  n_threestar <- degree_dist[4]
  n_triangles <- length(triangles(g))/3 / n_nodes
  poisson_est <- as.numeric(MASS::fitdistr(degree_dist * n_nodes, "poisson")$estimate)

  output <- c(edges = n_edges, twostar = n_twostar, threestar = n_threestar, triangles = n_triangles, poisson_est = poisson_est)

  return(output)
}

#' @export
netw_ss_sim <- function(param, n, type = "gnp"){

  if(type == "gnp"){
    graph <- igraph::sample_gnp(n = n, p = param, directed = FALSE, loops = FALSE)
  }

  if(type == "pa"){
    graph <- igraph::sample_pa(n = n, power = param, directed = FALSE)
  }

  if(type == "sbm"){
    pref.matrix <- cbind(c(param, 1e-3), c(1e-3, param))
    graph <- sample_sbm(n = n, pref.matrix = pref.matrix, block.sizes = c(n/2, n/2))
  }

  if(type == "smallworld"){
    graph <- sample_smallworld(dim = 1, size = n, nei = 5, p = param)
  }


  output <- netw_ss(graph)

  return(output)
}
