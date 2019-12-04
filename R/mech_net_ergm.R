
#' @export
sim_ergm_er <- function(p, n, nsim = 1, ...){
  ergg <- ergm::simulate_formula(network::network(n, directed = FALSE) ~ edges, coef = log(p/(1-p)), nsim = nsim, ...)
  return(ergg)
}

#' @export
ergm_to_igraph <- purrr::compose(purrr::partial(igraph::graph.adjacency, mode = "undirected"), network::as.matrix.network.adjacency)

#' @export
mech_net_ergm_n <- purrr::compose(ergm_to_igraph, sim_ergm_er)


stat_constructor <- function(x, dstat, lstat){list(degree = dstat(x), stat = lstat(x))}



#' @export
sim_ergm_triangles <- function(p, n, nsim = 1, ...){
  ergg <- ergm::simulate_formula(network::network(n, directed = FALSE) ~ triangles, coef = log(p/(1-p)), nsim = nsim, ...)
  return(ergg)
}

#' @export
mech_net_triangles_n <- purrr::compose(ergm_to_igraph, sim_ergm_triangles)
