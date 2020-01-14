
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


#' @export
sim_ergm_triangles <- function(p, n, nsim = 1, ...){
  ergg <- ergm::simulate_formula(network::network(n, directed = FALSE) ~ triangles, coef = log(p/(1-p)), nsim = nsim, ...)
  return(ergg)
}

#' @export
mech_net_triangles_n <- purrr::compose(ergm_to_igraph, sim_ergm_triangles)


#' @export
sim_ergm_altkstar <- function(p, n, nsim = 1, ...){
  ergg <- ergm::simulate_formula(network::network(n, directed = FALSE) ~ altkstar, coef = c(1, p), nsim = nsim, ...)
  return(ergg)
}

#' @export
mech_net_altkstar_n <- purrr::compose(ergm_to_igraph, sim_ergm_altkstar)

#' @export
sim_ergm_gwdegree <- function(p, n, nsim = 1, ...){
  ergg <- ergm::simulate_formula(network::network(n, directed = FALSE) ~ gwdegree(10, fixed = TRUE), coef = c(p), nsim = nsim, ...)
  return(ergg)
}

#' @export
mech_net_gwdegree_n <- purrr::compose(ergm_to_igraph, sim_ergm_gwdegree)

#' @export
sim_ergm_altkstar <- function(p, n, nsim = 1, ...){
  ergg <- ergm::simulate_formula(network::network(n, directed = FALSE) ~ altkstar(log(10), fixed = TRUE), coef = c(p), nsim = nsim, ...)
  return(ergg)
}

