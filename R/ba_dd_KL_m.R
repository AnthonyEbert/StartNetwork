
#' KL Divergence of mechanistic and statistical network models
#' @importFrom magrittr %>%
#' @export
ba_dd_KL_m <- function(p, n = 10, theta_s = 3, replicates = 1000, sorted = TRUE, lstat = function(x){sum(igraph::degree(x) == 6)} , ...){

  n <- choose(nl,2)

  g <- igraph::graph(edges = c(1, 2, 1, 3), directed = FALSE)

  g <- lapply(rep(nl, replicates), igraph::sample_pa, m = p, directed = FALSE, start.graph = g, ...)
  y <- sapply(g, igraph::gsize)
  ds <- lapply(g, igraph::degree)
  if(sorted){
    ds <- lapply(ds, sort)
  }

  y1 <- aapply(g, lstat) %>% rowMeans()

  stopifnot(length(y1) == length(pl))
  y2 <- mean(sapply(ds, number_of_graphs_dd, sorted = sorted, type = "log"))

  entropy <- entropy_calc(ds, hash = TRUE)

  KL_div <- KL_calc2(nl, pl, y1, y2, ne, entropy)

  output <- KL_div - entropy

  return(output)
}
