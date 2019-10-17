#' @export
er_dd_KL <- function(p, nl = 10, pl = 0.1, replicates = 1000, sorted = TRUE, ...){

  ne <- choose(nl,2)

  g <- lapply(rep(nl, replicates), igraph::sample_gnp, p = p, directed = FALSE, loops = FALSE, ...)
  y <- sapply(g, igraph::gsize)
  g <- lapply(g, igraph::degree)
  if(sorted){
    g <- lapply(g, sort)
  }

  y1 <- mean(y)
  y2 <- mean(log(sapply(g, number_of_graphs_dd, sorted = sorted)))

  entropy <- entropy_calc(g, hash = TRUE)

  KL_div <- KL_calc(nl, pl, y1, y2, ne, entropy)

  if(is.nan(KL_div)){print(y)}

  output <- KL_div - entropy

  return(output)
}
