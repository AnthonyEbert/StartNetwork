
#' @export
er_graphs_KL <- function(p, nl = 10, pl = 0.1, replicates = 1000, include_entropy = TRUE, entropy_cheat = FALSE, both = FALSE, ...){

  ne <- choose(nl,2)

  g <- lapply(rep(nl, replicates), igraph::sample_gnp, p = p, directed = FALSE, loops = FALSE, ...)
  y <- sapply(g, igraph::gsize)
  g <- lapply(g, igraph::as_edgelist)

  y1 <- mean(y)
  y2 <- 0

  entropy <- entropy_calc(g, hash = TRUE)

  KL_div <- KL_calc(nl, pl, y1, y2, ne, entropy)

  if(is.nan(KL_div)){print(y)}

  if(!include_entropy){
    output <- KL_div
  } else {
    output <- KL_div - entropy
  }

  if(both){
    output <- c(KL_div, entropy)
  }

  return(output)
}
