#' @export
triangle_dist_degseq <- function(ds, tri_stat, replicates = 100){
  triangle_dist <- replicate(replicates, length(igraph::triangles(igraph::sample_degseq(ds, method = "simple.no.multiple")))/3)
  out <- length(which(triangle_dist == tri_stat)) / replicates
  return(out)
}
