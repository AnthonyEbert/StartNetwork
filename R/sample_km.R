
#' @export
sample_km <- function(g, pf = 0.5, ps = 0.5, T1 = 2){

  n <- gorder(g)

  if("phi" %in% vertex_attr_names(g)){
    phi <- vertex_attr(g, "phi")
  } else {
    phi <- rep(1, n)
  }


  for(t in 1:T1){
    if(rbinom(1, size = 1, prob = pf) == 1){
      condition <- TRUE
      while(condition){
        nodes <- sample.int(n, 2)
        condition <- are.connected(g, nodes[1], nodes[2])
      }
      if(rbinom(1, 1, prob = phi[nodes[1]]) == 1){
        g <-add_edges(g, nodes)
      }
    }
    number_edges_to_remove <- rbinom(1, ecount(g), prob = ps)
    g <- delete_edges(g, sample.int(n = ecount(g), number_edges_to_remove))
  }

  return(g)
}
