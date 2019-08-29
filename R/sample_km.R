
#' @export
sample_km <- function(g, pf = 0.01, ps = 0.005, dm = 10){

  n <- gorder(g)
  sexes <- vertex_attr(g, "sexes")
  males <- which(sexes == "male")
  females <- which(sexes == "female")

  T1 <- n/2 - ecount(g)

  for(t in 1:T1){
    if(rbinom(1, size = 1, prob = pf) == 1){
      condition <- TRUE
      while(condition){
        male <- sample(males, 1)
        female <- sample(females, 1)
        condition <- !(!are.connected(g, male, female) &
          all(degree(g)[c(male, female)] < dm))
      }
      g <-add_edges(g, c(male, female))
    }
  }
    number_edges_to_remove <- rbinom(1, ecount(g), prob = ps)
    g <- delete_edges(g, sample.int(n = ecount(g), number_edges_to_remove))

  return(g)
}
