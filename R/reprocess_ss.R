#' @export
reprocess_ss <- function(x, param){
  output <- x
  output[3] <- attr(x, "lik_sum_stats") * param
  return(output)
}
