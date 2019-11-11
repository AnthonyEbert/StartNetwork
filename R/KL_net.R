
#' KL Divergence of mechanistic and statistical network models
#' @param theta_m numeric mechanistic model parameters
#' @param n integer number of nodes
#' @param theta_s numeric statistical model parameters
#' @param replicates numeric number of replicates
#' @param sorted logical whether to consider the sorted or unsorted degree sequence as the integral statistic
#' @param mech_net function the mechanistic network simulator
#' @param mech_args list arguments to the mechanistic network simulator
#' @param lstat function computes the likelihood statistic
#' @importFrom magrittr %>%
#' @examples
#'
#' theta_m = 5
#' theta_s = 1
#' n = 15
#'
#' mech_net = function(m, n, args){args$m = m; args$n = n; do.call(igraph::sample_pa, args)}
#' mech_args = list(directed = FALSE)
#' lstat = function(x){sum(igraph::degree(x) == 6)}
#'
#' KL_net(
#'   theta_m = theta_m,
#'   theta_s = theta_s,
#'   n = n,
#'   mech_net = mech_net,
#'   mech_args = mech_args,
#'   lstat = lstat
#' )
#'
#' @export
KL_net <- function(theta_m, n = 10, theta_s = 3, replicates = 1000, sorted = TRUE, mech_net, mech_args, lstat){

  stopifnot(is.numeric(n))
  stopifnot(n %% 1 == 0)
  stopifnot(is.numeric(replicates))
  stopifnot(replicates %% 1 == 0)
  stopifnot(replicates > 0)
  stopifnot(is.logical(sorted))

  ne <- choose(n,2)

  g <- lapply(rep(theta_m, replicates), func, mech_net = mech_net, lstat = lstat, n = n, args = mech_args)

  ds <- lapply(g, function(x){x$degree})
  if(sorted){
    ds <- lapply(ds, sort.int)
  }

  y1 <- aapply(g, function(x){x$stat}) %>% rowMeans()

  stopifnot(length(y1) == length(theta_s))
  y2 <- mean(sapply(ds, number_of_graphs_dd, sorted = sorted, type = "log"))

  entropy <- entropy_calc(ds, hash = TRUE)

  KL_div <- KL_calc2(n, theta_s, y1, y2, ne, entropy)

  output <- KL_div - entropy

  return(output)
}
