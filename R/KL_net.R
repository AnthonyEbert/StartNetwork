
# net_ss <- function(theta_m, n = 10, theta_s = 3, replicates = 1000, sorted = TRUE, mech_net, mech_args, lstat, lapply_opt = TRUE, ds_return = FALSE, pmf = NULL, mirror = TRUE){

#' Generates list of degree sequences and summary statistics for a mechanistic model
#' @param theta_m numeric mechanistic model parameters
#' @param replicates numeric number of replicates
#' @param sorted logical whether to consider the sorted or unsorted degree sequence as the integral statistic
#' @param mech_net function the mechanistic network simulator
#' @param lstat function computes the likelihood statistic
#' @param lapply_opt boolean internal
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
net_ss <- function(theta_m, replicates = 1000, sorted = TRUE, mech_net, lstat, lapply_opt = TRUE, ergm = FALSE){

  stopifnot(is.numeric(replicates))
  stopifnot(replicates %% 1 == 0)
  stopifnot(replicates > 0)
  stopifnot(is.logical(sorted))

  if(!ergm){
    if(sorted){
      dstat <- function(x){sort.int(as.numeric(igraph::degree(x)))}
    } else {
      dstat <- function(x){as.numeric(igraph::degree(x))}
    }
  } else {
    if(sorted){
      dstat <- function(x){sort.int(as.numeric(igraph::degree(ergm_to_igraph(x))))}
    } else {
      dstat <- function(x){as.numeric(igraph::degree(ergm_to_igraph(x)))}
    }
  }

  stat_func <- purrr::partial(stat_constructor, dstat = !!dstat, lstat = !!lstat)

  mech_stat <- purrr::compose(stat_func, mech_net)

  if(lapply_opt){
    g <- lapply(rep(theta_m, replicates), mech_stat)
  } else if(!lapply_opt){
    g <- mech_net(theta_m, nsim = replicates) %>% purrr::map(mech_stat)
  }

  attr(g, "sorted") <- sorted

  return(g)
}

#' @export
process_ss <- function(g, theta_s, mirror, type = "Bianconi"){
  sorted = attr(g, "sorted")

  ds <- lapply(g, function(x){x$degree})

  lik_sum_stats <- aapply(g, function(x){x$stat}) %>% rowMeans()

  stopifnot(length(lik_sum_stats) == length(theta_s))
  logh <- rowMeans(aapply(ds, number_of_graphs_dd, sorted = sorted, mirror = mirror, type = type))

  entropy <- entropy_calc(ds, hash = TRUE)

  log_lik <- sum(theta_s * lik_sum_stats)

  output <- c(entropy = entropy, logh = logh, log_lik = log_lik)
  attr(output, "lik_sum_stats") <- lik_sum_stats

  return(output)
}

#' @export
KL_ss <- function(theta_m, theta_s, mirror = TRUE, type = "Bianconi", ...){
  net_ss(theta_m = theta_m, ...) %>%
    process_ss(theta_s = theta_s, mirror = mirror, type = type)
}

#' @export
KL_net <- function(theta_m, ...){
  -sum(KL_ss(theta_m = theta_m, ...))
}

