
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
KL_net <- function(theta_m, n = 10, theta_s = 3, replicates = 1000, sorted = TRUE, mech_net, mech_args, lstat, lapply_opt = TRUE, ds_return = FALSE, pmf = NULL, type = "log"){

  stopifnot(is.numeric(n))
  stopifnot(n %% 1 == 0)
  stopifnot(is.numeric(replicates))
  stopifnot(replicates %% 1 == 0)
  stopifnot(replicates > 0)
  stopifnot(is.logical(sorted))

  ne <- choose(n,2)

  if(lapply_opt){
    g <- lapply(rep(theta_m, replicates), func, mech_net = mech_net, lstat = lstat, n = n, args = mech_args)
  } else if(!lapply_opt){
    g <- mech_net(theta_m, n, mech_args, nsim = replicates)
    g <- purrr::map(g, purrr::compose(function(x){list(degree = as.numeric(igraph::degree(x)) / 2, stat = lstat(x))}, igraph::graph_from_adjacency_matrix, network::as.matrix.network.adjacency), mode = "undirected")
  }


  ds <- lapply(g, function(x){x$degree})
  if(sorted){
    ds <- lapply(ds, sort.int)
  }

  if(ds_return){return(ds)}

  y1 <- aapply(g, function(x){x$stat}) %>% rowMeans()

  stopifnot(length(y1) == length(theta_s))
  y2 <- mean(sapply(ds, number_of_graphs_dd, sorted = sorted, type = type))

  entropy <- entropy_calc(ds, hash = TRUE)

  if(is.null(pmf)){
    output <- KL_calc2(theta_s, y1, y2, entropy)
  } else {
    y_hash <- sapply(ds, digest::digest, algo = "md5")
    y_table <- table(y_hash)
    y_df <- as.data.frame(y_table)
    names(y_df) <- c("hash", "Freq_y")
    out_df <- acetools::left_join_quietly(y_df, pmf, by = "hash")
    out_df$Freq_x[which(is.na(out_df$Freq_x))] <- 1/dim(pmf)[1] / 100
    likely <- sum(log(apply(out_df[,c(2,3)], 1, prod)), na.rm = TRUE)
    output <- -likely - y2 - entropy
  }

  return(output)
}
