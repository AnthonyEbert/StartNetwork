
#' @export
tidy_g <- function(g, tidy = TRUE){

  lik_sum_stats <- sapply(g, function(x){attr(x, "lik_sum_stats")})

  g_df <- do.call(dplyr::bind_rows, g) %>%
    dplyr::mutate(
      KLdi1 = -entropy - logh1 - log_lik,
      KLdi2 = -entropy - logh2 - log_lik,
      sum_stat = lik_sum_stats,
      parameter = theta_p
    )

  if(!tidy){return(g_df)}

  g_tidy <- process_g(g_df, NULL)

  return(g_tidy)
}

#' @import ggplot2
#' @export
plot_g <- function(g_tidy){
  p <- ggplot(g_tidy) + aes(x = parameter, y = value, group = interaction(parameter, key), col = key) + geom_boxplot(outlier.shape = NULL) + facet_wrap(~type, scales = "free_y")
  return(p)
}


