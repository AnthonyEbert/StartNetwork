
#' @export
tidy_g <- function(g){

  lik_sum_stats <- sapply(g, function(x){attr(x, "lik_sum_stats")})

  g_df <- do.call(dplyr::bind_rows, g) %>%
    mutate(
      KLdi1 = -entropy - logh1 - log_lik,
      KLdi2 = -entropy - logh2 - log_lik,
      sum_stat = lik_sum_stats,
      parameter = theta_p
    )

  g_tidy <- g_df %>%
    tidyr::gather( , , -parameter) %>%
    mutate(
      type = case_when(
        stringr::str_ends(key, "1") ~ "Bianconi",
        stringr::str_ends(key, "2") ~ "Liebenau",
        TRUE ~ "common"
      ),
      key = case_when(
        stringr::str_ends(key, "1") ~ substr(key, 1, 4),
        stringr::str_ends(key, "2") ~ substr(key, 1, 4),
        TRUE ~ key
      )
    )

  return(g_tidy)
}

#' @import ggplot2
#' @export
plot_g <- function(g_tidy){
  p <- ggplot(g_tidy) + aes(x = parameter, y = value, group = interaction(parameter, key), col = key) + geom_boxplot(outlier.shape = NULL) + facet_wrap(~type, scales = "free_y")
  return(p)
}


