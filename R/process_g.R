#' @export
process_g <- function(g_df, param_s){

  if(!is.null(param_s)){
    g_df <- g_df %>% dplyr::mutate(
      log_lik = sum_stat * param_s,
      KLdi1 = -entropy - logh1 - log_lik,
      KLdi2 = -entropy - logh2 - log_lik,
    )
  }

  g_tidy <- g_df %>%
    tidyr::gather( , , -parameter) %>%
    dplyr::mutate(
      type = dplyr::case_when(
        stringr::str_ends(key, "1") ~ "Bianconi",
        stringr::str_ends(key, "2") ~ "Liebenau",
        TRUE ~ "common"
      ),
      key = dplyr::case_when(
        stringr::str_ends(key, "1") ~ substr(key, 1, 4),
        stringr::str_ends(key, "2") ~ substr(key, 1, 4),
        TRUE ~ key
      )
    )

  return(g_tidy)
}
