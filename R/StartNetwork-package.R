## usethis namespace: start
#' @useDynLib StartNetwork, .registration = TRUE
## usethis namespace: end
NULL

.onUnload <- function (libpath) {
  library.dynam.unload("StartNetwork", libpath)
}
