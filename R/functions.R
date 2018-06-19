#' @export
randomStrings <- function(n = 1) {
  do.call(paste0, replicate(20, sample(c(
    letters, LETTERS, 0:9
  ), n, TRUE), FALSE))
}
