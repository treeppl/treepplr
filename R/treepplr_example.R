#' TreePPL example files
#'
#' @param path Optional. Name of example file. If NULL, returns a list of all example files.
#'  in the `extdata` directory.
#'
#' @return A list of files or a path to one example file.
#' @export
#'
#' @examples
#' tp_example()
#' tp_example("coin.json")
tp_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "treepplr"))
  } else {
    system.file("extdata", path, package = "treepplr", mustWork = TRUE)
  }
}
