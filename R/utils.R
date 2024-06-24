#' Provide normalized names and make sure the dir exists
#'
#' @description This provides a temporary directory where the executables can read and write temporaty files. Its path is returned in normalized format \bold{with} system-dependent terminal separator.
#' @param temp_dir NULL, or a path to be used; if NULL, R's \code{tempdir()} is used.
#' @param sep Better ignored; non-default values are passed to \code{normalizePath()}.
#' @param sub Extension for defining a sub-directory within the directory defined by \code{temp_dir}
#'
#' @return Normalized path with system-dependent terminal separator.
#' @export
#'
#' @examples
#' treeppl_tempdir(NULL)

treeppl_tempdir <- function(temp_dir = NULL, sep = NULL, sub = NULL) {
  if (is.null(sep)) {
    sep <- .sep()
  if (is.null(temp_dir))
    temp_dir <- tempdir()
  temp_dir <- normalizePath(temp_dir, sep)
  if (substring(temp_dir, nchar(temp_dir)) != sep) {
    temp_dir <- paste0(temp_dir, sep)
  }
  if (!is.null(sub))
    temp_dir <- paste0(temp_dir, sub, sep)
  if (!dir.exists(temp_dir))
    dir.create(temp_dir)
  temp_dir
}

.sep <- function() {
  if (.Platform$OS.type == "windows")
    "\\"
  else
    "/"
}
}
