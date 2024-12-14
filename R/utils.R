#' Provide normalized names and make sure the dir exists
#'
#' @description This provides a temporary directory where the executables can read and write temporaty files. Its path is returned in normalized format \bold{with} system-dependent terminal separator.
#' @param temp_dir NULL, or a path to be used; if NULL, R's [base::tempdir()] is used.
#' @param sep Better ignored; non-default values are passed to [base::normalizePath()].
#' @param sub Extension for defining a sub-directory within the directory defined by [base::tempdir].
#'
#' @return Normalized path with system-dependent terminal separator.
#' @export

tp_tempdir <- function(temp_dir = NULL,
                       sep = NULL,
                       sub = NULL) {
  if (is.null(sep)) {
    sep <- sep()
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
}

#' Provide a separator character platform dependent
sep <- function() {
  if (.Platform$OS.type == "windows")
    "\\"
  else
    "/"
}

#' Find model and data describe in model_name
find_file <- function(model_name, exten) {
  if (exten == "tppl") {
    return(readr::read_file(paste0(
      system.file("extdata", package = "treepplr"),
      sep(),
      model_name,
      ".",
      exten
    )))
  } else if (exten == "json") {
    return(jsonlite::fromJSON(paste0(
      system.file("extdata", package = "treepplr"),
      sep(),
      model_name,
      ".",
      exten
    )))
  }
}

#' Provide normalized model names supported by treepplr
#'
#' @description Provide a list of all the model names supported by treepplr.
#' They can also be use to find data attach to these models (see [treepplr::tp_data()])
#'
#' @return A list of model names.
#' @export
tp_model_name <- function() {
  list(
    custom = "custom",
    coin = "coin",
    hostrep3states = "hostrep3states",
    hostrep2states = "hostrep2states",
    treeinference = "treeinference",
    crbd = "crbd",
    clads = "clads"
  )
}
