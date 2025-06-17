#' Temporary directory for running treeppl
#'
#' @description
#' `tp_tempdir` returns a normalized path for a temporary directory where the
#' executables can read and write temporary files.
#'
#' @param temp_dir NULL, or a path to be used; if NULL, R's [base::tempdir()]
#' is used.
#' @param sep Better ignored; non-default values are passed to
#' [base::normalizePath()].
#' @param sub Extension for defining a sub-directory within the directory
#' defined by [base::tempdir].
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

# Platform-dependent separator character
sep <- function() {
  if (.Platform$OS.type == "windows")
    "\\"
  else
    "/"
}

# Find model and data files for model_name
find_file <- function(model_name, exten) {
  if (exten == "tppl") {
    readr::read_file(paste0(
      system.file("extdata", package = "treepplr"),
      sep(),
      model_name,
      ".",
      exten
    ))
  } else if (exten == "json") {
    jsonlite::fromJSON(paste0(
      system.file("extdata", package = "treepplr"),
      sep(),
      model_name,
      ".",
      exten
    ))
  }
}

#' Model file names stored by user in [base::tempdir()]) using
#' [treepplr::tp_write()])
#'
#' @description Provides a list of all the model file names currently
#' stored in [base::tempdir()]). To verify if there is a
#' matching data file, see [treepplr::tp_data_stored()].
#'
#' @return A list of model file names.
#' @export

tp_stored_model <- function() {
  stored_files("tppl")
}

#' Data file names stored by user in [base::tempdir()]) using
#' [treepplr::tp_write()])
#'
#' @description Provides a list of all the data file names currently
#' stored in [base::tempdir()]). To verify if there is a
#' matching model file, see [treepplr::tp_model_stored()].
#'
#' @return A list of data file names.
#' @export

tp_stored_data <- function() {
  res <- stored_files("json")
  list_na <- stringr::str_extract(res, "^((?!_out).)*$")
  list <- list_na[!is.na(list_na)]
  list
}

#' List of compiled models in [base::tempdir()])
#'
#' @description Provides a list of all the compiled model file names currently
#' stored in [base::tempdir()]).
#'
#' @return A list of compiled model file names.
#' @export

tp_stored_compiled <- function() {
  stored_files("exe")
}

#Provides file names already provided by user, stored in [base::tempdir()])
stored_files <- function(exten) {
  tmp <- list.files(tp_tempdir())
  list_na <-
    stringr::str_extract(tmp, paste0(".*(?=\\.", exten, ")"))
  list <- list_na[!is.na(list_na)]
  list
}

#' Model names supported by treepplr
#'
#' @description Provides a list of all model names supported by treepplr.
#' The names can also be used to find data for these models
#' (see [treepplr::tp_data()]).
#'
#' @return A list of model names.
#' @export
tp_model_names <- function() {
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
